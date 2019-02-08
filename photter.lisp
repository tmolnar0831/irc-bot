;;;; Name:    Photter IRC BOT
;;;; Author:  Tamas Molnar - tmolnar0831@gmail.com
;;;; License: MIT

(defpackage :photter
  (:use "COMMON-LISP"
        "WEATHER-CHECKER"
        "URI-ECHO")
  (:export "MAIN"))

(in-package :photter)

(defparameter *version* "2.1.1")
(defvar *nick* "photter")
(defvar *server* "irc.freenode.net")
(defvar *channel* nil)
(defvar *connection*)
(defparameter version-text
  (format nil "Version: ~A" *version*))
(defparameter help-text
    "Available commands: .weather <city> [<country> | For US/CA: <state/province>], .setlocation <city> [<country> | For US/CA: <state/province>], .getlocation, .remlocation, .version")

(defun answer (answer source destination)
  "General function for sending messages"
  (let ((text (cl-ppcre:regex-replace-all "(\\n)" answer " ")))
    (if (string-equal destination *nick*)
        (irc:privmsg *connection* source text)
        (irc:privmsg *connection* destination text))))

(defun process-message-params (message)
  (split-sequence:split-sequence #\Space (first message)))

(defun msg-hook (message)
  (let ((msg-src (irc:source message))              ;source nick of the message
        (msg-dst (first (irc:arguments message)))   ;destination nick or channel
        (arguments (last (irc:arguments message)))) ;the rest of the message
    (handler-case
        (cond
          ((string-equal (car (process-message-params arguments)) ".weather")
           (answer (display-weather-info msg-src (cdr (process-message-params arguments))) msg-src msg-dst))
          ((string-equal (car (process-message-params arguments)) ".setlocation")
           (if (cdr (process-message-params arguments))
               (progn
                 (add-location msg-src (cdr (process-message-params arguments)))
                 (answer (princ-to-string (get-location msg-src)) msg-src msg-dst))
               (answer ".setlocation <city, country|province|state>" msg-src msg-dst)))
          ((string-equal (car (process-message-params arguments)) ".getlocation")
           (answer (princ-to-string (get-location msg-src)) msg-src msg-dst))
          ((string-equal (car (process-message-params arguments)) ".remlocation")
           (rem-location msg-src))
          ((string-equal (car (process-message-params arguments)) ".help")
           (answer help-text msg-src msg-src))
          ((string-equal (car (process-message-params arguments)) ".version")
           (answer version-text msg-src msg-dst))
          (t (let ((response (process-message-for-uri-echo (process-message-params arguments))))
               (if (nth 0 response) (answer (nth 0 response) msg-src msg-dst)))))
      (error (err)
        (answer (format nil "Sorry, I got an error: ~A" err) msg-src msg-dst)))))

(defun main (&key ((:nick *nick*) *nick*)
               ((:channel *channel*) *channel*)
               password)
  (setf *connection* (irc:connect :nickname *nick* :server *server*))
  (unwind-protect (progn
                    (if password
                        ;; Freenode specific authentication process
                        ;; https://tools.ietf.org/html/rfc2812#section-3.1.1 does not work
                        (let ((authstring (format nil "identify ~A" password)))
                          (irc:privmsg *connection* "NickServ" authstring)))
                    (if (listp *channel*)
                        (mapcar #'(lambda (ch) (irc:join *connection* ch)) *channel*)
                        (irc:join *connection* *channel*))
                    (if (probe-file  *location-file*)
                        (load-weather-db *location-file*))
                    (irc:add-hook *connection* 'irc:irc-privmsg-message 'msg-hook)
                    (irc:read-message-loop *connection*))
    (progn
      (save-weather-db *location-file*)
      (irc:quit *connection* "Good Bye!"))))
