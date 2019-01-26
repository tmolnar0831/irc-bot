;;;; Name:    Photter IRC BOT
;;;; Author:  Tamas Molnar - tmolnar0831@gmail.com
;;;; License: MIT

(defpackage :photter
  (:use "COMMON-LISP"
        "WEATHER-CHECKER"
        "URI-ECHO")
  (:export "MAIN"))

(in-package :photter)

(defparameter *version* "2.0.0")
(defvar *nick* "photter-dev")
(defvar *server* "irc.freenode.net")
(defvar *channel* "#iron-bottest-room")
(defvar *connection*)
(defparameter version-text
  (format nil "Version: ~A" *version*))
(defparameter help-text
    "Available commands: .weather <city> [<ISO 3166 two-digit country code> | For US/CA: <full state/province name>], .setlocation <city> [<ISO 3166 two-digit country code> | For US/CA: <full state/province name>], .getlocation, .remlocation")

(defun answer (answer source destination)
  "General function for sending messages"
  (if (string-equal destination *nick*) 
      (irc:privmsg *connection* source (princ-to-string answer))
      (irc:privmsg *connection* destination (princ-to-string answer))))

(defun process-message-params (message)
  (split-sequence:split-sequence #\Space (first message)))

(defun msg-hook (message)
  (let ((msg-src (irc:source message))              ;source nick of the message
        (msg-dst (first (irc:arguments message)))   ;destination nick or channel
        (arguments (last (irc:arguments message)))) ;the rest of the message
    (handler-case
        (cond
          ((string-equal (car (process-message-params arguments)) ".weather")
           (let* ((location (get-location msg-src))
                  (weather-arguments (or (cdr (process-message-params arguments)) location))
                  (response (current-weather-information (princ-to-string weather-arguments))))
             (answer response msg-src msg-dst)))
          ((string-equal (car (process-message-params arguments)) ".setlocation")
           (add-location msg-src (cdr (process-message-params arguments)))
           (answer "Location updated..." msg-src msg-dst))
          ((string-equal (car (process-message-params arguments)) ".getlocation")
           (answer (get-location msg-src) msg-src msg-dst))
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

(defun main (&key ((:nick *nick*) *nick*) ((:channel *channel*) *channel*))
  (setf *connection* (irc:connect :nickname *nick* :server *server*))
  (unwind-protect (progn
                    (if (listp *channel*)
                        (mapcar #'(lambda (ch) (irc:join *connection* ch)) *channel*)
                        (irc:join *connection* *channel*))
                    (if (probe-file  *location-file*)
                        (load-weather-db *location-file*))
                    (irc:add-hook *connection* 'irc:irc-privmsg-message 'msg-hook)
                    (irc:read-message-loop *connection*))
    (progn
      (save-weather-db *location-file*)
      (irc:quit *connection* "Mr. Photter says Good Bye!"))))
