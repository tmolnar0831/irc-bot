;;;; Name:    Photter IRC BOT
;;;; Author:  Tamas Molnar - tmolnar0831@gmail.com
;;;; License: MIT

(defpackage :photter
  (:use "COMMON-LISP"
        "WEATHER-CHECKER"
        "URI-ECHO"
        "API-KEY")
  (:export "MAIN"))

(in-package :photter)

(defparameter *version* "0.2.2")
(defvar *nick* "photter")
(defvar *server* "irc.freenode.net")
(defvar *channel* "#iron-bottest-room")
(defvar *connection*)
(defparameter version-text
  (format nil "Version: ~A" *version*))
(defparameter help-text
    "Available commands: .weather <city> [<ISO 3166 two-digit country code> | For US/CA: <full state/province name>], .setlocation <city> [<ISO 3166 two-digit country code> | For US/CA: <full state/province name>], .getlocation, .remlocation")

(defun say-to-channel (say)
  (irc:privmsg *connection* *channel* (princ-to-string say)))

(defun say-to-private (answer destination)
  (irc:privmsg *connection* destination (princ-to-string answer)))

(defun process-message-params (message)
  (split-sequence:split-sequence #\Space (first message)))

(defun msg-hook (message)
  (let ((msg-src (irc:source message))              ;who sent the message
        (msg-dst (first (irc:arguments message)))   ;where the message arrived to
        (arguments (last (irc:arguments message)))) ;the rest of the message
    (handler-case
        (cond
          ((string-equal (car (process-message-params arguments)) ".weather")
           (let* ((location (get-location msg-src))
                 (response (current-weather-information (or (cdr (process-message-params arguments)) location))))
             (if (string-equal msg-dst *nick*)
                 (say-to-private response msg-src)
                 (say-to-channel response))))
          ((string-equal (car (process-message-params arguments)) ".setlocation")
           (add-location msg-src (cdr (process-message-params arguments))))
          ((string-equal (car (process-message-params arguments)) ".getlocation")
           (say-to-channel (get-location msg-src)))
          ((string-equal (car (process-message-params arguments)) ".remlocation")
           (rem-location msg-src))
          ((string-equal (car (process-message-params arguments)) ".help")
           (say-to-private help-text msg-src))
          ((string-equal (car (process-message-params arguments)) ".version")
           (say-to-channel version-text))
          (t (let ((response (process-message-for-uri-echo (process-message-params arguments))))
               (if response (say-to-channel response)))))
      (error (err)
        (say-to-channel (format nil "Sorry, I got an error: ~A" err))))))

(defun main (&key ((:nick *nick*) *nick*) ((:channel *channel*) *channel*))
  (setf *api-key* (load-api-key "openweathermap"))
  (setf *connection* (irc:connect :nickname *nick* :server *server*))
  (unwind-protect (progn
                    (irc:join *connection* *channel*)
                    (if (probe-file  *location-file*)
                        (load-weather-db *location-file*))
                    (irc:add-hook *connection* 'irc:irc-privmsg-message 'msg-hook)
                    (irc:read-message-loop *connection*))
    (progn
      (save-weather-db *location-file*)
      (irc:quit *connection*))))
