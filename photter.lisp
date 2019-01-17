;;;; Name:    Photter IRC BOT
;;;; Author:  Tamas Molnar - tmolnar0831@gmail.com
;;;; License: MIT

(defpackage :photter
  (:use "COMMON-LISP"
        "WEATHER-CHECKER"
        "API-KEY"
        "WEB-ECHO")
  (:export "MAIN"))
(in-package :photter)

(defparameter *version* "0.1.0")
(defvar *nick* "photter")
(defvar *server* "irc.freenode.net")
(defvar *channel* "#iron-bottest-room")
(defvar *connection*)

(defparameter about-text
  (format nil "I am a Common Lisp IRC BOT maintained by st_iron. Use .help for command info."))

(defparameter help-text
    "Available commands: .weather <city> [<ISO 3166 two-digit country code> | <full state/province name>]")

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
          ;; In case of .weather command
          ((string-equal (car (process-message-params arguments)) ".weather")
           ;; If the message is sent to the bot
           (if (string-equal msg-dst *nick*)
               ;; Send the answer in private message
               (say-to-private (current-weather-information (cdr (process-message-params arguments))) msg-src)
               ;; Else message to the channel
               (say-to-channel (current-weather-information (cdr (process-message-params arguments))))))
          ;; Show the help text with .help
          ((string-equal (car (process-message-params arguments)) ".help")
           (say-to-channel help-text))
          ;; Show the about text with .about
          ((string-equal (car (process-message-params arguments)) ".about")
           (say-to-channel about-text)))
      (error (err)
        (say-to-channel (format nil "Sorry, I got an error: ~A" err))))))

(defun main (&key ((:nick *nick*) *nick*) ((:channel *channel*) *channel*))
  (setf *api-key* (load-api-key "openweathermap"))
  (setf *connection* (irc:connect :nickname *nick* :server *server*))
  (unwind-protect (progn
                    (irc:join *connection* *channel*)
                    (irc:add-hook *connection* 'irc:irc-privmsg-message 'msg-hook)
                    (irc:read-message-loop *connection*))
    (irc:quit *connection*)))

