;;;; Name:    Photter IRC bot
;;;; Author:  Tamas Molnar - tmolnar0831@gmail.com
;;;; License: MIT

(defpackage :photter
  (:use "COMMON-LISP"
        "WEATHER-CHECKER"
        "API-KEY")
  (:export "MAIN"))
(in-package :photter)

(defparameter *version* "0.0.1")
(defvar *nick* "photter")
(defvar *server* "irc.freenode.net")
(defvar *channel* "#iron-bottest-room")
(defvar *connection*)

(defparameter about-text
  (format nil "I am a Common Lisp IRC BOT maintained by st_iron. Use .help for command info."))

(defparameter help-text
    "Available commands: .weather <city> [<ISO 3166 two-digit country code>]")

(defun say-to-channel (say)
  (irc:privmsg *connection* *channel* (princ-to-string say)))

(defun send-answer (answer destination)
  (irc:privmsg *connection* destination (princ-to-string answer)))

(defun process-message-params (message)
  (split-sequence:split-sequence #\Space (first message)))

(defun issued-command (message)
  (first message))

(defun argument-vector (message)
  (rest message))

(defun msg-hook (message)
  (let ((msg-src (irc:source message))
        (msg-dst (first (irc:arguments message)))
        (arguments (last (irc:arguments message))))
    (handler-case
        (cond ((string-equal (issued-command (process-message-params arguments)) ".weather")
               (say-to-channel (format-answer-string (get-processed-output (first (argument-vector (process-message-params arguments)))))))
              ((string-equal (issued-command (process-message-params arguments)) ".help")
               (say-to-channel help-text))
              ((string-equal (issued-command (process-message-params arguments)) ".about")
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

