;;;; Name:    Photter IRC bot
;;;; Author:  Tamas Molnar - tmolnar0831@gmail.com
;;;; License: MIT


(defparameter *version* "0.0.1")
(defvar *nick* "photter")
(defvar *server* "irc.freenode.net")
(defvar *channel* "#iron-bottest-room")
(defvar *connection*)



(defparameter help-text
    "Available commands: .weather <city>,[<ISO 3166 country code>]")

(defun say-to-channel (say)
  (irc:privmsg *connection* *channel* (format nil "~S" say)))

(defun process-message-params (message)
  (split-sequence:split-sequence #\Space (first message)))

(defun issued-command (message)
  (first message))

(defun argument-vector (message)
  (rest message))

(defun msg-hook (message)
  (let ((arguments (last (irc:arguments message))))
    (cond ((string-equal (issued-command (process-message-params arguments)) ".weather")
           (get-processed-output (first (argument-vector (process-message-params arguments))))
           (say-to-channel (return-answer *processed*)))
          ((string-equal (issued-command (process-message-params arguments)) ".help")
           (say-to-channel help-text)))))

(defun main ()
  (setf *api-key* (load-api-key "openweathermap"))
  (setf *connection* (irc:connect :nickname *nick* :server *server*))
  (irc:join *connection* *channel*)
  (irc:add-hook *connection* 'irc:irc-privmsg-message 'msg-hook)
  (irc:read-message-loop *connection*))

