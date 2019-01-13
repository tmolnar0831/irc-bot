;;;; Name:    Photter IRC bot
;;;; Author:  Tamas Molnar - tmolnar0831@gmail.com
;;;; License: MIT

(require :cl-irc)
(require :split-sequence)

(defparameter *version* "0.0.1")
(defvar *nick* "st_iron_test_bot")
(defvar *server* "irc.freenode.net")
(defvar *channel* "#iron-bottest-room")
(defvar *connection* (irc:connect :nickname *nick*
                                  :server *server*))

(irc:join *connection* *channel*)

(defparameter help-text
    "Available commands: ,bot")

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
    (if (string-equal (issued-command (process-message-params arguments)) ",help")
        (say-to-channel help-text))
    (if (string-equal (issued-command (process-message-params arguments)) ",bot")
        (say-to-channel (argument-vector (process-message-params arguments))))))

(irc:add-hook *connection* 'irc:irc-privmsg-message 'msg-hook)

(irc:read-message-loop *connection*)
