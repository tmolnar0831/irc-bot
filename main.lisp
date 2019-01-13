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

(defun say-to-channel (say)
  (irc:privmsg *connection* *channel* (format nil "~S" say)))

(defun process-message-params (message)
  (split-sequence:split-sequence #\Space (first message)))

(defun msg-hook (message)
  (let ((arguments (last (irc:arguments message))))
    (say-to-channel (process-message-params arguments))))

(irc:add-hook *connection* 'irc:irc-privmsg-message 'msg-hook)

(irc:read-message-loop *connection*)
