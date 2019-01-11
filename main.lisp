;;;; Name: Test IRC bot
;;;; Author: Tamas Molnar - tmolnar0831@gmail.com

;;; Load the external libraries
(require :cl-irc)

;;; Global variables
(defvar *nick* "st_iron_test_bot")
(defvar *server* "irc.freenode.net")
(defvar *channel* "#iron-bottest-room")
(defvar *connection* (irc:connect :nickname *nick*
                                  :server *server*))

;;; Channel join
(irc:join *connection* *channel*)

;;; Read the message loop
(irc:read-message-loop *connection*)

;;; Actions
(defun msg-hook (message)
  (let ((channel (first (irc:arguments message)))
        (arguments (last (irc:arguments message)))
        (source (irc:source message)))
    (irc:privmsg *connection* *channel* (format nil "~S" arguments))))

(irc:add-hook *connection* 'irc:irc-privmsg-message 'msg-hook)
