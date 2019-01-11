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
(defun send-msg-to-channel (conn chan msg)
  (irc:privmsg conn chan msg))
(send-msg-to-channel *connection* *channel* "Hello Room!")
;(irc:op *connection* *channel* "st_iron")

(defun msg-hook (message)
  (irc:privmsg *connection* *channel* (format nil "Message: ~A" irc:arguments message)))

(irc:add-hook *connection* 'irc:irc-privmsg-message 'msg-hook)
;(irc:remove-hook *connection* 'irc:irc-privmsg-message 'msg-hook)
