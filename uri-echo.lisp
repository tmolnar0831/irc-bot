;;;; Name:    Photter IRC BOT URI echo module
;;;; Author:  Tamas Molnar - tmolnar0831@gmail.com
;;;; License: MIT

(defpackage :uri-echo
  (:use "COMMON-LISP" "PLUMP" "DRAKMA")
  (:export "PROCESS-MESSAGE-FOR-URI-ECHO"))

(in-package :uri-echo)

(defun decode-html-entities (txt)
  "Decode the HTML entities in a text"
  (let* ((quot (cl-ppcre:regex-replace-all "\&quot;" txt "\"" :preserve-case t))
         (amp (cl-ppcre:regex-replace-all "\&amp;" quot "&" :preserve-case t))
         (lt (cl-ppcre:regex-replace-all "\&lt;" amp "<" :preserve-case t))
         (decoded-html (cl-ppcre:regex-replace-all "\&gt;" lt ">" :preserve-case t)))
    decoded-html))

(defun process-message-for-uri-echo (msg)
  "Main input for Photter to check if there is an URI in the message"
  (let ((uris (collect-uris (look-for-uri msg))))
    (if (> (length uris) 0)
        (map 'list #'(lambda (uri)
                       (let ((full-title (nth 0 (find-title-tag (query-uri uri)))))
                         (if full-title
                             (let* ((remove-first-tag (subseq full-title 7))
                                    (title (reverse (subseq (reverse remove-first-tag) 8))))
                               (string-trim '(#\Space #\Tab #\Newline) title)
                               (decode-html-entities title)))))
             uris))))

(defun query-uri (uri)
  "Query a URI, return the HTTP response"
  (multiple-value-bind (response status headers uri)
      (drakma:http-request uri :force-binary t)
    (if (and (= status 200)
             (string-equal (cdr (assoc :content-type headers)) "text/html" :end1 9))
        (flexi-streams:octets-to-string response :external-format :utf-8)
        nil)))

(defun find-title-tag (uri)
  "Return the title tag from the HTTP response"
  (if uri
      (last (map 'list #'(lambda (node)
                           (plump:serialize node nil))
                 (plump:get-elements-by-tag-name (plump:parse uri) "title")))
      nil))

(defun look-for-uri (msg)
  "Search for URIs in the incoming message"
  (mapcar #'(lambda (item)
              (if (> (length item) 4)
                  (if (string-equal "http" item :end1 4 :end2 4)
                      item)))
          msg))

(defun collect-uris (msg)
  "Collect only the URIs to a list"
  (let ((uris (loop for x in (look-for-uri msg)
                    if x
                      collect x)))
    uris))
