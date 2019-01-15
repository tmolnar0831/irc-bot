;;;; Name:    Photter IRC BOT Web echo module
;;;; Author:  Tamas Molnar - tmolnar0831@gmail.com
;;;; License: MIT

(defpackage :web-echo
  (:use "COMMON-LISP" "PLUMP")
  (:export "GET-HTML-TITLE-TAG"))
(in-package :web-echo)

(defun check-uri (uri)
  "Check an URI, return the response"
  (multiple-value-bind (response status headers uri)
      (drakma:http-request uri)
    (if (= status 200)
        response)))

(defun get-html-title-tag (uri)
  "Get the content of the title tag if any"
  (let* ((result (check-uri uri))
         (root (plump:parse result))
         (titles (plump:get-elements-by-tag-name root "title"))
         (serialized-titles (map 'list #'(lambda (node) (plump:serialize node nil)) titles)))
    (plump:render-text (plump:parse (car serialized-titles)))))

(defun look-for-uri (msg)
  "Look for URIs in a list"
  (mapcar #'(lambda (item)
              (if (string-equal "http" item :end1 3 :end2 3)
                  item))
          msg))

(defun collect-uris (msg)
  "Collect only the URIs to a list"
  (let ((uris (loop for x in (look-for-uri *uzi*)
                    if x
                      collect x)))
    uris))
