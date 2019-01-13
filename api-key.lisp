
(defpackage "API-KEY"
  (:use "COMMON-LISP")
  (:export "LOAD-API-KEY"))
(in-package "API-KEY")

(defun load-api-key (service-name)
  (let ((path (merge-pathnames (make-pathname :name service-name :type "apikey")
                               (uiop/configuration:xdg-config-home) nil)))
    (with-open-file (in path :direction :input)
      (string-trim #(#\space #\tab) (read-line in)))))
