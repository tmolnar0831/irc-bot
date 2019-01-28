;;;; Name:    OpenWeatherMap - Common Lisp API Client
;;;; Author:  Tamas Molnar - tmolnar0831@gmail.com
;;;; License: MIT

(defpackage :weather-checker
  (:use "COMMON-LISP"
        "APIXU")
  (:export "*LOCATION-FILE*"
           "SAVE-WEATHER-DB"
           "LOAD-WEATHER-DB"
           "ADD-LOCATION"
           "GET-LOCATION"
           "REM-LOCATION"
           "CURRENT-WEATHER-INFORMATION"
           "WEATHER-ERROR"
           "WEATHER-ERROR-CODE"
           "WEATHER-ERROR-MESSAGE"))

(in-package :weather-checker)

(defvar *location-db* (make-hash-table :test 'equal)
  "Hash table for location data")
(defvar *location-file* "~/common-lisp/weather-location.db"
  "File for the weather location db")

(defun current-weather-information (location)
  "Return the current weather information"
  (let ((weather-information (apixu:apixu-query location)))
    (formatted-weather-data weather-information)))

(defun add-location (nick location)
  "Add a new weather location or modify it"
  (setf (gethash nick *location-db*) location))

(defun get-location (nick)
  "Check a location"
  (gethash nick *location-db*))

(defun rem-location (nick)
  "Remove a location"
  (remhash nick *location-db*))

(defun save-weather-db (filename)
  "Save the hash table to a file"
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *location-db* out))))

(defun load-weather-db (filename)
  "Load the hash table from a file"
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *location-db* (read in)))))

(define-condition weather-error (error)
  ((code :initarg :code :reader weather-error-code)
   (message :initarg :message :reader weather-error-message))
  (:report (lambda (condition stream)
             (write-string (weather-error-message condition) stream))))

(defun aget (alist key) (cdr (assoc key alist)))

(defun formatted-weather-data (data)
  "Return a formatted answer string"
  (format nil "~A, ~A, ~A, ~A, ~A°C, feels like ~A°C, wind ~Akph ~A, precipitation ~Amm, humidity ~A%, UV ~A"
          (aget (aget data :location) :name)
          (aget (aget data :location) :region)
          (aget (aget data :location) :country)
          (aget (aget (aget data :current) :condition) :text)
          (aget (aget data :current) :temp--c)
          (aget (aget data :current) :feelslike--c)
          (aget (aget data :current) :wind--kph)
          (aget (aget data :current) :wind--dir)
          (aget (aget data :current) :precip--mm)
          (aget (aget data :current) :humidity)
          (aget (aget data :current) :uv)))
