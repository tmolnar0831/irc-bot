;;;; Name:    OpenWeatherMap - Common Lisp API Client
;;;; Author:  Tamas Molnar - tmolnar0831@gmail.com
;;;; License: MIT

(defpackage :weather-checker
  (:use "COMMON-LISP")
  (:export "*API-KEY*"
           "*LOCATION-FILE*"
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

(defvar *api-uri* "https://api.openweathermap.org/data/2.5/weather"
  "URL of the OpenWeatherMap API")
(defvar *api-city* nil
  "City for the query")
(defvar *api-country* nil
  "Country code in ISO 3166 format")
(defvar *api-units* nil
  "Metrics for the API response")
(defvar *api-key* nil
  "API Key")
(defvar *location-db* (make-hash-table :test 'equal)
  "Hash table for location data")
(defvar *location-file* "~/common-lisp/weather-location.db"
  "File for the weather location db")

(defun current-weather-information (location)
  "Return the current weather information"
  (let* ((api-ready-location (weather-location location))
         (weather-json-information (babel:octets-to-string (drakma:http-request *api-uri*
                                                                                :method :get
                                                                                :parameters (list (cons "q" api-ready-location)
                                                                                                  (cons "APPID" *api-key*)
                                                                                                  (cons "units" "metric")))))
         (weather-lisp-information (with-input-from-string (s weather-json-information) (json:decode-json s))))
    (if (eql (cdr (assoc :cod weather-lisp-information)) 200)
        (formatted-weather-data weather-lisp-information)
        (error 'weather-error
               :code (aget weather-lisp-information :cod)
               :message (aget weather-lisp-information :message)))))

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

(defun weather-location (location)
  "Return the string representation of the location"
  (if (> (length location) 1)
      (let ((country (car (reverse location)))
            (city (reverse (cdr (reverse location)))))
        (format nil "~{~A~^ ~},~A" city country))
      (format nil "~{~A~}" location)))

(define-condition weather-error (error)
  ((code :initarg :code :reader weather-error-code)
   (message :initarg :message :reader weather-error-message))
  (:report (lambda (condition stream)
             (write-string (weather-error-message condition) stream))))

(defun aget (alist key) (cdr (assoc key alist)))

(defun weather-city (data)
  "Return the city name"
  (rest (assoc :name data)))

(defun weather-temperature (data)
  "Return the temperature"
  (rest (nth 1 (assoc :main data))))

(defun weather-pressure (data)
  "Return the pressure in hPa"
  (rest (nth 2 (assoc :main data))))

(defun weather-humidity (data)
  "Return the humidity in percentage"
  (rest (nth 3 (assoc :main data))))

(defun weather-wind (data)
  "Return the wind in km/h"
  (rest (nth 1 (assoc :wind data))))

(defun weather-country (data)
  "Return the country code"
  (cdr (nth 3 (cdr (assoc :sys data)))))

(defun weather-precipitation (data)
  "Return the precipitation"
  (cdr (nth 2 (nth 1 (assoc :weather data)))))

(defun weather-air (data)
  "Return the air conditions"
  (cdr (nth 2 (nth 2 (assoc :weather data)))))

(defun weather-sunset (data)
  "Return the time of sunset in UTC"
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (decode-universal-time (cdr (nth 6 (assoc :sys data))))
    (format nil "~A:~A" hour minute)))

(defun weather-sunrise (data)
  "Return the time of sunrise in UTC"
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (decode-universal-time (cdr (nth 5 (assoc :sys data))))
    (format nil "~A:~A" hour minute)))

(defun formatted-weather-data (data)
  "Return a formatted answer string"
  (format nil "~A ~A, ~A, ~1$°C, wind ~Akm/h, humidity ~A%~@[, ~A~]"
          (weather-city data)
          (weather-country data)
          (weather-precipitation data)
          (weather-temperature data)
          (weather-wind data)
          (weather-humidity data)
          (weather-air data)))
