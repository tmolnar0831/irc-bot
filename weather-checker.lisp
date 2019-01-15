;;;; Name:    OpenWeatherMap Common Lisp API Client
;;;; Author:  Tamas Molnar - tmolnar0831@gmail.com
;;;; License: MIT

(defpackage :weather-checker
  (:use "COMMON-LISP")
  (:export "*API-KEY*"
           "FORMAT-ANSWER-STRING"
           "GET-PROCESSED-OUTPUT"
           "WEATHER-ERROR"
           "WEATHER-ERROR-CODE"
           "WEATHER-ERROR-MESSAGE"))
(in-package :weather-checker)

(defvar *api-url* "https://api.openweathermap.org/data/2.5/weather"
  "The URL of the OpenWeatherMap API")
(defvar *api-query-string* "?q="
  "The query string for the API requests")
(defvar *api-city* nil
  "The city for the query")
(defvar *api-country* nil
  "The country code in ISO 3166 format")
(defvar *api-units* nil
  "The metrics for the API response")
(defvar *api-key* nil)

(defun build-query-url (url query-string city &optional country (key *api-key*) (unit "&units=metric"))
  "Returns the URL string for the API query"
  (cond (country
         (format nil "~A~A~A,~A&APPID=~A~A" url query-string city country key unit))
        (t
         (format nil "~A~A~A&APPID=~A~A" url query-string city key unit))))

(defun run-query (city &optional country)
  "Query the URL and return a vector object"
  (cond (country
         (babel:octets-to-string
          (drakma:http-request (build-query-url *api-url* *api-query-string* city country))))
        (t
         (babel:octets-to-string
          (drakma:http-request (build-query-url *api-url* *api-query-string* city))))))

(defun process-answer (answer)
  (with-input-from-string (s answer)
    (json:decode-json s)))

(define-condition weather-error (error)
  ((code :initarg :code :reader weather-error-code)
   (message :initarg :message :reader weather-error-message))
  (:report (lambda (condition stream)
             (write-string (weather-error-message condition) stream))))

(defun aget (alist key) (cdr (assoc key alist)))

(defun get-processed-output (city &optional country)
  (let ((answer (cond (country
                       (process-answer (run-query city country)))
                      (t
                       (process-answer (run-query city))))))

    (if (eql (cdr (assoc :cod answer)) 200)
        answer
        (error 'weather-error
               :code (aget answer :cod)
               :message (aget answer :message)))))

(defun weather-city (data)
  "Return the city name"
  (rest (assoc :name data)))

(defun weather-temperature (data)
  "Return the temperature in Celsius"
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

(defun format-answer-string (data)
  "Return a formatted string as answer"
  (format nil "~A ~A, ~A°C, wind ~Akm/h, pressure ~AhPa, humidity ~A%"
          (weather-city data)
          (weather-country data)
          (weather-temperature data)
          (weather-wind data)
          (weather-pressure data)
          (weather-humidity data)))

