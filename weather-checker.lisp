;;;; Name:    OpenWeatherMap Common Lisp API Client
;;;; Author:  Tamas Molnar - tmolnar0831@gmail.com
;;;; License: MIT

(defpackage :weather-checker
  (:use "COMMON-LISP")
  (:export "*API-KEY*"
           "FORMATTED-WEATHER-DATA"
           "WEATHER-LOCATION"
           "WEATHER-DATA"
           "WEATHER-ERROR"
           "WEATHER-ERROR-CODE"
           "WEATHER-ERROR-MESSAGE"))

(in-package :weather-checker)

(defvar *api-url* "https://api.openweathermap.org/data/2.5/weather"
  "URL of the OpenWeatherMap API")
(defvar *api-city* nil
  "City for the query")
(defvar *api-country* nil
  "Country code in ISO 3166 format")
(defvar *api-units* nil
  "Metrics for the API response")
(defvar *api-key* nil
  "API Key")

(defun weather-location (location)
  "Return the string representation of the location"
  (let ((country (car (reverse location)))
        (city (reverse (cdr (reverse location)))))
    (format nil "~{~A~^ ~},~A" city country)))

(defun weather-query-url (url location &optional (key *api-key*) (unit "&units=metric"))
  "Return the URI string for the API query"
  (format nil "~A?q=~A&APPID=~A~A" url location key unit))

(defun weather-query (location)
  "HTTP Request the URI and return an object containing the JSON data"
  (babel:octets-to-string
   (drakma:http-request (weather-query-url *api-url* location))))

(defun weather-json-to-answer (answer)
  "Turn JSON to Lisp lists"
  (with-input-from-string (s answer)
    (json:decode-json s)))

(define-condition weather-error (error)
  ((code :initarg :code :reader weather-error-code)
   (message :initarg :message :reader weather-error-message))
  (:report (lambda (condition stream)
             (write-string (weather-error-message condition) stream))))

(defun aget (alist key) (cdr (assoc key alist)))

(defun weather-data (location)
  "Run the HTTP request, return Lisp lists"
  (let ((answer (weather-json-to-answer (weather-query location))))
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

(defun formatted-weather-data (data)
  "Return a formatted string as answer"
  (format nil "~A ~A, ~A°C, wind ~Akm/h, pressure ~AhPa, humidity ~A%"
          (weather-city data)
          (weather-country data)
          (weather-temperature data)
          (weather-wind data)
          (weather-pressure data)
          (weather-humidity data)))

