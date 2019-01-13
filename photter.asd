(asdf:defsystem "photter"
  :description "An IRC bot for #photography"
  :author "Tamas Molnar"
  :version "0.0.1"
  :license "MIT"
  :depends-on ("cl-irc"
               "split-sequence"
               "weather-checker")
  :components ((:file "api-key"         :depends-on ())
               (:file "photter"         :depends-on ("api-key"))))

(asdf:defsystem "weather-checker"
  :description "An OpenWeatherMap IRC bot extensing for #photography"
  :author "Tamas Molnar"
  :version "0.0.1"
  :license "MIT"
  :depends-on ("drakma" "cl-json")
  :components ((:file "weather-checker" :depends-on ())))
