(asdf:defsystem "photter"
  :description "Photter IRC BOT"
  :author "Tamas Molnar"
  :version "2.0.5"
  :license "MIT"
  :depends-on ("cl-irc"
               "cl-ppcre"
               "split-sequence"
               "drakma"
               "cl-json"
               "plump"
               "apixu")
  :components ((:file "uri-echo")
               (:file "weather-checker")
               (:file "photter":depends-on ("weather-checker" "uri-echo")))
  #+asdf-unicode :encoding #+asdf-unicode :utf-8)
