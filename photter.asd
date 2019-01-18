
(asdf:defsystem "photter"
  :description "Photter IRC BOT"
  :author "Tamas Molnar"
  :version "0.2.0"
  :license "MIT"
  :depends-on ("cl-irc" "split-sequence" "drakma" "cl-json")
  :components ((:file "api-key")
               (:file "weather-checker" :depends-on ("api-key"))
               (:file "photter":depends-on ("weather-checker")))
  #+asdf-unicode :encoding #+asdf-unicode :utf-8)
