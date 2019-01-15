
(asdf:defsystem "photter"
  :description "Photter IRC bot"
  :author "Tamas Molnar"
  :version "0.0.1"
  :license "MIT"
  :depends-on ("cl-irc" "split-sequence" "drakma" "cl-json" "plump")
  :components ((:file "api-key")
               (:file "web-echo")
               (:file "weather-checker" :depends-on ("api-key"))
               (:file "photter":depends-on ("weather-checker")))
  #+asdf-unicode :encoding #+asdf-unicode :utf-8)
