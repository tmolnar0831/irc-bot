(asdf:defsystem "photter"
  :description "Photter IRC BOT"
  :author "Tamas Molnar"
  :version "0.2.2"
  :license "MIT"
  :depends-on ("cl-irc" "split-sequence" "drakma" "cl-json" "plump")
  :components ((:file "api-key")
               (:file "uri-echo")
               (:file "weather-checker" :depends-on ("api-key"))
               (:file "photter":depends-on ("weather-checker" "uri-echo")))
  #+asdf-unicode :encoding #+asdf-unicode :utf-8)
