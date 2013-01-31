(defsystem :now
  :serial t
  :components ((:static-file "now.asd")
               (:file "package")
               (:file "utility")
               (:file "avatars")
               (:file "build")
               (:file "serve")
               (:file "render")
               (:file "ical")
               (:file "events")
               (:file "yelp")
               (:file "js")
               (:file "initialize"))
  :depends-on (:hunchentoot :deck-client :closure-html :alexandria :iterate :cl-who
                            :parenscript :local-time :split-sequence :chronicity
                            :drakma :cl-json :babel :anapora))