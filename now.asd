(defsystem :now
  :serial t
  :components ((:static-file "now.asd")
               (:file "package")
               (:file "utility")
               (:file "avatars")
               (:file "icons")
               (:file "build")
               (:file "serve")
               (:file "color")
               (:file "vector")
               (:file "google")
               (:file "maps")
               (:file "render")
               (:file "debug")
               (:file "ical")
               (:file "events")
               (:file "yelp")
               (:file "geocode")
               (:file "historic")
               (:file "importing")
               (:file "js")
               (:file "initialize"))
  :depends-on (:hunchentoot :deck-client :closure-html :alexandria :iterate :cl-who
                            :parenscript :local-time :split-sequence :chronicity
                            :drakma :cl-json :babel :anaphora :net-telent-date :fare-csv
                            :vecto :cl-ppcre))