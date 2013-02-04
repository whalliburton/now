(in-package :now)

(defun start-session ()
  (setf *deck-id* (start-deck-session "demo" "demo"))
  (start-printer-session))

(defparameter *templates*
  '(("day" (("start" :date)))

    ("event" (("name" :string) ("unique-id" :string) ("description" :string)
              ("ical" :any)))
    ("starting" "event" "day" nil (("time" :date)))
    ("ending" "event" "day" nil (("time" :date)))

    ("place" (("name" :string) ("latitude" :float) ("longitude" :float)
              ("notes" :any) ("category" :string)))

    ("person" (("name" :string)))

    ("meeting" (("name" :string)))

    ("location" ("meeting" "event") "place" "occuring here")

    ("hometown" "person" "place" "inhabitant")
    ("currently" "person" "place" "now here")
    ("possibly" "person" ("place" "meeting") "possibly here")
    ("occured" "person" ("place" "meeting") "occured here")))

(defun create-templates ()
  (iter (for name in (mapcar #'car *templates*)) (ignore-errors (deck:delete-template name)))
  (iter (for template in *templates*)
        (if (aand (second template) (listp it) (listp (car it)))
          (deck:add-node-template (first template) (second template))
          (deck:add-edge-template (first template) (second template) (third template)
                                  :reverse-name (fourth template)
                                  :fields (fifth template)))))

(defun build ()
  (create-templates))

(defmethod sail:serialize-replacement ((fields-base fields-base))
  (id fields-base))

