(in-package :now)

(defmacro render-page (title &rest body)
  `(with-html-output-to-string (stream)
     (:html
      (:head
       (:title (str ,title))
       (:link :rel "stylesheet" :type "text/css" :href "/css/now.css")
       (:script :src "/js/now.js" :type "text/javascript"))
     (:body :id "body"
            ;; (:div :class "settings"  :style "width:32px;height:32px;"
            ;;       :onmouseover "showSettings();"
            ;;       :onmouseout "hideSettings();"
            ;;       (:img :id "settings-icon"
            ;;             :style "visibility:hidden;" :src "/images/cog.png"
            ;;             :onclick "showSettingsDialog();"))
            ,@body))))

(defun create-button-strip (stream description)
  (with-html-output (stream)
    (:table
     (:tr
      (iter (for (name onclick selected) in description)
            (htm (:td (:div :class (if selected "page-button-selected" "page-button")
                            :onclick onclick (str name)))))))))

(defun render-front-page (what id)
  (render-page
   "Now"
   (create-button-strip stream
                        `(("events" "go(\"/?what=event\");" ,(or (and (null what) (null id))
                                                                 (equal what "event")))
                          ("places" "go(\"/?what=place\");" ,(equal what "place"))
                          ("days" "go(\"/?what=day\");" ,(equal what "day"))))
   (cond
     (id
      (let ((node (deck:get-node (parse-integer id))))
        (htm
         (:div
          :style "padding:40px"
          (:div :style "font-size:30pt;padding-bottom:40px;" (esc (deck-client:name (template node))))
          (render-node node stream)
          (render-children node stream)
          (render-node node stream t)))))
     (what
      (htm
       (:br)
       (:table
        (iter (for node in (deck:search (format nil "demo:~A" what)))
              (render-node node stream))))))))

(defun render-node (node stream &optional detail)
  (let* ((template-id (template-id node))
         (type (cond
                 ((template-is-type-of template-id "demo:place") :place)
                 ((template-is-type-of template-id "demo:day") :day)
                 ((template-is-type-of template-id "demo:event") :event))))
    (with-html-output (stream)
      (:tr
       (:td
        :class "selectable-row"
        :onclick (format nil "go(\"/?id=~A\");" (id node))
        (case type
          (:place (render-place node stream detail))
          (:day (render-day node stream detail))
          (:event (render-event node stream detail))))))))

(defun event-time-string (event)
  (when-let ((starting (car (second (car (deck:search `((:node :any (:= :id ,(id event))) "starting") :with-edges t))))))
    (format-timestring t (field-value starting "time") :format +day-time-format+)))

(defun render-event (event stream detail)
  (with-html-output (stream)
    (if detail
      (htm
       (aand (event-time-string event)
             (htm (:div :style "padding:20px;font-size:20pt;" (esc it))))
       (:pre :style "font-family:Futura;"
             (esc (field-value event "description"))))
      (esc (field-value event "name")))))

(defun render-place (place stream detail)
  (with-html-output (stream)
    (if detail
      nil
      (esc (field-value place "name")))))

(defparameter +day-time-format+
  '(:short-weekday #\space :short-month #\space (:day 2 #\space)))

(defun render-day (day stream detail)
  (if detail
    nil
    (format-timestring stream (field-value day "start") :format +day-time-format+)))

(defun render-children (node stream)
  (when-let (children (deck:get-children node :any-direction t))
    (with-html-output (stream)
      (:table :style "padding:10px 0px 0px 20px;"
              (iter (for child in children)
                    (render-node child stream))))))