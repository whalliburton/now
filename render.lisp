(in-package :now)

(defparameter *google-api-key* "AIzaSyCuSOmkr3Fckg0sV4Eps7lJfeW_f8U_fNs")

(defmacro render-page (title &rest body)
  `(with-html-output-to-string (stream)
     (:html
      (:head
       (:title (str ,title))
       (:link :rel "stylesheet" :type "text/css" :href "/css/now.css")
       (:script :src (format nil "https://maps.google.com/maps/api/js?key=~A&sensor=true"
                             *google-api-key*)
                :type "text/javascript")
       (:script :src "/js/now.js" :type "text/javascript"))
     (:body :id "body"
            ;; (:div :class "settings"  :style "width:32px;height:32px;"
            ;;       :onmouseover "showSettings();"
            ;;       :onmouseout "hideSettings();"
            ;;       (:img :id "settings-icon"
            ;;             :style "visibility:hidden;" :src "/images/cog.png"
            ;;             :onclick "showSettingsDialog();"))
            ,@body))))

(defun render-front-page (what id)
  (render-page
   "Now"
   (cond
     (id
      (let ((node (deck:get-node (parse-integer id))))
        (htm (:span :class "button"
                    :style "cursor:pointer;padding:5px 10px 5px 10px;" :onclick "go(\"/\");" (icon :arrow-up)))
        (htm
         (:div
          :style "padding:30px"
          (render-node node stream)
          (render-children node stream)
          (:br)
          (render-node node stream t)))))
     (t
      (htm
       (:table
        (:tr
         (iter (for name in '("event" "place" "day"))
               (htm (:td :class "selectable-row" :onclick (format nil "go(\"/?what=~A\");" name)
                         (esc name) (str "s")))))))
      (htm
       (:br)
       (:table
        (iter (for node in (deck:search (format nil "demo:~A" (or what "event"))))
              (render-node node stream))))))))

(defun render-node (node stream &optional detail)
  (with-html-output (stream)
    (:tr
     (:td
      :class "selectable-row"
      :onclick (format nil "go(\"/?id=~A\");" (id node))
      (render
       (intern (string-upcase (name (template node))) :keyword)
       node stream detail)))))

(defmethod render :before (type node stream detail)
  (unless detail
    (icon (case type
            (:place :map-marker)
            (:day :calendar)
            (:event :time)))
    (princ #\space stream)))

(defmethod render ((type (eql :place)) node stream detail)
  (let ((latitude (field-value node "latitude")))
    (with-html-output (stream)
      (if detail
        (when latitude
          (let ((id (random-string 3)))
            (htm
             (:div :id id :style "width:400px;height:400px;")
             (:script :type "text/javascript"
                      (fmt "mapto(~S,~A,~A,~S);"
                           id latitude (field-value node "longitude")
                           (field-value node "name"))))))
        (progn
          (esc (field-value node "name"))
          (when latitude
            (str " ")
            (icon :star)))))))

(defun event-time-string (event)
  (when-let ((starting (car (second (car (deck:search `((:node :any (:= :id ,(id event))) "starting") :with-edges t))))))
    (format-timestring t (field-value starting "time") :format +day-time-format+)))

(defmethod render ((type (eql :event)) node stream detail)
  (with-html-output (stream)
    (if detail
      (htm
       (:div :style "width:500px;"
             (iter (for line in (split-sequence #\newline (field-value node "description")))
                   (htm (:p (esc line))))))
      (esc (field-value node "name")))))

(defparameter +day-time-format+
  '(:short-weekday #\space :short-month #\space (:day 2 #\space)))

(defmethod render ((type (eql :day)) node stream detail)
  (if detail
    nil
    (format-timestring stream (field-value node "start") :format +day-time-format+)))

(defun render-children (node stream)
  (when-let (children (deck:get-children node :any-direction t))
    (with-html-output (stream)
      (:table :style "padding:10px 0px 0px 20px;"
              (iter (for child in children)
                    (render-node child stream))))))