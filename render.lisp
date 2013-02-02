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
                    :style "cursor:pointer;padding:5px 10px 5px 10px;" :onclick "go(\"/\");"
                    (icon :arrow-up)))
        (htm
         (:div
          :style "padding:30px"
          (render-node node stream)
          (render-children node stream)
          (:br)
          (render-node node stream t)))))
     (t
      (if (equal what "map")
        (htm (:span :class "button"
                    :style "cursor:pointer;padding:5px 10px 5px 10px;" :onclick "go(\"/\");"
                    (icon :arrow-up))
             (:br) (:br)
             (render-map stream))
        (htm
         (:table
          (:tr
           (iter (for name in '("event" "place" "day" ("map")))
                 (let ((template (not (consp name)))
                       (name (if (consp name) (car name) name)))
                   (htm (:td :class "selectable-row" :onclick (format nil "go(\"/?what=~A\");" name)
                             (esc name) (when template (str "s"))))))))
         (:br)
         (:table
          (iter (for node in (deck:search (format nil "demo:~A" (or what "event"))))
                (render-node node stream)))))))))

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

(defun mapto (stream name lat lon zoom)
  (let ((id (random-string 3)))
    (with-html-output (stream)
      (htm
       (:div :id id :style "width:400px;height:400px;")
       (:script :type "text/javascript"
                (fmt "mapto(~S,~A,~A,~A,~S);" id lat lon zoom name))))))

(defmethod render ((type (eql :place)) node stream detail)
  (let ((latitude (field-value node "latitude")))
    (with-html-output (stream)
      (if detail
        (when latitude
          (mapto stream (field-value node "name") latitude (field-value node "longitude") 8))
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

(defun render-map (stream)
  (with-html-output (stream)
    (:table
     (:tr
      (:td :style "vertical-align:top;padding:5px;"
       (:span :class "button"
              :style "cursor:pointer;padding:5px 10px 5px 10px;" :onclick "centerOnMarker(14);"
              (icon :map-marker)))
      (:td
       (mapto stream "Merlyn's" 47.658752 -117.41198 18))
      (:td :style "width:50px;")
      (:td (:div :id "list"))))
    (htm
     (:br)
     (:input :style "border-width:0px;padding:5px 10px 5px 10px;" :type "text"
             :onchange "sendNewMapLocation(this);"))))

(defun set-map-position (lat lng)
  (let ((lat (parse-float lat))
        (lng (parse-float lng)))
    (when-let ((near (geocode-latlng lat lng)))
      (format nil "setContents('list',~S);"
              (with-html-output-to-string (stream)
                (:table :class "maplist"
                 (iter (for el in near)
                       (multiple-value-bind (name lat lng) (decode-geocode el)
                         (htm (:tr (:td (esc name)) (:td (fmt "~A" lat)) (:td (fmt "~A" lng))))))))))))

(defun set-map-location (name bounds)
  (destructuring-bind (sw1 sw2 ne1 ne2) (split-sequence #\, (url-decode bounds))
    (let ((sw1 (parse-float sw1))
          (sw2 (parse-float sw2))
          (ne1 (parse-float ne1))
          (ne2 (parse-float ne2))
          (hits (geocode name (format nil "~A,~A|~A,~A" sw1 sw2 ne1 ne2)))
          (yelps (yelp-business-search :box (list ))))
      (or
       (iter (for el in hits)
             (multiple-value-bind (name lat lng) (decode-geocode el)
               (when (and (< sw1 lat se1) (< sw2 lng se2))
                 (format t "map hit: ~A ~A ~A~%" name lat lng)
                 (return (format nil "moveMap(~A,~A,8);moveMarker(~A,~A,~S);" lat lng lat lng name)))))
       (if hits
         (progn
           (format t "No results inside bounds.~%")
           (mapc #'print-geocode hits))
         (format t "No map results."))))))
