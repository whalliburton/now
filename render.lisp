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
           (iter (for name in '("place" "event" "day" ("map")))
                 (let ((template (not (consp name)))
                       (name (if (consp name) (car name) name)))
                   (htm (:td :class "selectable-row" :onclick (format nil "go(\"/?what=~A\");" name)
                             (esc name) (when template (str "s"))))))))
         (:br)
         (:table
          (iter (for node in (deck:search (format nil "demo:~A" (or what "place"))))
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

(defun mapto (stream name lat lon zoom &optional onload)
  (let ((id (random-string 3)))
    (with-html-output (stream)
      (htm
       (:div :id id :style "width:400px;height:400px;")
       (:script :type "text/javascript"
                (fmt "mapto(~S,~A,~A,~A,~S,~S);" id lat lon zoom name onload))))))

(defmethod render ((type (eql :place)) node stream detail)
  (let ((latitude (field-value node "latitude")))
    (with-html-output (stream)
      (if detail
        (when latitude
          (mapto stream (field-value node "name") latitude (field-value node "longitude") 8))
        (progn
          (esc (field-value node "name"))
          ;; (when latitude
          ;;   (str " ")
          ;;   (icon :star))
          )))))

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
              :style "cursor:pointer;padding:5px 10px 5px 10px;" :onclick "centerOnMarker();"
              (icon :map-marker)))
      (:td :style "vertical-align:top;"
       (mapto stream "Missoula" 46.870047 -113.995 15 "sendDragend();"))
      (:td :style "width:50px;")
      (:td :rowspan 2 (:div :id "list")))
     (:tr
      (:td)
      (:td :style "padding:20px;vertical-align:top;"
           (:input :style "border-width:1px;border-color:#222;border-style:solid;padding:5px 10px 5px 10px;width:360px;background-color:black;color:white;" :type "text"
                   :onchange "sendNewMapLocation(this);"))))))

(defparameter *local-bounds* '(46.49 -114.54 47.24 -113.45))

(defun set-maplist (elements)
  (let* ((sw1 (first *local-bounds*))
         (sw2 (second *local-bounds*))
         (ne1 (third *local-bounds*))
         (ne2 (fourth *local-bounds*))
         (sorted
           (iter (for el in elements)
                 (destructuring-bind (name lat lng &rest rest) el
                   (declare (ignore name rest))
                   (if (and (< sw1 lat ne1) (< sw2 lng ne2))
                     (collect (cons t el) into inside)
                     (collect (cons nil el) into outside)))
                 (finally (return (append inside (list nil) outside))))))
    (format nil "setContents('list',~S);setupPois(~S);"
            (with-html-output-to-string (stream)
              (:table :class "maplist"
                      (iter (for el in sorted)
                            (if el
                              (destructuring-bind (inside name lat lng &optional icon) el
                                (declare (ignore inside))
                                (htm (:tr :class "selectable"
                                          :onclick (format nil "selectMaplist(~S,~A,~A);"
                                                           (cl-who:escape-string name) lat lng)

                                          (:td (when icon
                                                 (htm (:img :src
                                                            (format nil "/images/v/~(~A~)/FFF" icon)))))
                                          (:td (esc name)) (:td (fmt "~A" lat)) (:td (fmt "~A" lng)))))

                              (htm (:tr (:td :colspan 3 (:hr))))))))
            (json:encode-json-to-string
             (iter (for el in sorted)
                   (when el
                     (destructuring-bind (inside name lat lng &optional icon) el
                       (declare (ignore inside))
                       (collect (list name lat lng (and icon (format nil "v/~(~A~)/000A" icon)))))))))))

(defun handle-map-click (lat lng)
  (let ((lat (parse-float lat))
        (lng (parse-float lng)))
    (when-let ((near (geocode-latlng lat lng)))
      (set-maplist (mapcar #'decode-geocode-list near)))))

(defun set-map-location (name bounds)
  (let ((name (string-trim '(#\space) (url-decode name))))
    (destructuring-bind (sw1 sw2 ne1 ne2) (split-sequence #\, (url-decode bounds))
      (let ((sw1 (parse-float sw1))
            (sw2 (parse-float sw2))
            (ne1 (parse-float ne1))
            (ne2 (parse-float ne2))
            (hits (geocode name (format nil "~A,~A|~A,~A" sw1 sw2 ne1 ne2)))
            (yelps (yelp-business-search :term name :box (list sw1 ne2 ne1 sw2))))
        (set-maplist
         (when (or hits yelps)
           (append
            (and yelps (decode-yelps yelps))
            (mapcar #'decode-geocode-list hits))))))))


        ;; (or
        ;;  (iter (for el in hits)
        ;;        (multiple-value-bind (name lat lng) (decode-geocode el)
        ;;          (when
        ;;            (format t "map hit: ~A ~A ~A~%" name lat lng)
        ;;            (return (format nil "moveMap(~A,~A,8);moveMarker(~A,~A,~S);" lat lng lat lng name)))))
        ;;  (if hits
        ;;    (progn
        ;;      (format t "No results inside bounds.~%")
        ;;      (mapc #'print-geocode hits))
        ;;    (format t "No map results.")))

(defun select-maplist (name lat lng)
  (format nil "moveMarker(~A,~A,~S);centerOnMarker();infoWindow(~A,~A,~S);"
          lat lng (url-decode name)
          lat lng (with-html-output-to-string (stream)
                      (:div :style "color:black;"
                            (esc (url-decode name))))))

(defun place-search (sw1 sw2 ne1 ne2)
  (deck:search `(("demo:place" (:and (:>= "longitude" ,sw2)
                                     (:<= "longitude" ,ne2)
                                     (:>= "latitude" ,sw1)
                                     (:<= "latitude" ,ne1))))))

(defun parse-text-bounds (string)
  (mapcar #'parse-float (split-sequence #\, (url-decode string))))

(defun update-map-on-new-bounds (bounds)
  (destructuring-bind (sw1 sw2 ne1 ne2) bounds
    (let (
          ;;          (yelps (yelp-business-search :box (list sw1 ne2 ne1 sw2)))
          (local (place-search sw1 sw2 ne1 ne2)))
      (when (or local ; yelps
                )
        (set-maplist
         (append
          (mapcar (lambda (node)
                    (list (field-value node "name")
                          (field-value node "latitude")
                          (field-value node "longitude")))
                  local)
;          (decode-yelps yelps)
          )))))
  )

(defun handle-map-dragend (bounds)
  (update-map-on-new-bounds (parse-text-bounds bounds)))

(defun handle-map-zoom-changed (bounds zoom)
  (update-map-on-new-bounds (parse-text-bounds bounds)))