(in-package :now)

(defparameter *maps-language* "en")

(defmacro render-page (title &rest body)
  `(with-html-output-to-string (stream)
     (:html
      (:head
       (:title (str ,title))
       (:link :rel "stylesheet" :type "text/css" :href "/css/now.css")
       (:script :src (format nil "https://maps.google.com/maps/api/js?key=~A&sensor=true&language=~A"
                             *google-api-key* *maps-language*)
                :type "text/javascript")
       (:script :src "/js/now.js" :type "text/javascript")
       (:script :src "/js/iscroll.js" :type "text/javascript")
       (:script :src "http://maps.stamen.com/js/tile.stamen.js?v1.2.1" :type "text/javascript"))
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
      (cond
        ((equal what "map")
         (htm (:span :class "button"
                     :style "cursor:pointer;padding:5px 10px 5px 10px;" :onclick "go(\"/\");"
                     (icon :arrow-up))
              (:br) (:br)
              (render-map stream)))
        ((equal what "icons") (render-icons stream))
        ((equal what "colors") (render-colors stream))
        (t
         (htm
          (:table
           (:tr
            (iter (for name in '("tag" "place" "event" "day" ("map")))
                  (let ((template (not (consp name)))
                        (name (if (consp name) (car name) name)))
                    (htm (:td :class "selectable-row" :onclick (format nil "go(\"/?what=~A\");" name)
                              (esc name) (when template (str "s"))))))))
          (:br)
          (:table
           (iter (for node in (deck:search (format nil "demo:~A" (or what "tag"))))
                 (render-node node stream))))))))))

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
            (:event :time)
            (:tag :tag)))
    (princ #\space stream)))

(defun mapto (stream name lat lon zoom &optional onload controls-id)
  (let ((id (random-string 3)))
    (with-html-output (stream)
      (htm
       (:div :id id :style "width:600px;height:600px;")
       (:script :type "text/javascript"
                (fmt "mapto(~S,~A,~A,~A,~S,~S,~S);sendClientLocation();setMapStyles(~S);"
                     id lat lon zoom name (or onload "null") (or controls-id "null")
                     (create-map-styles)))))))

(defmethod render ((type (eql :place)) node stream detail)
  (let ((latitude (field-value node "latitude")))
    (with-html-output (stream)
      (if detail
        (progn
          (when latitude
            (mapto stream (field-value node "name") latitude (field-value node "longitude") 8)))
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

(defmethod render ((type (eql :tag)) node stream detail)
  (if detail
    nil
    (let ((name (field-value node "name")))
      (with-html-output (stream)
        (when-let (icon (icon-from-yelp-category name))
          (icon icon)
          (str " "))
        (esc (substitute #\space #\_ name))))))

(defun render-children (node stream)
  (when-let (children (deck:get-children node :any-direction t))
    (with-html-output (stream)
      (:table :style "padding:10px 0px 0px 20px;"
              (iter (for child in children)
                    (render-node child stream))))))

(defvar *initial-map-position* nil)

(defun render-map (stream)
  (destructuring-bind (name lat lng zoom) *initial-map-position*
    (with-html-output (stream)
      (:table
       (:tr
        (:td :style "vertical-align:top;padding:5px;"
             (:table
              (:tr (:td
                    (:span :class "button"
                           :style "cursor:pointer;padding:5px 10px 5px 10px;"
                           :onclick "centerOnMarker();"
                           (icon :map-marker))))
              (:tr (:td :style "padding-top:10px;"
                    (:span :class "button"
                           :style "cursor:pointer;padding:5px 10px 5px 10px;"
                           :onclick "request(\"show-layer-list-dialog\");"
                           (icon :check))))))
        (:td :style "vertical-align:top;"
             (mapto stream name lat lng zoom "sendDragend();" "controls"))
        (:td :style "width:20px;")
        (:td :style "vertical-align:top" :rowspan 3
             (:div :style "height:600px;" :id "wrapper"
                   (:div :id "list"))))
       (:tr (:td) (:td (:div :style "padding:20px 20px 0px 20px;" :id "controls")))
       (:tr
        (:td)
        (:td :style "padding:20px;vertical-align:top;"
             (:input :style "border-width:1px;border-color:#222;border-style:solid;padding:5px 10px 5px 10px;width:360px;background-color:black;color:white;" :type "text"
                     :onchange "sendNewMapLocation(this);")))))))

(defparameter *local-bounds* nil)

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
                 (finally (return (append inside (when outside (list nil)) outside))))))
    (format nil "setContents('list',~S);startIscroll('wrapper');setupPois(~S);setMapStyles(~S);"
            (with-html-output-to-string (stream)
              (:div
               (:table
                :class "maplist"
                (iter (for el in sorted)
                      (for index from 0)
                      (let ((box-id (format nil "box-~A" index)))
                        (if el
                          (destructuring-bind (inside name lat lng &optional icon) el
                            (declare (ignore inside))
                            (htm (:tr :class "selectable"
                                      :id box-id
                                      :onclick (format nil "selectMaplist(~S,~A,~A);"
                                                       (cl-who:escape-string name) lat lng)
                                      :onmouseover (format nil "hilightPoi(~S);" box-id)
                                      :onmouseout  (format nil "unhilightPoi(~S);" box-id)
                                      (:td (when icon
                                             (htm (:img :src
                                                        (format nil "/images/v/~(~A~)" icon)))))
                                      (:td (esc name))
                                      ;; (:td (fmt "~A" lat)) (:td (fmt "~A" lng))
                                      )))

                          (htm (:tr (:td :colspan 3 (:hr))))))))))
            (json:encode-json-to-string
             (iter (for el in sorted)
                   (for index from 0)
                   (when el
                     (destructuring-bind (inside name lat lng &optional icon) el
                       (declare (ignore inside))
                       (collect (list name lat lng (and icon (format nil "v/~(~A~)/24/0005/FFFF" icon))
                                      (format nil "box-~A" index)))))))
            (create-map-styles))))

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
;            (hits (geocode name (format nil "~A,~A|~A,~A" sw1 sw2 ne1 ne2)))
            (yelps (yelp-business-search :term name :box (list sw1 ne2 ne1 sw2))))
        (set-maplist
         (when yelps
           (append
;            (and yelps (decode-yelps yelps))
;            (mapcar #'decode-geocode-list hits)
            )))))))


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
  (format nil "centerMap(~A,~A);infoWindow(~A,~A,~S);"
          lat lng
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
                          (field-value node "longitude")
                          (icon-from-yelp-category (first (field-value node "categories")))))
                  local)
;          (decode-yelps yelps)
          )))))
  )

(defun handle-map-dragend (bounds)
  (update-map-on-new-bounds (parse-text-bounds bounds)))

(defun handle-map-zoom-changed (bounds zoom)
  (update-map-on-new-bounds (parse-text-bounds bounds)))

(defun set-client-location (lat lng)
  (format nil "moveMarker(~A,~A,'self');centerOnMarker();" lat lng))

(defun show-layer-list-dialog ()
  (format nil "showDialog(~S);startIscroll('listing');"
          (with-html-output-to-string (stream)
            (:div :style "background-color:black;border:1px solid gray;padding:10px;"
             (:div :id "listing"
                   :style "padding:20px;height:400px;"
                   (:table
                    (iter (for (code name) in *map-features*)
                          (let ((selected (member code (current-map-styles))))
                            (htm (:tr
                                  :id code
                                  :style "padding:10px;"
                                  :class (if selected "selected" "selectable")
                                  :onclick (format nil "request(\"toggle-map-feature\",{name:\"~A\"});" code)
                                  (:td (esc name))))))))
             (:div :style "height:20px;")
             (:div :class "button"
                   :onclick "closeDialog();" (esc "Done"))))))

(defun toggle-map-feature (name)
  (let* ((code (or (car (assoc name *map-features* :test #'string-equal))
                   (error "Unknown feature ~S." name)))
         (selected (member code (session-value 'map-styles))))
    (if selected
      (setf (session-value 'map-styles) (delete code (session-value 'map-styles)))
      (pushnew code (session-value 'map-styles)))
    (format nil "setClass(\"~A\",~S);setMapStyles(~S);" code (if selected "selectable" "selected")
            (create-map-styles))))