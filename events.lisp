(in-package :now)

(defparameter *rss-events-feed* "http://www.missoulaevents.net/index.php/x/1cc2089018315a6063a623e8208268e7/y/blended/fuseaction/rss.dynamic.htm")

(defun nth-position (char string n)
  (let ((count 0))
    (iter
     (for c in-string string)
     (for i from 0)
     (when (char= c char)
       (incf count)
       (when (= count n) (return i))))))

(defun ical-uri-from-link (uri)
  (let* ((uri (puri:uri uri))
         (path (puri:uri-path uri)))
    (format nil "http://~A~A/type/3/fuseaction/events.save.htm"
            (puri:uri-host uri)
            (subseq path 0 (nth-position #\/ path 4)))))

(defun ical-from-link (uri)
  (parse-ical (http-request (ical-uri-from-link uri))))

(defun fetch-events ()
  (labels ((parse-time (string)
             (local-time:universal-to-timestamp (net.telent.date:parse-time string)))
           (split-description (string)
             (let ((div (nth-position #\- string 2)))
               (values
                (parse-time (substitute #\space #\- (subseq string 0 (1- div))))
                (string-right-trim '(#\space) (subseq string (+ 2 div)))))))
    (multiple-value-bind (raw status) (http-request *rss-events-feed*)
      (when (= status 200)
        (let ((stp (cxml:parse raw (stp:make-builder))) rtn)
          (stp:do-recursively (el stp)
            (when (and (typep el 'stp:element)
                       (equal (stp:local-name el) "item"))
              (push
               (iter (for child in (stp:list-children el))
                     (when (typep child 'stp:element)
                       (let ((name (stp:local-name child))
                             (data (car (stp:serialize child (cxml-xmls:make-xmls-builder)))))
                         (cond
                           ((string-equal name "pubdate")
                            (collect (list name (parse-time data))))
                           ((string-equal name "link")
                            (let ((uri (puri:uri data)))
                              (collect (list name uri))
                              (collect (list "ical" (ical-uri-from-link uri)))))
                           ((string-equal name "description")
                            (multiple-value-bind (time place) (split-description data)
                              (declare (ignore time))
                              (collect (list name place))))
                           (t (collect (list name data)))))))
               rtn)))
          (nreverse rtn))))))

(defun find-event (unique-id)
  (deck:search `((:node "demo:event" (:= "unique-id" ,unique-id)))))

(defun create-event-node (event)
  (flet ((val (key) (second (assoc key event :test #'string-equal))))
    (let ((uid (val "guid"))
          (date (val "pubdate")))
      (unless (find-event uid)
        (let ((title (val "title"))
              (ical (ical-from-link (val "ical"))))
          (format t "new event: ~A~%" title)
          (let ((place (aand (second (assoc "LOCATION" ical :test #'string=))
                             (find-or-create-place it)))
                (day (and date (find-or-create-day date)))
                (event (deck:add-node "demo:event" `(("name" ,title)
                                                     ("unique-id" ,uid)
                                                     ("description" ,(ical-summary ical))
                                                     ("ical" ,ical)))))
            (when place
              (deck:add-edge "demo:location" event place))
            (when day
              (deck:add-edge "demo:starting" event day :fields `(("time" ,date))))))))))

(defun split-raw-location (string)
  (let ((string
          (coerce (iter (for c in-string string)
                        (for pc previous c)
                        (unless (or (equal c #\\)
                                    (and (equal pc #\\)
                                         (not (equal c #\'))))
                          (collect c)))
                  'string)))
    (let ((pos (iter (for c in-string string)
                     (for pc previous c)
                     (for i from 0)
                     (when (and pc (lower-case-p pc) (or (upper-case-p c) (digit-char-p c)))
                       (return i)))))
      (if pos
        (values (subseq string 0 pos)
                (subseq string pos))
        (progn
          (warn "odd location ~S" string)
          string)))))

(defun find-or-create-place (string)
  (multiple-value-bind (name address) (split-raw-location string)
    (or
     (deck:search `((:node "demo:place" (:= "name" ,name))) :first-one t)
     (progn
       (format t "new place: ~A~%~A~%" name address)
       (deck:add-node "demo:place" `(("name" ,name)))))))

(defun create-event-nodes (events)
  (iter (for event in events)
        (create-event-node event)))

(defun unescape-string (string)
  (with-output-to-string (stream)
    (iter (for c in-string string)
          (unless (char= c #\\)
            (write-char c stream)))))

(defun ical-summary (ical)
  (unescape-string (format nil "~{~A~^~%~}" (second (assoc "SUMMARY" ical :test #'string=)))))

(defun update-events ()
  (create-event-nodes (fetch-events)))

(defun print-event (event)
  (let ((ical (field-value event "ical")))
    (format t "~A~%~A~%~%"
            (field-value event "name")
            (second (assoc "LOCATION" ical :test #'string=)))))

(defun day-from-timestamp (date)
  (multiple-value-bind (nsec ss mm hh day month year day-of-week) (decode-timestamp date)
    (declare (ignore nsec ss mm hh day-of-week))
    (encode-timestamp 0 0 0 0 day month year)))

(defun find-or-create-day (date)
  (let ((day (day-from-timestamp date)))
    (or
     (deck:search `((:node "demo:day" (:= "start" ,day))) :first-one t )
     (progn
       (format t "new day: ~A~%" day)
       (deck:add-node "demo:day" `(("start" ,day)))))))
