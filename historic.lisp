(in-package :now)

(defun filter-historic-by-state (state)
  (with-input-from-file (stream (now-file "data/main.csv"))
    (iter (for line = (handler-case
                          (read-csv-line stream)
                        (error () nil)))
          (while line)
          (when (equal (seventh line) state)
            (format t "~A~%" line)
            (collect line)))))

(defun filter-points-with-places (places)
  (let ((points
          (let ((hash (make-hash-table)))
            (iter (for place in places)
                  (setf (gethash (parse-integer (first place)) hash) t))
            hash)))
    (with-input-from-file (stream (now-file "data/point.csv"))
      (read-line stream)
      (iter (for line = (handler-case
                            (read-csv-line stream)
                          (error () nil)))
            (while line)
            (when (gethash (parse-integer (first line)) points)
              (collect line))))))

(defun convert-utm-to-latlng (zone east north)
  (let ((raw (run-program-to-string (now-file "data/geo-conv.py") (list zone (princ-to-string east) (princ-to-string north)))))
    (mapcar #'parse-float (split-sequence #\space raw))))

(defun convert-historic-points (points)
  (iter (for (id zone east north) in points)
        (when-let ((zone-string (cond
                                  ((string= zone "10") "epsg:3157")
                                  ((string= zone "11") "epsg:2153")
                                  ((string= zone "12") "epsg:2152")
                                  ((string= zone "13") "epsg:2151")
                                  (t (warn "unknown zone ~A" zone)))))
          (let ((rtn (convert-utm-to-latlng zone-string east north)))
            (format t "~A ~A ~A -> ~A~%" zone-string east north rtn)
            (collect (append (list id) rtn))))))

(defun match-historic-points (places points)
  (iter (for el in places)
        (if-let (hit (cdr (assoc (car el) points :test #'string=)))
          (collect (append hit el))
          (warn "Missing point ~A~%" (car el)))))

(defun load-historic-places-by-state (&optional (state "MONTANA"))
  (let* ((places (filter-historic-by-state state))
         (points (convert-historic-points (filter-points-with-places places))))
    (build-historic-places-nodes (match-historic-points places points))))

(defun build-historic-places-nodes (places-latlng)
  (iter (for el in places-latlng)
        (format t "new historic place: ~A~%" (fourth el))
        (let ((place
                (deck:add-node "demo:place" `(("name" ,(fourth el))
                                              ("latitude" ,(first el))
                                              ("longitude" ,(second el))
                                              ("notes" ,el)
                                              ("categories" '("landmarks"))))))
          (add-tags place '("landmarks")))))

