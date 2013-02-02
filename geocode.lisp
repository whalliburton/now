(in-package :now)

(defun geocode (address &optional bounds)
  (let ((rtn
          (json:decode-json-from-string
           (babel:octets-to-string
            (http-request (format nil "http://maps.googleapis.com/maps/api/geocode/json?address=~A~@[&bounds=~A~]&sensor=false"
                                  (url-encode address)
                                  (and bounds (url-encode bounds))))))))
    (cdr (assoc :results rtn))))

(defun geocode-latlng (lat lng &optional pretty)
  (let* ((rtn
           (json:decode-json-from-string
            (babel:octets-to-string
             (http-request (format nil "http://maps.googleapis.com/maps/api/geocode/json?latlng=~A,~A&sensor=false"
                                   lat lng)))))
         (raw (cdr (assoc :results rtn))))
    (if pretty
      (progn
        (mapc #'print-geocode raw)
        (values))
      raw)))

(defun decode-geocode (geocode)
  (let* ((address (second (assoc :address--components geocode)))
         (types (cdr (assoc :types geocode)))
         (name (cdr (assoc :long--name address)))
         (geometry (cdr (assoc :geometry geocode)))
         (location (cdr (assoc :location geometry)))
         (lat (cdr (assoc :lat location)))
         (lng (cdr (assoc :lng location))))
    (values name lat lng types)))

(defun decode-geocode-list (geocode)
  (multiple-value-list (decode-geocode geocode)))

(defun print-geocode (geocode)
  (multiple-value-bind (name lat lng types) (decode-geocode geocode)
    (format t "~30A  ~10A  ~10A  ~{~A~^ ~}~%" name lat lng types)))
