(in-package :now)

(defparameter *google-api-key* "AIzaSyCuSOmkr3Fckg0sV4Eps7lJfeW_f8U_fNs")

(defun places-search (lat lng radius)
  (cdr
   (assoc
    :results
    (json:decode-json-from-string
     (babel:octets-to-string
      (http-request
       (format nil "https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=~A,~A&radius=~A&sensor=true&key=~A" lat lng radius *google-api-key*)))))))
