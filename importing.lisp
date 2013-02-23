(in-package :now)

(defparameter *categories*
  '("coffee"
    "hospitals"
    "airports"
    "landmarks"
    "grocery"
    "medcenters"
    "drugstores"
    "museums"
    "breweries"
    "beer_and_wine"
    "pubs"
    "sportsbars"
    "divebars"
    "bars"
    "wine_bars"
    "nightlife"
    "musicvenues"
    "banks"))

(defun import-yelp-categories ()
  (destructuring-bind (sw1 sw2 ne1 ne2) *local-bounds*
    (iter (for category in *categories*)
          (format t "importing ~A~%" category)
          (let ((yelps
                  (yelp-business-search :box (list sw1 ne2 ne1 sw2) :number 20 :category category)))
            (format t "  ~A hits~%" (length yelps))
            (build-yelp-place-nodes (decode-yelps yelps))))))

(defmacro define-place (name lat lng &optional (zoom 15))
  `(defun ,(symb 'import- name) ()
     (create-templates)
     (setf *local-bounds* ',(list (- lat 0.5) (+ lng 1.0) (+ lat 0.5) (- lng 1.0))
           *initial-map-position* ',(list (format nil "~:(~A~)" name) lat lng zoom))
     (import-yelp-categories)))

(define-place gardiner 45.032837 -110.707768)
(define-place missoula 46.870047 -113.995 13)



