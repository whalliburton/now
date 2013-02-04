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

(defun import-missoula ()
  ;; (update-events)
  ;; (load-historic-places-by-state "MONTANA")
  (create-templates)
  (setf *local-bounds* '(46.49 -114.54 47.24 -113.45)
        *initial-map-position* '("Missoula" 46.870047 -113.995 15))
  (destructuring-bind (sw1 sw2 ne1 ne2) *local-bounds*
    (iter (for category in *categories*)
          (format t "importing ~A~%" category)
          (let ((yelps
                  (yelp-business-search :box (list sw1 ne2 ne1 sw2) :number 20 :category category)))
            (format t "  ~A hits~%" (length yelps))
            (build-yelp-place-nodes (decode-yelps yelps))))))


