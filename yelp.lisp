(in-package :now)

(defparameter *yelp-id* "tiPsG14mmRtpvaWTHun-_A")

(defun yelp-business-search (&key term number lat lon location box radius category)
  (cdr
   (assoc :businesses
          (json:decode-json-from-string
           (let ((raw
                   (http-request
                    (format nil "http://api.yelp.com/business_review_search?~@[term=~A&~]~
                        ~@[lat=~A&~]~@[long=~A&~]~@[tl_lat=~A&~]~@[tl_long=~A&~]~
                        ~@[br_lat=~A&~]~@[br_long=~A&~]~@[location=~A&~]~
                        ~@[radius=~A&~]~@[num_biz_requested=~A&~]~
                        ~@[category=~A&~]ywsid=~A"
                            (and term (url-encode term)) lat lon
                            (when box (first box))
                            (when box (second box))
                            (when box (third box))
                            (when box (fourth box))
                            (and location (url-encode location))
                            radius number category *yelp-id*))))
             (if (stringp raw) raw (babel:octets-to-string raw)))))))

(defun update-places-from-yelp ()
  (iter (for place in (deck:search "demo:place"))
        (update-place-from-yelp place)))

(defun update-place-from-yelp (place)
  (unless (field-value place "latitude")
    (when-let (hit (first
                    (yelp-business-search :location "Missoula, MT" :term (field-value place "name"))))
      (format t "yelp hit: ~A~%" (field-value place "name"))
      (deck:set-fields place `(("latitude" ,(cdr (assoc :latitude hit)))
                               ("longitude" ,(cdr (assoc :longitude hit)))
                               ("notes" ,hit))))))

(defun decode-yelps (yelps)
  (iter (for yelp in yelps)
        (collect (list (cdr (assoc :name yelp))
                       (cdr (assoc :latitude yelp))
                       (cdr (assoc :longitude yelp))
                       (filter-yelp-categories (cdr (assoc :categories yelp)))))))

;;; http://www.yelp.com/developers/documentation/category_list

(defparameter *yelp-category-to-icon*
  '(("coffee" :coffee)
    ("hotels" :building)
    ("airports" :plane)
    ("bookstores" :book)
    ("locksmiths" :lock)

    ("hostels" :h-sign)
    ("bedbreakfast" :h-sign)

    ("grocery" :shopping-cart)
    ("convenience" :shopping-cart)
    ("wholesale_stores" :shopping-cart)
    ("deptstores" :shopping-cart)
    ("cosmetics" :shopping-cart)
    ("healthmarkets" :shopping-cart)
    ("womenscloth" :shopping-cart)

    ("landmarks" :building)
    ("museums" :building)
    ("nonprofit" :building)
    ("drycleaninglaundry" :building)

    ("gourmet" :food)
    ("delis" :food)
    ("tradamerican" :food)
    ("vietnamese" :food)
    ("bakeries" :food)
    ("bagels" :food)
    ("icecream" :food)
    ("breakfast_brunch" :food)
    ("farmersmarket" :food)
    ("newamerican" :food)
    ("tapasmallplates" :food)
    ("diners" :food)
    ("restaurants" :food)
    ("greek" :food)
    ("burgers" :food)
    ("pizza" :food)
    ("seafood" :food)
    ("steak" :food)
    ("tapas" :food)
    ("chinese" :food)
    ("thai" :food)
    ("mexican" :food)
    ("cajun" :food)
    ("french" :food)
    ("sandwiches" :food)
    ("chicken_wings" :food)
    ("market" :food)

    ("medcenters" :hospital)
    ("drugstores" :medkit)

    ("breweries" :beer)
    ("beer_and_wine" :beer)
    ("pubs" :beer)
    ("sportsbars" :beer)
    ("divebars" :beer)
    ("bars" :glass)
    ("champagne_bars" :glass)
    ("wine_bars" :glass)
    ("nightlife" :glass)

    ("adultentertainment" :strikethrough)
    ("poolhalls" :circle)

    ("banks" :money)

    ("musicvenues" :music)))

(defun icon-from-yelp-category (category)
  (or (second (assoc category *yelp-category-to-icon* :test #'string=))
      (warn "unknown yelp category ~S" category)))

(defun filter-yelp-categories (categories)
  (iter (for el in categories)
        (collect (cdr (assoc :category--filter el)))))

(defun place-exists (name lat lng)
  (deck:nodes-exist `(("demo:place" (:and (:= "name" ,name)
                                          (:= "latitude" ,lat)
                                          (:= "longitude" ,lng))))))

(defun build-yelp-place-nodes (yelps)
  (iter (for el in yelps)
        (format t "new place: ~A~%" (first el))
        (if (place-exists (first el) (second el) (third el))
          (warn "duplicate place ~S" (first el))
          (let ((place
                  (deck:add-node "demo:place" `(("name" ,(first el))
                                                ("latitude" ,(second el))
                                                ("longitude" ,(third el))
                                                ("categories" ,(fourth el))))))
            (add-tags place (fourth el))))))

(defun add-tags (node tags)
  (iter (for tag in tags)
        (deck:add-edge "demo:tagged" node (find-or-create-tag tag))))

(defun find-or-create-tag (name)
  (or
   (deck:search `(("demo:tag" (:= "name" ,name))) :first-one t :return-ids t)
   (progn
     (format t "new tag ~S~%" name)
     (deck:add-node "demo:tag" `(("name" ,name))))))