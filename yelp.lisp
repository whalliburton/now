(in-package :now)

(defparameter *yelp-id* "tiPsG14mmRtpvaWTHun-_A")

(defun yelp-business-search (&key term number lat lon location box radius category)
  (json:decode-json-from-string
   (let ((raw
           (http-request
            (format nil "http://api.yelp.com/business_review_search?~@[term=~A&~]~
                        ~@[lat=~A&~]~@[long=~A&~]~@[tl_lat=~A&~]~@[tl_long=~A&~]~
                        ~@[br_lat=~A&~]~@[br_long=~A&~]~@[location=~A&~]~
                        ~@[radius=~A&~]~@[num_biz_requested=~A&~]~
                        ~@[category=~A&~]ywsid=~A"
                    (url-encode term) lat lon
                    (when box (first box))
                    (when box (second box))
                    (when box (third box))
                    (when box (fourth box))
                    (url-encode location)
                    radius number category *yelp-id*))))
     (if (stringp raw) raw (babel:octets-to-string raw)))))


