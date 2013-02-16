(in-package :now)

(defvar *now-acceptor* nil)
(defvar *now-port* 1234)
(defvar *now-username* "fast")
(defvar *now-password* "track")

(defun start-now ()
  (when *now-acceptor* (hunchentoot:stop *now-acceptor*))
  (setf hunchentoot:*message-log-pathname* (now-file (format nil "logs/message-~A.log" (now)))
        hunchentoot:*access-log-pathname* (now-file (format nil "logs/access-~A.log" (now)))
        *now-acceptor*
        (hunchentoot:start
         (make-instance 'hunchentoot:acceptor :port *now-port*))
        hunchentoot:*dispatch-table*
        `(,(hunchentoot:create-prefix-dispatcher "/css/now.css" 'now-css)
          ,(hunchentoot:create-prefix-dispatcher "/a" 'ajax-handler)
          ,(hunchentoot:create-static-file-dispatcher-and-handler "/futura.ttf"
                                                                  (now-file "futura.ttf"))
          ,(hunchentoot:create-static-file-dispatcher-and-handler "/fontawesome-webfont.ttf"
                                                                  (now-file "fontawesome-webfont.ttf"))
          ,(hunchentoot:create-static-file-dispatcher-and-handler
            "/js/iscroll.js" (now-file "data/iscroll.js"))
          ,(hunchentoot:create-prefix-dispatcher "/js/now.js" 'now-js:js-file)
          ,(hunchentoot:create-prefix-dispatcher "/images/v/" 'vector-dispatch)
          ,(hunchentoot:create-folder-dispatcher-and-handler "/images/" (now-file "images/"))
          ,(hunchentoot:create-prefix-dispatcher "/favicon.ico" 'favicon-dispatch)
          hunchentoot:dispatch-easy-handlers
          hunchentoot:default-dispatcher)))

(defun now-css () (slurp-file (now-file "now.css")))

(defun favicon-dispatch () (slurp-as-octets (now-file "icon.ico")))

(hunchentoot:define-easy-handler (front-page :uri "/") (what id)
  (multiple-value-bind (username password) (hunchentoot:authorization)
    (if (not (and (string= username *now-username*)
                  (string= password *now-password*)))
      (hunchentoot:require-authorization "now")
      (progn
        (hunchentoot:start-session)
        (render-front-page what id)))))

(defun ajax-handler ()
  (let ((args (mapcar (lambda (el) (let ((pos (position #\= el)))
                                     (if pos
                                       (list
                                        (intern (string-upcase (subseq el 0 pos)) :keyword)
                                        (subseq el (1+ pos)))
                                       el)))
                      (split-sequence
                       #\&
                       (subseq (hunchentoot:request-uri hunchentoot:*request*)
                               (1+ (position #\& (hunchentoot:request-uri hunchentoot:*request*))))))))
    (flet ((with-args (&rest names)
             (loop for name in names
                   collect (or (second (assoc name args))
                               (error "Missing arg ~S." name)))))
      (let ((command (first (with-args :command))))
        (format t "command: ~A  ~S~%" command args)
        (cond
          ((string-equal command "add-comment")
           (apply #'add-comment nil (with-args :target :text)))
          ((string-equal command "add-comment-draft")
           (apply #'add-comment t (with-args :target :text)))
          ((string-equal command "show-settings-dialog")
           (show-settings-dialog))
          ((string-equal command "show-avatars-dialog")
           (show-avatars-dialog))
          ((string-equal command "handle-map-click")
           (apply #'handle-map-click (with-args :lat :lng)))
          ((string-equal command "handle-map-zoom-changed")
           (apply #'handle-map-zoom-changed (with-args :bounds :zoom)))
          ((string-equal command "handle-map-dragend")
           (apply #'handle-map-dragend (with-args :bounds)))
          ((string-equal command "set-map-location")
           (apply #'set-map-location (with-args :name :bounds)))
          ((string-equal command "select-maplist")
           (apply #'select-maplist (with-args :name :lat :lng)))
          ((string-equal command "set-client-location")
           (apply #'set-client-location (with-args :lat :lng)))
          ((string-equal command "show-layer-list-dialog")
           (show-layer-list-dialog))
          ((string-equal command "toggle-map-feature")
           (apply #'toggle-map-feature (with-args :name))))))))

(defun add-comment (draft target-id text)
  (when (plusp (length text))
    (deck:add-edge (if draft "demo:draft comment of" "demo:comment of")
                   (safe-parse-integer target-id)
                   (deck:add-node "demo:comment" `(("text" ,(hunchentoot:url-decode text)))))
    (format t "comment added: ~A ~A~&" target-id text)))

(defun show-settings-dialog ()
  (format nil "showDialog(~S);"
          (with-html-output-to-string (stream)
            (:div :class "settings-dialog"
                  (:center
                   (:img :src "/images/cog.png")
                   (:table
                    (:tr (:td  :style "cursor:pointer;padding:20px 0 20px 0;"
                               :onclick "showAvatarsDialog();" "Choose Avatar")))
                   (:img :src "/images/check-large.png" :onclick "closeDialog();"
                         :style "cursor:pointer;"))))))


(defun show-avatars-dialog ()
  (format nil "showDialog(~S);"
          (with-html-output-to-string (stream)
            (:div :class "avatars-dialog"
                  (:span :style "font-size:32px;" "Avatars")
                  (:table
                   (:tr (:table :class "avatars"
                         (loop for row in (group *avatars* 10)
                               do (htm (:tr
                                        (loop for char in row
                                              do (htm (:td (fmt "&#~D;" (char-code char))))))))))
                   (:tr (:td (:img :src "/images/check-large.png" :onclick "showSettingsDialog();"
                                   :style "cursor:pointer;"))))))))

(defmacro with-sessions ((session-id-var session-var) &body body)
  `(iter (for (,session-id-var . ,session-var) in (with-mutex ((session-db-lock *now-acceptor*))
                             (session-db *now-acceptor*)))
         ,@body))

(defun list-all-sessions ()
  (with-sessions (nil session) (collect session)))

(defun session-seconds-since-last-click (session)
  (timestamp-difference
   (now)
   (universal-to-timestamp (hunchentoot::session-last-click session))))

(defun session-duration (session)
  (timestamp-difference
   (now)
   (universal-to-timestamp (hunchentoot::session-start session))))

(defun short-user-agent-string (session)
  (let ((user-agent (hunchentoot:session-user-agent session)))
    (cond
      ((scan "Firefox" user-agent) "firefox")
      ((scan "Conkeror" user-agent) "conkeror")
      ((scan "Chrome" user-agent) "chrome")
      ((scan "Drakma" user-agent) "drakma")
      ((scan "MSIE 9" user-agent) "ie9")
      (t user-agent))))

(defun list-sessions (&optional detail)
  (if-let (sessions (list-all-sessions))
    (print-table
     (iter (for session in (sort sessions #'< :key #'session-seconds-since-last-click))
           (let ((id (hunchentoot::session-id session)))
             (collect
                 (nconc
                  (list
                   id
                   (seconds-to-duration-string (session-seconds-since-last-click session) 0)
                   (seconds-to-duration-string (session-duration session) 0)
                   (short-user-agent-string session)
                   (session-remote-addr session))))
             (when detail
               (let ((start (universal-to-timestamp
                             (hunchentoot::session-start session)))
                     (last-click (universal-to-timestamp
                                  (hunchentoot::session-last-click session))))
                 (collect `(:subtable
                            ("user agent" ,(session-user-agent session))
                            ("remote addr" ,(session-remote-addr session))
                            ("start" ,(format nil "~A  ~A"
                                              start
                                              (seconds-to-duration-string
                                               (round (timestamp-difference (now) start)) 0)))
                            ("last click" ,(format nil "~A  ~A"
                                                   last-click
                                                   (seconds-to-duration-string
                                                    (round (timestamp-difference (now) last-click))
                                                    0)))
                            ("")))))))
     :headings '("id" "idle time" "duration" "agent" "ip"))
    (format t "no sessions~%")))

(defalias w list-sessions)
