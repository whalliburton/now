(in-package :now)

(helpers:add-config-parameters
 deck-local-socket-name             "/mnt/projects/sockets/deck"
 deck-cache-invalidation-port       2003
 deck-uses-sharder                  nil)

(defun initialize ()
  (setf hunchentoot:*catch-errors-p* nil)
  (ensure-directories-exist (now-file "logs/"))
  (start-session)
  (build)
  (import-missoula)
  (start-now)
  (format t "Welcome to Now!~%"))

(defmethod hunchentoot:maybe-invoke-debugger ((condition usocket:timeout-error))
  (warn "Timeout error. ~S" condition))