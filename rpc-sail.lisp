(in-package :now)

(helpers:add-config-parameters
 now-rpc-port                      12401
 now-host                          "ec2-local-ip")

(defun start-sail ()
  (sail:start-sail-server
   "now"
   :port (helpers:config now-rpc-port nil)
   :host (helpers:host-or-local-ip now-host)
   :public '()
   :private ()))

(defun restart-sail ()
  (when *sails* (stop-sails))
  (sleep 0.25) ;; to get back the port
  (start-sail))
