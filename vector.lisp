(in-package :now)

(defun draw-icon (icon color)
  (with-canvas (:width 24 :height 24)
    (let ((font (get-font (now-file "fontawesome-webfont.ttf"))))
      (set-font font 24)
      (when color
        (multiple-value-bind (r g b a) (rgba-from-hex color)
          (set-rgba-fill (float (/ r 255))
                         (float (/ g 255))
                         (float (/ b 255))
                         (float (/ (or a 255) 255))))        )
      (draw-string 0 4 (princ-to-string (icon-index-from-name icon)))
      (stroke)
      (flexi-streams:with-output-to-sequence (stream)
        (save-png-stream stream)))))

(defun-simple-memoized draw-icon-memoized (icon-color :test equal)
  (draw-icon (first icon-color) (second icon-color)))

(defun vector-dispatch ()
  (let ((raw (subseq (hunchentoot:script-name*) 10)))
    (destructuring-bind (name &optional color) (split-sequence #\/ raw)
      (draw-icon-memoized (list name color)))))
