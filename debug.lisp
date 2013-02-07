(in-package :now)

(defun render-icons (stream)
  (with-html-output (stream)
    ;;  (iter (for size from 10 to 200)
    ;;        (htm
    ;;         (:tr (:td (:img :src (format nil "/images/v/~(~A~)/FFF/~A" "money" size))
    ;;                   (:img :src (format nil "/images/v/~(~A~)/FFF/~A" "glass" size)))))))
    (:table
     (iter (for row in (group *icon-index* 20))
           (htm
            (:tr
             (iter (for (icon index) in row)
                   (htm (:td (:img :src (format nil "/images/v/~(~A~)/FFF/~A" icon 42)))))))))))

