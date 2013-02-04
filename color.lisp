(in-package :now)

(defun valid-rgba-hex-color (str)
  (when-let (len (and (stringp str) (length str)))
    (and (or (= len 8) (= len 6) (= len 4) (= len 3))
         (iter (for x from 1 to (1- len))
               (unless (digit-char-p (char str x) 16)
                 (return nil))
               (finally (return str))))))

(defun rgba-from-hex (hex)
  (let ((length (length hex))
        alpha-chars)
   (if (valid-rgba-hex-color hex)
     (progn
       (when (or (= length 8) (= length 4))
         (setf alpha-chars (subseq hex (if (= length 8) 6 3))
               hex (subseq hex 0 (if (= length 8) 6 3))))
       (let* ((short (= (length hex) 3))
              (r (parse-integer
                  (or (and short (subseq hex 0 1)) (subseq hex 0 2)) :radix 16))
              (g (parse-integer
                  (or (and short (subseq hex 1 2)) (subseq hex 2 4)) :radix 16))
              (b (parse-integer
                  (or (and short (subseq hex 2 3)) (subseq hex 4 6)) :radix 16))
              (a (and alpha-chars (parse-integer alpha-chars :radix 16))))
         (if short
           (values (+ r (* 16 r)) (+ g (* 16 g)) (+ b (* 16 b)) (and a (+ a (* 16 a))))
           (values r g b a))))
     (error "Invalid hex color ~A." hex))))
