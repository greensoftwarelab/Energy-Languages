;; The Computer Language Benchmarks Game
;; http://benchmarksgame.alioth.debian.org/
;;
;; adapted from the C version by Kentaro Nakazawa 2016-11-08

#+sbcl (require :sb-gmp)		; GMP disabled when commenting out

(declaim (optimize (speed 3) (debug 0) (space 0) (safety 0)))

(declaim (type integer acc den num))
(defparameter acc 0)
(defparameter den 1)
(defparameter num 1)

(defun extract-digit (nth)
  (declare (type fixnum nth))
  (the fixnum (truncate (+ (* num nth) acc) den)))

(defun eliminate-digit (d)
  (declare (type fixnum d))
  (decf acc (* den d))
  (setf acc (* acc 10))
  (setf num (* num 10)))

(defun next-term (k)
  (declare (type fixnum k))
  (let ((k2 (1+ (* k 2))))
    (declare (type fixnum k2))
    (incf acc (* num 2))
    (setf acc (* acc k2))
    (setf den (* den k2))
    (setf num (* num k))))

(defun main ()
  (do ((d 0) (k 0) (i 0) (n 10000))
      ((>= i n))
    (declare (type fixnum d k i n))
    (setf n (parse-integer (nth 1 #+sbcl sb-ext:*posix-argv*
				  #+cmu extensions:*command-line-strings*
				  #+gcl si::*command-args*)))
    (next-term (incf k))
    (when (> num acc)
      (go continue))

    (setf d (extract-digit 3))
    (when (/= d (extract-digit 4))
      (go continue))

    (princ d)
    (when (= (mod (incf i) 10) 0)
      (format t "~C:~D~%" #\Tab i))
    (eliminate-digit d)
   continue))

