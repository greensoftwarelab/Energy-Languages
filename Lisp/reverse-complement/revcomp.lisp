;;; The Computer Language Benchmarks Game
;;; http://benchmarksgame.alioth.debian.org/
;;; Michael Weber 2006-07-18
;;; Changes by Stefan Lang 2007-02-08

(defconstant +buffer-size+ (expt 2 15))
(defconstant +newline+ (char-code #\Newline))
(defconstant +ub+ '(unsigned-byte 8))

(defconstant +lut+
  (let ((lut (make-array 256 :element-type +ub+)))
    (loop for a across "wsatugcyrkmbdhvnWSATUGCYRKMBDHVN"
          for b across "WSTAACGRYMKVHDBNWSTAACGRYMKVHDBN"
          do (setf (aref lut (char-code a)) (char-code b)))
    lut))

(defun slice (array start end)
  (declare (optimize (speed 3) (safety 0))
           ((simple-array #.+ub+) array)
           (fixnum start end))
  (let ((res (make-array (- end start) :element-type +ub+)))
    (loop for i from start below end
          for j from 0
          do (setf (aref res j) (aref array i)))
    res))

(defun main ()
  (declare (optimize (speed 3) (safety 0)))
  (with-open-file (in "/dev/stdin" :element-type +ub+)
    (with-open-file (out "/dev/stdout" :element-type +ub+ :direction :output :if-exists :append)
      (let ((i-buf (make-array +buffer-size+ :element-type +ub+))
            (o-buf (make-array +buffer-size+ :element-type +ub+))
            (chunks nil))
        (declare (list chunks))
        (flet ((flush-chunks ()
                 (let ((j 0) (k 0))
                   (declare (fixnum j k))
                   ;; reverse, complement, filter out old newlines, put newlines back in after 60 chars,
                   ;; buffer output, and write buffers.  all in one go :(
                   (loop for chunk in chunks
                         do (loop for i from (1- (length (the (simple-array #.+ub+) chunk))) downto 0
                                  for byte = (aref chunk i)
                                  unless (= byte +newline+)
				  do (setf (aref o-buf k) (aref +lut+ byte) j (1+ j) k (1+ k))
                                  if (= j 60) do (setf (aref o-buf k) +newline+ j 0 k (1+ k))
                                  if (> k (- +buffer-size+ 2)) do (write-sequence o-buf out :end (shiftf k 0)))
                         finally (when (plusp k)
                                   (write-sequence o-buf out :end k))
                                 (when (plusp j)
                                   (write-byte +newline+ out)))
                   (setf chunks nil))))
          (prog ((start 0) (end 0))
           read-chunk
             (setf end (read-sequence i-buf in))
             (when (zerop end) (go end-of-input))
           parse-chunk
             (let ((bod (position #.(char-code #\>) i-buf :start start :end end)))
               (cond ((numberp bod)
                      (push (slice i-buf start bod) chunks)
                      (setf start bod)
                      (flush-chunks)
                      ;; parse description
                      (loop for eod = (position +newline+ i-buf :start start :end end)
                            do (cond ((numberp eod)
                                      (write-sequence i-buf out :start start :end (incf eod))
                                      (setf start eod)
                                      (go parse-chunk))
                                     (t (write-sequence i-buf out :start start :end end)
                                        (setf start 0)
                                        (setf end (read-sequence i-buf in))))))
                     (t (push (slice i-buf start end) chunks)
                        (setf start 0)
                        (go read-chunk))))
           end-of-input
             (flush-chunks)))))))