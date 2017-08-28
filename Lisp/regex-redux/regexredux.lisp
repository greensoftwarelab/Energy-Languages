;;; The Computer Language Benchmarks Game
;;; http://benchmarksgame.alioth.debian.org/
;;;
;;; regex-dna program contributed by: Witali Kusnezow 2009-03-02
;;; converted from regex-dna program

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf)
  (require :cl-ppcre)

#+sb-thread
(progn
  (define-alien-routine sysconf long (name int))
  (use-package :sb-thread)))

(eval-when (:compile-toplevel)
(setf cl-ppcre:*regex-char-code-limit* 128))

(defconstant  +regex-list+
  '("agggtaaa|tttaccct"
    "[cgt]gggtaaa|tttaccc[acg]"
    "a[act]ggtaaa|tttacc[agt]t"
    "ag[act]gtaaa|tttac[agt]ct"
    "agg[act]taaa|ttta[agt]cct"
    "aggg[acg]aaa|ttt[cgt]ccct"
    "agggt[cgt]aa|tt[acg]accct"
    "agggta[cgt]a|t[acg]taccct"
    "agggtaa[cgt]|[acg]ttaccct"))

(defconstant  +alternatives+
  '(("tHa[Nt]" "<4>")  ("aND|caN|Ha[DS]|WaS" "<3>")
    ("a[NSt]|BY" "<2>")  ("<[^>]*>" "|")
    ("\\|[^|][^|]*\\|" "-")))

#+sb-thread
(progn
  (defconstant  +cpu-count+ (sysconf 84))
  (defvar *mutex* (make-mutex))
  (defvar *aux-mutex* (make-mutex))

  (defmacro bg  (&body body) `(make-thread (lambda () ,@body)))
  (defmacro join-all (&body body)
	`(mapcar
	  #'join-thread
	  (loop for item in (list ,@body)
		 append (if (consp item) item (list item))))))

(defun read-all
    (stream &aux (buf-size (* 1024 1024))
     (size 0)
     (buf-list
      (loop
         for buf = (make-string buf-size :element-type 'base-char)
         for len = (read-sequence buf stream)
         do (incf size len)
         collect (if (< len buf-size) (subseq buf 0 len) buf)
         while (= len buf-size))))
  (declare (type fixnum size))
  (loop with res-string = (make-string size :element-type 'base-char)
     with i of-type fixnum = 0
     for str in buf-list
     do (setf (subseq res-string i) (the simple-base-string str))
     (incf i (length (the simple-base-string str)))
     finally (return res-string)))

(defun length-to-replace (match)
  (loop for x in match
     sum (- (the fixnum (cdr x))
            (the fixnum (car x))) of-type fixnum))

(defun replace-aux
    (match replacement target-string result-string
     &key (match-begin 0) (match-end -1)
     (match-length (length match))
     &aux
     (len (length replacement))
     (first-match (if (zerop match-begin) '(0 . 0) (nth (1- match-begin) match)))
     (target-start (cdr first-match))
     (result-start (+ (the fixnum (* len match-begin))
                    (- target-start
                       (the fixnum (length-to-replace (subseq match 0 match-begin)))))))
  (declare (type fixnum match-begin match-end match-length target-start result-start len)
           (type list match)
           (type simple-base-string result-string target-string)
           (type vector replacement))
  (loop with (i j) of-type fixnum = (list result-start target-start)
     with mmatch = (if (> match-begin match-end)
                       match (subseq match match-begin match-end))
     for pair in mmatch
     do (setf (subseq result-string i) (subseq target-string j (car pair))
              i (+ i (- (the fixnum (car pair)) j))
              (subseq result-string i) replacement
              j (cdr pair)
              i (+ i len))
     finally (if (or (minusp match-end) (<= match-length match-end))
                 (setf (subseq result-string i ) (subseq target-string j))))
  nil)

#+sb-thread
(defun parts
    (parts-num len
     &aux
     (ranges (loop with (step rest) of-type fixnum =  (multiple-value-list (floor len parts-num))
                with i of-type fixnum = 0 while (< i len)
                collect i into res of-type fixnum
                do (incf i step)(if (plusp rest) (progn (incf i) (decf rest)) )
                finally (return (append res (list len))))
             ))
  (declare (type fixnum len parts-num)
           (type list ranges))
  (mapcar #'cons ranges (subseq ranges 1)))

(defun replace-all
    (regexp replacement target-string
     &aux (rmatch '()) (match '())
     (result-string (make-string 0 :element-type 'base-char)))
  (declare (type simple-base-string result-string target-string)
           (type vector replacement))
  (cl-ppcre:do-scans
      (match-start match-end reg-starts reg-ends regexp target-string nil)
    (push (cons match-start match-end) rmatch))
  (if rmatch
      (progn
        (setf match (reverse rmatch)
              result-string (make-string
                             (+ (- (length target-string)
                                   (length-to-replace match))
                                (the fixnum (* (length replacement)
                                               (length match)))) :element-type 'base-char))
        #-sb-thread
        (replace-aux match replacement target-string result-string)
        #+sb-thread
        (mapcar #'join-thread
                (loop with len of-type fixnum = (length match)
				   with parts-list  = (parts +cpu-count+ len)
                   with current of-type fixnum = 0
                   repeat +cpu-count+
                   collect
					 (bg (let (range)
                           (with-mutex (*mutex*)
                             (setf range (nth current parts-list))
                             (incf current))
                           (replace-aux match replacement target-string result-string
                                        :match-begin (car range) :match-end (cdr range)
                                        :match-length len)))))
        result-string)
      target-string))

(defun main (&optional (stream *standard-input*)
             &aux (sequence (read-all stream))
             (size (length sequence)))
  (declare (type simple-base-string sequence))
  (setf sequence (replace-all ">[^\\n]*\\n|\\n" "" sequence))

  #-sb-thread
  (progn
    (loop for regex in +regex-list+ do
         (format t "~a ~a~%" regex
                 (/ (length
                     (the list
                       (cl-ppcre:all-matches regex sequence))) 2)))
    (format t "~%~a~%~a~%" size (length sequence))
    (loop for pair in +alternatives+ do
         (setf sequence (replace-all  (car pair) (cadr pair) sequence )))
    (format t "~a~%" (length sequence)))
  #+sb-thread
  (let* ((len (length +regex-list+))
         (result (make-list (1+ len))))
    (join-all
	 (loop with idx of-type fixnum = 0
		repeat len
		collect
          (bg (let (reg cur)
                (with-mutex (*aux-mutex*)
                  (setf cur idx reg (nth cur +regex-list+))
                  (incf idx))
              (setf (nth cur result)
                    (format nil "~a ~a" reg
                            (/ (length
                                (the list
                                  (cl-ppcre:all-matches reg sequence))) 2))))))
	 (bg (loop with seq = (copy-seq sequence)
            for pair in +alternatives+ do
              (setf seq (replace-all  (car pair) (cadr pair) seq ))
            finally (setf (nth len result)
                          (format nil "~%~a~%~a~%~a" size (length sequence) (length seq))))))
    (format t "~{~a~%~}" result))
  )

