;;   The Computer Language Benchmarks Game
;;   http://benchmarksgame.alioth.debian.org/
;;   contributed by Currell Berry based on Java submission #1
;;
;; Note I think you could probably get further performance gains by rebasing the 
;; implementation on Java #6, which is similar to java #1 but does not rely on 
;; the external library it.unimi.dsi.fastutil my testing (once I had already translated
;; Java #1 to lisp) shows that a great deal of Java #1's performance comes from it
;; relying on a specialized hash table fastutil.longs.Long2IntOpenHashMap. 

;(require :alexandria)
(defpackage :knucleotide
  (:use :cl))

(in-package :knucleotide)

(declaim (optimize (speed 3) (safety 0) (space 0) (debug 0)))

;; simple thread runner implementation
;; we have a semaphore
(defparameter *my-tr-available-thread-semaphore* nil)
;; each time a thread finishes up, we increment this semaphore
;; the main thread waits on the semaphore, whenever it goes above 0 it finds
;; another task if one is available and starts it up

(defparameter *my-tr-task-remaining-count* 0)
(declaim (type fixnum *my-tr-task-remaining-count*))
(defparameter *my-tr-completed-task-mutex* nil)
(defparameter *my-tr-task-completed-cv* nil)

;;writing to status and result should only be done in my-tr-run
(defstruct my-task
  (mylambda) ; the thing to run
  (status nil) ; nil or t
  (result nil))

(defparameter *my-task-list* #())
(declaim (type vector *my-task-list*))

;; must have set up populated *my-task-list* first
;; each time a thread becomes available, then we run the next task
(defun my-tr-run (threadcount)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type fixnum threadcount))
  (setf *my-tr-available-thread-semaphore* (sb-thread:make-semaphore :count threadcount))
  (setf *my-tr-completed-task-mutex* (sb-thread:make-mutex)) 
  (setf *my-tr-task-remaining-count* (length *my-task-list*)) 
  (setf *my-tr-task-completed-cv* (sb-thread:make-waitqueue)) 
  (loop for taskindex from 0 below (length *my-task-list*) do
       (sb-thread:wait-on-semaphore *my-tr-available-thread-semaphore*)
       (let ((thetask (elt *my-task-list* taskindex)))
         (sb-thread:make-thread (lambda () (let ((results
                                                  (funcall (my-task-mylambda thetask))))
                                             (setf (my-task-result thetask) results)
                                             (setf (my-task-status thetask) t)
                                             (sb-thread:signal-semaphore *my-tr-available-thread-semaphore*)
                                             (sb-thread:with-mutex (*my-tr-completed-task-mutex*)
                                               (decf *my-tr-task-remaining-count*)
                                               (sb-thread:condition-notify *my-tr-task-completed-cv*)
                                               )
                                             )))))
  (loop
     (sb-thread:with-mutex (*my-tr-completed-task-mutex*)
       (if (eql *my-tr-task-remaining-count* 0)
           (return)
           (sb-thread:condition-wait *my-tr-task-completed-cv* *my-tr-completed-task-mutex*)))))

(defconstant CODES  #(-1 0 -1 1 3 -1 -1 2)) 
(defconstant NUCLEOTIDES #(#\A #\C #\G #\T)) 

(defun hash-function (x)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type (unsigned-byte 64) x))
  x)

(defstruct result
  (outmap (make-hash-table
           :test 'eql
           :hash-function #'hash-function
           :rehash-size 2.0
           :rehash-threshold 0.7))
  (keylength 0))

(defun create-fragment-tasks (sequence mfragment-lengths)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type (simple-array (unsigned-byte 8) (*)) sequence))
  (let ((tasks (make-array 46 :fill-pointer 0)))
    (loop for fragmentLength in mfragment-lengths do
         (loop for i of-type (unsigned-byte 32) from 0 below fragmentLength do
              (let ((offset i)
                    (mfragmentlength fragmentLength))
                ;;(format t "fragmentLength: ~a~%" fragmentLength)
                (vector-push-extend
                 (make-my-task :mylambda
                               (lambda ()
                                 (declare (type (unsigned-byte 32) mfragmentlength)
                                          (type (unsigned-byte 32) offset))
                                 ;(format t "offset: ~a, fragmentLength: ~a~%" offset mfragmentlength)
                                 (create-fragment-map sequence offset mfragmentlength)
                                 ))
                 tasks))))
    tasks))

(defun create-fragment-map (sequence offset fragmentLength)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type (simple-array (unsigned-byte 8) (*)) sequence)
           (type (unsigned-byte 32) offset)
           (type (unsigned-byte 32) fragmentLength))
  (let* ((res (make-result :keylength fragmentLength))
         (mymap (result-outmap res))
         (lastIndex (+ (- (length sequence) fragmentLength) 1)))
    (declare (type (unsigned-byte 32) lastIndex))
    (loop
       for index of-type (unsigned-byte 32) from offset below lastIndex by fragmentLength
       do
         (let* ((key (get-key sequence index fragmentLength))
                (value (gethash key mymap 0)))
           (declare (type (unsigned-byte 64) key)
                    (type (unsigned-byte 32) value))
           (setf (gethash key mymap 0) (the (unsigned-byte 32) (+ value 1)))))
    res))

(defun sum-two-maps (result1 result2)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (loop for key2 of-type (unsigned-byte 64) being the hash-keys  of (result-outmap result2)
     using (hash-value value2) do
       (setf (gethash key2 (result-outmap result1)) (+ (the (unsigned-byte 32) (gethash key2 (result-outmap result1) 0)) (the (unsigned-byte 32) value2))))
  result1
  )

(defun write-frequencies (totalCount frequencies)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((freq (make-array (hash-table-count (result-outmap frequencies)) :fill-pointer 0 :element-type 'cons)))
    (loop for key being the hash-keys of (result-outmap frequencies)
       using (hash-value cnt) do
         (let ((nentry (cons (key-to-string key (result-keylength frequencies)) cnt)))
           (vector-push-extend nentry freq)))
    (sort freq (lambda (x y) (> (cdr x) (cdr y))))
    (let ((outstr
           (apply #'concatenate 
                  (append (list 'string)
                          (loop for index from 0 below (length freq)
                                           for (key . value) = (elt freq index)
                                           collect 
                                             (format nil "~a ~,3f~%" key (/ (* value 100.0) totalCount)))))))
      outstr)))

(defun write-count (tasks nucleotideFragment)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((key (to-codes-new (map '(vector (unsigned-byte 8)) #'char-code nucleotideFragment) (length nucleotideFragment)) )
         (k (get-key key 0 (length nucleotideFragment)))
         (count 0))
    (loop for task across tasks 
       for result = (my-task-result task) do
         (if (eql (result-keylength result) (length nucleotideFragment))
             (setf count (+ count (gethash k (result-outmap result) 0)))))
    (format nil "~a~c~a~%" count #\tab nucleotideFragment)))

(defun key-to-string (key length)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type (unsigned-byte 64) key)
           (type (unsigned-byte 32) length))
  (let ((res (make-string length)))
    (loop for i from 0 below length do
         (setf (elt res (- length i 1)) (elt NUCLEOTIDES (logand key #x3)))
         (setf key (ash key -2)))
    res))

(defun get-key (arr offset length)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type (simple-array (unsigned-byte 8)) arr)
           (type (unsigned-byte 32) offset)
           (type (unsigned-byte 32) length))
  (let ((key 0))
    (declare (type (unsigned-byte 64) key))
    (loop for i of-type (unsigned-byte 32) from offset below (+ offset length) do
         (setf key (the fixnum (+ (the fixnum (* key 4)) (the fixnum (elt arr i))))))
    key))

(defun to-codes-new (sequence length)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type (simple-array (unsigned-byte 8)) sequence)
           (type (unsigned-byte 32) length)
           )
  (let ((result (make-array length :element-type '(unsigned-byte 8))))
    (loop for i of-type (unsigned-byte 32) from 0 below length do
         (setf (elt result i) (elt CODES (logand (elt sequence i) #x7))))
    result))

(defun read-in-data-new (pistream)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((bytes (make-array 1048576 :element-type '(unsigned-byte 8)))
        (position 0))
    (declare (type (simple-array (unsigned-byte 8)) bytes)
             (type (unsigned-byte 32) position))
    (with-open-stream  (istream pistream)
      (loop for line = (read-line istream nil :eof) ;first, get to dna sequence three
         until (or (string-equal ">THREE" (subseq line 0 6))  (eql line :eof)))

      (loop for line = (read-line istream nil :eof)
         while (not (or (eql line :eof) (eql (elt line 0) #\>))) do
           (if (> (+ position (length line)) (length bytes)) ;;then we need to grow the array
               (let ((newbytes (make-array (* 2 (length bytes)) :element-type '(unsigned-byte 8))))
                 (loop for i from 0 below (length bytes) do
                      (setf (elt newbytes i) (elt bytes i)))
                 (setf bytes newbytes)))
           (loop for i from 0 below (length line) do
                (setf (elt bytes position) (char-code (elt line i)))
                (incf position))))
           (to-codes-new bytes position)))

(defconstant FRAGMENT-LENGTHS (list 1 2 3 4 6 12 18))
(defconstant NUCLEOTIDE-FRAGMENTS (list "GGT" "GGTA" "GGTATT" "GGTATTTTAATT"
                                           "GGTATTTTAATTTATAGT" ))

(defun get-input-stream ()
 (open #p"/dev/stdin" :external-format :iso-8859-1)
;(open "knucleotide-input-example.txt")
;(open "fasta-small.txt"  :external-format :iso-8859-1)
;(open "fasta.txt"  :external-format :iso-8859-1)
)

(defun main ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((pistream (get-input-stream))
         (msequence (read-in-data-new pistream)))
    (setf *my-task-list* (create-fragment-tasks msequence FRAGMENT-LENGTHS))
    (my-tr-run 4)
    (format t "~a~%" (write-frequencies (length msequence) (my-task-result (elt *my-task-list* 0))))
    
    (format t "~a~%" (write-frequencies (- (length msequence) 1) (sum-two-maps
                                                       (my-task-result (elt *my-task-list* 1))
                                                       (my-task-result (elt *my-task-list* 2))
                                                                  )))
    (loop for nucleotide-fragment in NUCLEOTIDE-FRAGMENTS do
         (princ (write-count *my-task-list*  nucleotide-fragment)))
))

(in-package :cl-user)

(defun main ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (knucleotide::main))