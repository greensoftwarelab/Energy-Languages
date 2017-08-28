;;;   The Computer Language Benchmarks Game
;;;   http://benchmarksgame.alioth.debian.org/
;;;
;;;   by Jon Smith from GCC Mandelbrot version
;;;   does computation nearly the same as the GCC #4 version of the code.  
;;;
;;;   to compile:
;;sbcl --load mandelbrot.lisp --eval "(save-lisp-and-die \"mandelbrot.core\" :purify t :toplevel (lambda () (vops::main) (quit)))"
;;to run
;;sbcl --noinform --core mandelbrot.core 16000


(setf sb-ext:*efficiency-note-cost-threshold* 1)
(setf sb-ext:*efficiency-note-limit* 8)

(setf sb-ext:*inline-expansion-limit* 1000)
(sb-int:set-floating-point-modes :traps (list :divide-by-zero))

(defpackage :vops
  (:use :cl))

(in-package :sb-vm)
(declaim (optimize (speed 3) (safety 0) (space 0) (debug 0)))

(eval-when (:load-toplevel :compile-toplevel :execute)
  #+x86-64(handler-bind ((simple-error (lambda (condition)
					 condition
					 (invoke-restart 'continue))))
          
	    (sb-c::defknown complex-double-float/sse-* ((complex double-float) (complex double-float))
		(complex double-float))
	    (sb-c::defknown cmple-movmskpd ((complex double-float) (complex double-float))
		(unsigned-byte 32))

	    (define-vop (complex-double-float/sse-*)
	      (:policy :fast-safe)
	      (:translate complex-double-float/sse-*)
	      (:args (x :scs (complex-double-reg))
		     (y :scs (complex-double-reg)))
	      (:arg-types complex-double-float complex-double-float)
	      (:results (r :scs (complex-double-reg)))
	      (:result-types complex-double-float)
	      (:generator 2
			  (flet ((get-constant (tn)
				   (register-inline-constant
				    (tn-value tn))))
			    (cond ((location= x r)
				   (inst mulpd x y))
				  ((location= y r)
				   (inst mulpd y x))
				  ((not (location= r y))
				   (if (sc-is x fp-complex-double-immediate)
				       (inst movapd r (get-constant x))
				       (move r x))
				   (inst mulpd r y))))))

	    (macrolet ((generate (opinst test movmsk constant-sc load-inst)
			 `(flet ((get-constant (tn)
				   (register-inline-constant
				    ,@(and (eq constant-sc 'fp-single-immediate)
					   '(:aligned))
				    (tn-value tn))))
			    (declare (ignorable #'get-constant))
			    (cond
			      ((location= x r)
			       (when (sc-is y ,constant-sc)
				 (setf y (get-constant y)))
			       (inst ,opinst ,test x y)
			       (inst ,movmsk r x))
			      ((not (location= r y))
			       (if (sc-is x ,constant-sc)
				   (inst ,load-inst r (get-constant x))
				   (move tmp x))
			       (when (sc-is y ,constant-sc)
				 (setf y (get-constant y)))
			       (inst ,opinst ,test tmp y)
			       (inst ,movmsk r tmp))
			      (t
			       (if (sc-is x ,constant-sc)
				   (inst ,load-inst tmp (get-constant x))
				   (move tmp x))
			       (when (sc-is y ,constant-sc)
				 (setf y (get-constant y)))
			       (inst ,opinst ,test tmp y)
			       (inst ,movmsk r tmp)
			       ))))
		       (frob (test cdinst cdname cdcost)
			 `(progn
			    (define-vop (,cdname)
			      (:translate ,cdname)
			      (:policy :fast-safe)
			      (:args (x :scs (complex-double-reg))
				     (y :scs (complex-double-reg)))
			      (:arg-types complex-double-float complex-double-float)
			      (:results (r :scs (unsigned-reg)))
			      (:result-types unsigned-num)
			      (:temporary (:sc complex-double-reg) tmp)
			      (:info)
			      (:generator ,cdcost
					  (generate ,cdinst ,test movmskpd
						    fp-complex-double-immediate movapd))))))
	      (frob :le cmppd cmple-movmskpd 3)))

  #+x86-64(declaim (inline complex-double-float/sse-*))
  #+x86-64(declaim (inline cmple-movmskpd))
  (declaim (inline vops::calc-row vops::main))
  #+x86-64(declaim (inline vops::complex-double-float/sse-*))
  #+x86-64(declaim (inline vops::cmple-movmskpd)))

#+x86-64(defun vops::complex-double-float/sse-* (numbera numberb)
	    (declare (type (complex double-float) numbera numberb)
		     (optimize (speed 3) (safety 0) (space 0) (debug 0)))
	    (complex-double-float/sse-* numbera numberb))

#+x86-64(defun vops::cmple-movmskpd (numbera numberb)
	    (declare (type (complex double-float) numbera numberb)
		     (optimize (speed 3) (safety 0) (space 0) (debug 0)))
	    (cmple-movmskpd numbera numberb))

(in-package :vops)
(declaim (optimize (speed 3) (safety 0) (space 0) (debug 0)))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defmacro cbyte (form)
    (cond ((stringp form)
	   (map '(simple-array (unsigned-byte 8) (*)) #'char-code form))
	  ((characterp form)
	   (char-code form))
	  ((listp form)
	   `(map '(simple-array (unsigned-byte 8) (*)) #'char-code ,form))
	  )))

(defconstant +zero+ (complex 0.0d0 0.0d0))
(defconstant +four+ (complex 4.0d0 4.0d0))
(defconstant +workers+ 8)

#+x86-64(defmacro escapes? (n two-pixels  crv civ)
	  (let ((escaped (gensym "escaped"))
		(temp (gensym "temp"))
		(temp2 (gensym "temp2"))
		(zrv (gensym))
		(ziv (gensym))
		(trv (gensym))
		(tiv (gensym)))
	    `(let ((,zrv vops::+zero+) (,ziv vops::+zero+) (,trv vops::+zero+) (,tiv vops::+zero+))
	       (block ,escaped 
		 ,@(nreverse (loop for i from 0 below n
				collecting	   
				`(progn
				   (let* ((,temp (complex-double-float/sse-* ,zrv ,ziv)))
				     (setf ,zrv (+ (- ,trv ,tiv) ,crv))
				     (setf ,trv (complex-double-float/sse-* ,zrv ,zrv))
				     (setf ,ziv (+ ,temp ,temp ,civ))
				     (setf ,tiv (complex-double-float/sse-* ,ziv ,ziv)))
				   (let ((,temp2 (+ ,trv ,tiv)))
				     (setf ,two-pixels (cmple-movmskpd ,temp2 ,vops::+four+)))
				   (when (= ,two-pixels 0)
				     (return-from ,escaped)))
				))))))

#+x86(defmacro escapes? (n two-pixels crv civ)
       (let ((escaped (gensym "escaped"))
	     (zrv1 (gensym)) 
	     (zrv2 (gensym))
	     (ziv1 (gensym))
	     (ziv2 (gensym))
	     (trv1 (gensym))
	     (trv2 (gensym))
	     (tiv1 (gensym))
	     (tiv2 (gensym))
	     (crv1 (gensym))
	     (crv2 (gensym))
	     (civ1 (gensym))
	     (civ2 (gensym))
	     (temp (gensym))
	     (temp2 (gensym)))
	 `(let ((,zrv1 0.0d0)
		(,zrv2 0.0d0)
		(,ziv1 0.0d0)
		(,ziv2 0.0d0)
		(,trv1 0.0d0)
		(,trv2 0.0d0)
		(,tiv1 0.0d0)
		(,tiv2 0.0d0)
		(,crv1 (realpart ,crv))
		(,crv2 (imagpart ,crv))
		(,civ1 (realpart ,civ))
		(,civ2 (imagpart ,civ)))
	    (declare (type double-float
			   ,zrv1 ,zrv2 ,ziv1
			   ,ziv2 ,trv1 ,trv2
			   ,tiv1 ,tiv2 ,crv1
			   ,crv2 ,civ1 ,civ2))
	    (setf ,two-pixels 3)
	    (block ,escaped
	      ,@(nreverse (loop for i from 0 below n
			     collecting
			     `(progn 
				(let ((,temp (* ,zrv1 ,ziv1)))
				  (setf ,zrv1 (+ (- ,trv1 ,tiv1) ,crv1))
				  (setf ,trv1 (* ,zrv1 ,zrv1))
				  (setf ,ziv1 (+ ,temp ,temp ,civ1))
				  (setf ,tiv1 (* ,ziv1 ,ziv1)))
				(let ((,temp2 (+ ,trv1 ,tiv1)))
				  (when (> ,temp2 4.0d0)
				    (setf ,two-pixels (logxor ,two-pixels 1))
				    (return-from ,escaped)))))))
	    (block ,escaped
	      ,@(nreverse (loop for i from 0 below n
			     collecting
			     `(progn 
				(let ((,temp (* ,zrv2 ,ziv2)))
				  (setf ,zrv2 (+ (- ,trv2 ,tiv2) ,crv2))
				  (setf ,trv2 (* ,zrv2 ,zrv2))
				  (setf ,ziv2 (+ ,temp ,temp ,civ2))
				  (setf ,tiv2 (* ,ziv2 ,ziv2)))
				(let ((,temp2 (+ ,trv2 ,tiv2)))
				  (when (> ,temp2 4.0d0)
				    (setf ,two-pixels (logxor ,two-pixels 2))
				    (return-from ,escaped))))))))))


(defun vops::calc-row (y n bitmap bytes-per-row crvs inverse-h)
  (declare (type fixnum y N bytes-per-row)
	   (type double-float inverse-h)
	   (type (simple-array (unsigned-byte 8) (*)) bitmap)
	   (type (simple-array (complex double-float) (*)) crvs))
  (let ((index (the fixnum (* bytes-per-row y)))
	(civ-init (complex
		   (the double-float (- (* y inverse-h) 1.0d0))
		   (the double-float (- (* y inverse-h) 1.0d0))))
	(bit 0)
	(code 0))
    (declare (type fixnum index bit)
	     (type (unsigned-byte 8) code))
    (loop for x of-type fixnum from 0 below N by 2
       do
       (let ((two-pixels 3))
	 (let ((crv (aref crvs (ash x -1)))
	       (civ civ-init))
	   (declare (type (complex double-float)
			  crv civ)
		    (type fixnum two-pixels))
	   (escapes? 50 two-pixels crv civ))
	 (setf code (logior (ash code 2) two-pixels))

	 (when (= (incf bit) 4)
	   (setf (aref bitmap index) code
		 bit 0
		 code 0)
	   (incf index))))
    (values)))

  (defun vops::main ()
    (let* ((args sb-ext:*posix-argv*)
	   (n (parse-integer (or (second args) "1000")))
	   (bytes-per-row (ash (the fixnum (+ N 7)) -3))
	   (inverse-w (/ 2.0d0 (the fixnum (ash bytes-per-row 3))))
	   (inverse-h (/ 2.0d0 N))
	   (crvs 
	    (make-array (ash N -1) :element-type '(complex double-float))))
      (declare (type fixnum N bytes-per-row)
	       (type double-float inverse-h inverse-w)
	       (type (simple-array (complex double-float) (*)) crvs))
      (let ((bitmap 
	     (make-array (* bytes-per-row N)
			 :initial-element 0
			 :element-type '(unsigned-byte 8))))
	(declare (type (simple-array (unsigned-byte 8) (*)) bitmap))
	(loop for i of-type fixnum from 0 below N by 2 
	   do (setf (aref crvs (ash i -1))
		    (complex (- (* (+ i 1.0d0) inverse-w) 1.5d0)
			     (- (* i inverse-w) 1.5d0))))
      
	#-sb-thread
	(loop for y from 0 below N
	   do (calc-row y n bitmap bytes-per-row crvs inverse-h))
	#+sb-thread
	(let ((ndiv (the fixnum (truncate n +workers+))))
	  (mapcar #'sb-thread:join-thread  
		  (loop for i from 0 below +workers+
		     collecting (sb-thread:make-thread 
				 (let ((start (* ndiv i))
				       (end (* ndiv (+ i 1))))
				   (lambda () (loop for y from start  to end 
						 do (calc-row y n bitmap bytes-per-row crvs inverse-h))))))))
	(with-open-file (stream #p"/dev/stdout"
				:direction :output
				:if-exists :append
				:element-type '(unsigned-byte 8))
	  (write-sequence (cbyte (format nil "P4~%~d ~d~%" n n)) stream)
	  (write-sequence bitmap stream)))
      (values)))

(declaim (inline ))

(in-package :cl-user)

(defun main ()
  (vops::main))

