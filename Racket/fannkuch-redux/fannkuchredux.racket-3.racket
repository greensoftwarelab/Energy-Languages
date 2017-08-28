#lang racket/base

;;; The Computer Language Benchmarks Game
;;; http://benchmarksgame.alioth.debian.org/

;; Written by Dima Dorfman, 2004
;; Slightly improved by Sven Hartrumpf, 2005-2006
;; Ever-so-slightly tweaked for MzScheme by Brent Fulgham
;; PLT-ized for v4.0 by Matthew
;; Updated by Danny Yoo and Matthias Felleisen
;; Optimized and Parallelized by Gustavo Massaccesi, 2013

(require (for-syntax (only-in racket/base 
                              lambda 
                              syntax 
                              syntax-case
                              make-rename-transformer
                              #%app)))
(require racket/unsafe/ops
         racket/future)
(require racket/cmdline)

(define-sequence-syntax unsafe-in-fxrange 
  (lambda () #'in-fxrange/proc) 
  (lambda (stx) 
    (syntax-case stx () 
      [[(d) (_ nat)] 
       #'[(d) 
          (:do-in ([(n) nat])
                  #f 
                  ([i 0])
                  (unsafe-fx< i n)
                  ([(d) i])
                  #t
                  #t
                  [(unsafe-fx+ 1 i)])]]))) 

(define (unsafe-in-fxrange/proc n) 
  (make-do-sequence (lambda () (values (lambda (x) x)
                                       (lambda (x) (unsafe-fx+ 1 x))
                                       0
                                       (lambda (x) (unsafe-fx< x n))
                                       #f
                                       #f)))) 


(define-syntax-rule (define/0st-bool (name arg0 rest ...) body ...)
  (begin
    (define-syntax-rule (name arg0/v rest ...)
      (if arg0/v (name/t rest ...) (name/f rest ...)))
    (define (name/t rest ...) (let ([arg0 #t]) body ...))
    (define (name/f rest ...) (let ([arg0 #f]) body ...))
    ))

(define (fannkuch n)
  (let ([future-slices (for/list ([k (unsafe-in-fxrange n)])
                         (let ([pi (for/vector #:length n ([i (unsafe-in-fxrange n)])
                                     (unsafe-fxmodulo (unsafe-fx+ i k) n))]
                               [tmp (make-vector n)]
                               [count (make-vector (unsafe-fx- n 1))]
                               [retval (mcons #f #f)])
                           (future (lambda () 
                                     (fannkuch/slice n pi tmp count retval)))))])
    (for/fold ([flips 0] [checksum 0]) ([f (in-list future-slices)])
      (let-values ([(flips2 checksum2) (touch f)])
          (values (unsafe-fxmax flips flips2) (unsafe-fx+ checksum checksum2))))))
      

(define (fannkuch/slice n pi tmp count retval)
  (define/0st-bool (loop even-parity? flips r checksum n-1 pi tmp count retval)
    (for ([i (unsafe-in-fxrange r)])
      (unsafe-vector-set! count i (unsafe-fx+ 1 i)))
    (let* ([next-flips (count-flips pi tmp n)]
           [flips2 (unsafe-fxmax next-flips flips)]
           [next-checksum (if even-parity? 
                              (unsafe-fx+ checksum  next-flips)
                              (unsafe-fx- checksum next-flips))])
      (let loop2 ([r 1])
        (if (unsafe-fx= r n-1)
            (values flips2 next-checksum)
            (let ([perm0 (unsafe-vector-ref pi 0)])
              (for ([i (unsafe-in-fxrange r)])
                (unsafe-vector-set! pi i (unsafe-vector-ref pi (unsafe-fx+ 1 i))))
              (unsafe-vector-set! pi r perm0)
              (unsafe-vector-set! count r (unsafe-fx- (unsafe-vector-ref count r) 1))
              (if (unsafe-fx= (unsafe-vector-ref count r) 0)
                  (loop2 (unsafe-fx+ 1 r))
                  (loop (not even-parity?)
                        flips2
                        r
                        next-checksum
                        n-1
                        pi
                        tmp
                        count
                        retval)))))))
  (loop #t 0 (unsafe-fx- n 1) 0 (unsafe-fx- n 1) pi tmp count retval))


(define (count-flips pi rho n)
  (vector-copy-all! rho pi n)
  (let loop ([k 0])
    (if (unsafe-fx= (unsafe-vector-ref rho 0) 0)
        k
        (let loop2 ([i 0]
                    [j (unsafe-vector-ref rho 0)])
          (if (unsafe-fx> j i)
              (begin 
                (vector-swap! rho i j)
                (loop2 (unsafe-fx+ 1 i) (unsafe-fx- j 1)))
              (loop (unsafe-fx+ 1 k)))))))

(define (vector-copy-all! dest src n) 
 (for ([i (unsafe-in-fxrange n)])
   (unsafe-vector-set! dest i (unsafe-vector-ref src i))))

(define-syntax-rule (vector-swap! v i j)
  (let ([t (unsafe-vector-ref v i)])
    (unsafe-vector-set! v i (unsafe-vector-ref v j))
    (unsafe-vector-set! v j t)))

; assume that n>=3
(command-line #:args (n)
              (define-values (answer checksum)
                (fannkuch (string->number n)))
              (printf "~a\nPfannkuchen(~a) = ~a\n" 
                      checksum
                      n 
                      answer))