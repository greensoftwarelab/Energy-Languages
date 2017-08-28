#lang racket/base

;;; The Computer Language Benchmarks Game
;;; http://benchmarksgame.alioth.debian.org/

;;; Derived from the Chicken variant by Sven Hartrumpf
;;; contributed by Matthew Flatt
;;; *reset*
;;; improved by Phil Nguyen:
;;; - use `cons` instead of struct `node`
;;; - remove the confirmed unneccessary field `val`
;;; - accumulate part of `check`
;;; - use unsafe accessors and fixnum arithmetics
;;; - clean up with `define` instead of nested `let`
;;; - clean up with `for/sum` instead of `for/fold`

(require racket/cmdline)

#;(struct node (left right))
(define node cons)
(require (rename-in racket/unsafe/ops
                    [unsafe-car node-left]
                    [unsafe-cdr node-right]
                    [unsafe-fx+ +]
                    [unsafe-fx- -]
                    [unsafe-fx= =]))

(define (make d)
  (if (= d 0)
      (node #f #f)
      (let ([d2 (- d 1)])
        (node (make d2) (make d2)))))

(define (check t)
  (let sum ([t t] [acc 0])
    (cond [(node-left t) (sum (node-right t) (sum (node-left t) (+ 1 acc)))]
          [else          (+ 1 acc)])))

(define (main n)
  (define min-depth 4)
  (define max-depth (max (+ min-depth 2) n))
  (define stretch-depth (+ max-depth 1))
  (printf "stretch tree of depth ~a\t check: ~a\n" stretch-depth (check (make stretch-depth)))
  (define long-lived-tree (make max-depth))
  (for ([d (in-range 4 (add1 max-depth) 2)])
    (define iterations (arithmetic-shift 1 (+ (- max-depth d) min-depth)))
    (printf "~a\t trees of depth ~a\t check: ~a\n"
            iterations
            d
            (for/sum ([_ (in-range iterations)])
              (check (make d)))))
  (printf "long lived tree of depth ~a\t check: ~a\n" max-depth (check long-lived-tree)))

(command-line #:args (n) 
              (main (string->number n)))