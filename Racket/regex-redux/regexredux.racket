#lang racket/base

;;; The Computer Language Benchmarks Game
;;; http://benchmarksgame.alioth.debian.org/

;;; based on a version by by Anthony Borla
;;; regex-dna program contributed by Matthew Flatt
;;; converted from regex-dna program


(require racket/port)

;; -------------------------------

(define VARIANTS
  '(#"agggtaaa|tttaccct" #"[cgt]gggtaaa|tttaccc[acg]" #"a[act]ggtaaa|tttacc[agt]t"
    #"ag[act]gtaaa|tttac[agt]ct" #"agg[act]taaa|ttta[agt]cct" #"aggg[acg]aaa|ttt[cgt]ccct"
    #"agggt[cgt]aa|tt[acg]accct" #"agggta[cgt]a|t[acg]taccct" #"agggtaa[cgt]|[acg]ttaccct"))


(define IUBS
  '((#"tHa[Nt]" #"<4>") (#"aND|caN|Ha[DS]|WaS" #"<3>") (#"a[NSt]|BY" #"<2>")
    (#"<[^>]*>" #"|") (#"\\|[^|][^|]*\\|" #"-")))

;; -------------------------------

(define (ci-byte-regexp s)
  (byte-regexp (bytes-append #"(?i:" s #")")))

;; -------------------------------

(define (match-count str rx offset cnt)
  (let ([m (regexp-match-positions rx str offset)])
    (if m
        (match-count str rx (cdar m) (add1 cnt))
        cnt)))

;; -------------------------------

;; Load sequence and record its length
(let* ([orig (port->bytes)]
       [filtered (regexp-replace* #rx#"(?:>.*?\n)|\n" orig #"")])

  ;; Perform regexp counts
  (for ([i (in-list VARIANTS)])
    (printf "~a ~a\n" i (match-count filtered (ci-byte-regexp i) 0 0)))

  ;; Perform regexp replacements, and record sequence length
  (let ([replaced
         (for/fold ([sequence filtered]) ([IUB IUBS])
           (regexp-replace* (byte-regexp (car IUB)) sequence (cadr IUB)))])
    ;; Print statistics
    (printf "\n~a\n~a\n~a\n" 
            (bytes-length orig)
            (bytes-length filtered)
            (bytes-length replaced))))