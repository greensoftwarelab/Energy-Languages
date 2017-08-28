#lang racket/base
(require racket/fixnum)
(require racket/generator)
(require racket/sequence)
;;;
;;; The Computer Language Benchmarks Game
;;; http://benchmarksgame.alioth.debian.org/

;;; contributed by Matthew Flatt, modified by
;;; modified by James Bergstra

;;; Notes on the implementation: the strategy is to map the DNA letters to the
;;; bytes 0, 1, 2, 3, and then create a hash function that is simply the
;;; concatenation of these two-byte codes. This is handy because the slow part
;;; of this test is building the hash table, and this hash function means that
;;; we can take advantage of overlapping DNA sub-sequences to get a
;;; constant-time hash function (that does not depend on the sequence length).
;;;
;;; The bottleneck in this code seems to be Racket's hash table. The time to
;;; create the last hash table (for the len-18 string) seems to be about half
;;; the runtime of the whole program.

;; Map A->0, C->1, G->2 T->3 (and lowercase too)
(define dna->num
  (let ([tbl (make-bytes 256 255)])
    (for ([ch (in-list (bytes->list #"ACGTacgt"))]
          [ii (in-list '(0 1 2 3 0 1 2 3))])
      (bytes-set! tbl ch ii))
    (lambda (ch) (bytes-ref tbl ch))))

;;; map a hash key back to a string (needed for printing)
(define (unhash key len)
  (let ([rval (make-string len)])
    (sequence-fold
      (lambda (key pos)
        (string-set! rval pos (string-ref "ACGT" (bitwise-and key 3)))
        (arithmetic-shift key -2))
      key
      (in-range len))
    rval))

;;; Ideally this would serve all-counts, but my attempt to do that
;;; was horribly slow.
(define (hashes keylen dna as-codes)
  (generator ()
    (let ([key 0] [ishift (* 2 keylen)] [thresh (sub1 keylen)])
      (for
        ([bb (in-bytes dna)]
         [ii (in-range (bytes-length dna))])
        (set! key (arithmetic-shift (+ key (arithmetic-shift (if as-codes bb (dna->num bb) ) ishift)) -2))
        (if (>= ii thresh) (yield key) #f))
      )))

(define (all-counts keylen dna)
  (let ([table (make-hasheq)]
        [key 0]
        [ishift (* 2 keylen)]
        )
    (for
      ([bb (in-bytes dna)]
       [ii (in-range (bytes-length dna))])
      (set! key (arithmetic-shift (+ key (arithmetic-shift bb ishift)) -2))
      (if (>= ii (- keylen 1)) (hash-set! table key (add1 (hash-ref table key 0))) #f)
      )
    table))

(define (readbuf in foo)
  (let ([s (open-output-bytes)])
    ;; Skip to ">THREE ..."
    (regexp-match #rx#"(?m:^>THREE.*$)" in)
    ;; Copy everything but newlines
    (for ([l (in-bytes-lines in)])
      (write-bytes l s))
    ;; Replace letters with numbers 0, 1, 2, 3
    (let ([actg (get-output-bytes s)])
      (for ([ii (in-range (bytes-length actg))])
           (bytes-set! actg ii (foo (bytes-ref actg ii))))
      actg)))

(define (write-freqs table len)
  (let* ([content (hash-map table cons)]
         [total (exact->inexact (apply + (map cdr content)))])
    (for ([a (sort content > #:key cdr)])
      (printf "~a ~a\n" 
              (unhash (car a) len)
              (real->decimal-string (* 100 (/ (cdr a) total)) 3)))))

(define (write-one-freq table key)
  (let ([cnt (hash-ref table ((hashes (bytes-length key) key #f)) 0)])
    (printf "~a\t~a\n" cnt key)))

(define dna (readbuf (current-input-port) dna->num))

(write-freqs (all-counts 1 dna) 1)
(newline)

(write-freqs (all-counts 2 dna) 2)
(newline)

;; Specific sequences:
(for ([seq '(#"GGT" #"GGTA" #"GGTATT" #"GGTATTTTAATT" #"GGTATTTTAATTTATAGT")]) 
  (write-one-freq (all-counts (bytes-length seq) dna) seq))