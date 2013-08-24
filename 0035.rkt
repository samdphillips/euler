#lang racket/base

#|

The number, 197, is called a circular prime because all rotations of the
digits: 197, 971, and 719, are themselves prime.

There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31,
37, 71, 73, 79, and 97.

How many circular primes are there below one million?

|#

(require racket/generator
         racket/sequence)

(define table-size 1000000)

(define table-space
  (add1 (* 16 table-size)))

(define table-space/2
  (quotient table-space 2))

;; greatest-prime :: integer
;; This is the largest prime that we haven't sieved
(define greatest-prime 3)

;; primes :: bytes
;; table of primes.  Starts at 3, and only includes odd numbers.
(define primes (make-bytes table-size #xff))

(define (prime-address n)
  (define bits (sub1 (quotient n 2)))
  (quotient/remainder bits 8))

(define (prime-ref n)
  (define-values (byte shift) (prime-address n))
  (bitwise-bit-set? (bytes-ref primes byte) shift))

(define (unprime! n)
  (when (odd? n)
    (define-values (byte shift) (prime-address n))
    (bytes-set! 
      primes byte (bitwise-and
                    (bytes-ref primes byte)
                    (bitwise-not
                      (arithmetic-shift 1 shift))))))

(define (sieve!)
  (define p greatest-prime)
  (for ([i (in-range (* 2 p) (add1 table-space) p)])
    (unprime! i))

  (let/ec break
    (for ([i (in-range (+ 2 p) (add1 table-space) 2)])
      (when (prime-ref i)
        (set! greatest-prime i)
        (break)))))

(define (prime? n)
  (cond [(<= n 1) #f]
        [(even? n) (= 2 n)]
        [(= n greatest-prime) #t]
        [(> n table-space) (error 'prime? "not enough primes")]
        [(or (< n greatest-prime)
             (> greatest-prime table-space/2))
         (prime-ref n)]
        [else
          (sieve!)
          (prime? n)]))

(define (in-primes)
  (sequence-filter prime? 1000000))

(define (igits b)
  (define (rec n [a null])
    (cond [(zero? n) a]
          [else
            (define-values (q r) (quotient/remainder n b))
            (rec q (cons r a))]))
  rec)

(define digits (igits 10))

(define (build-int n*)
  (for/fold ([v 0]) ([n n*])
    (+ n (* v 10))))

(define (rotate-list n*)
  (define (rotate n*)
    (append (cdr n*) (list (car n*))))

  (in-generator
    (define (generate n* i)
      (unless (zero? i)
        (yield n*)
        (define r (rotate n*))
        (generate r (sub1 i))))

    (generate n* (length n*))))

(define (rotations n)
  (sequence-map build-int (rotate-list (digits n))))

(define (rotations-prime? n)
  (for/and ([p (rotations n)])
    (prime? p)))

(define (solve)
  (sequence-count rotations-prime? (in-primes)))

