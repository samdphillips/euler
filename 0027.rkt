#lang racket/base

#|

Euler discovered the remarkable quadratic formula:

n^2 + n + 41

It turns out that the formula will produce 40 primes for the consecutive
values n = 0 to 39.  However, when n = 40, 40^2 + 40 + 41 = 40(40 + 1) +
41 is divisible by 41, and certainly when n = 41, 41² + 41 + 41 is
clearly divisible by 41.

The incredible formula  n^2 − 79n + 1601 was discovered, which produces
80 primes for the consecutive values n = 0 to 79. The product of the
coefficients, −79 and 1601, is −126479.

Considering quadratics of the form:

n^2 + an + b, where |a| < 1000 and |b| < 1000

where |n| is the modulus/absolute value of n e.g. |11| = 11 and |−4| = 4 

Find the product of the coefficients, a and b, for the quadratic
expression that produces the maximum number of primes for consecutive
values of n, starting with n = 0.

|#

(require racket/generator
         racket/sequence)

(define table-size 10000)

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
  (sequence-filter prime? 1000))

(define (prime-length a b)
  (define (once n)
    (define v (+ (expt n 2) (* a n) b))
    (cond [(prime? v) (once (add1 n))]
          [else n]))
  (once 0))

(define (solve)
  (for*/fold ([m 0] [i #f] [j #f]) ([a (in-range -1000 1001)]
                                    [b (in-primes)])
    (define len (prime-length a b))
    (cond [(> len m) (values len a b)]
          [else
            (values m i j)])))

