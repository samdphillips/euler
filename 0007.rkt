#lang racket/base

#|
By listing the first six prime numbers: 2, 3, 5, 7, 11, and
13, we can see that the 6th prime is 13.

What is the 10001st prime number?
|#

(define (sieve p prime?)
  (lambda (n)
    (if (zero? (modulo n p)) #f (prime? n))))

(define (next-prime n prime?)
  (or (prime? n) (next-prime (add1 n) prime?)))

(define (solve [i 10001] [n 2] [prime? values])
  (if (zero? i)
      n
      (let ([p (next-prime n prime?)])
        (solve (sub1 i) p (sieve p prime?)))))
