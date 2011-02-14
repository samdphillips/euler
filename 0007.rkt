#lang racket/base

#|
By listing the first six prime numbers: 2, 3, 5, 7, 11, and
13, we can see that the 6th prime is 13.

What is the 10001st prime number?
|#

(require racket/stream)

(define (in-primes)
  (make-do-sequence
   (lambda ()
     (values 
      (lambda (s)
        (stream-first s))        
      (lambda (s)
        (let ([p (stream-first s)])
          (stream-filter
           (lambda (n)
             (< 0 (modulo n p)))
           (stream-rest s))))
      (in-naturals 2)
      (lambda a #t)
      (lambda a #t)
      (lambda a #t)))))

(define (solve n)
  (stream-ref (in-primes)
              (sub1 n)))

(solve 10001)
