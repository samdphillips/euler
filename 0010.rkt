#lang racket/base

#|
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
|#

(require (only-in racket/function const))

(define (solve [n 2000000])
  (define ((sieve p prime?) n)
    (if (zero? (modulo n p)) #f (prime? n)))  
  
  (define (next sum m prime?)    
    (cond [(>= m n)   sum]
          [(prime? m)
           (let ([sum (+ sum m)])
             #;(printf "~a ~a~%" m sum)
             (next sum (+ 2 m) (sieve m prime?)))]
          [else
           (next sum (+ 2 m) prime?)]))
  
  (next 2 3 (sieve 2 (const #t))))