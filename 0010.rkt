#lang racket/base

#|
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
|#

(define (solve [n 2000000])
  (define numbers (make-vector (- n 2) #t))
  
  (define (prime? i)
    (vector-ref numbers (- i 2)))
  
  (for/fold ([s 0]) ([i (in-range 2 n)])
    (if (prime? i)
        (begin 
          (for ([j (in-range (* 2 i) n i)])
            (vector-set! numbers (- j 2) #f))
          (+ s i))
        s)))
  
  
    
  
  