#lang racket/base

#|
2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

What is the sum of the digits of the number 2^1000?
|#

(define (solve [n 1000])  
  (define (sum s n)
    (if (zero? n) 
        s
        (let-values ([(n m) (quotient/remainder n 10)])
          (sum (+ s m) n))))
  
  (sum 0 (expt 2 n)))

