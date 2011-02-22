#lang racket/base

#|
n! means n × (n − 1) × ... × 3 × 2 × 1

Find the sum of the digits in the number 100!
|#

(define (fact n [a 1])
  (if (= n 1)
      a
      (fact (sub1 n) (* a n))))


(define (solve [n 100])  
  (define (sum s n)
    (if (zero? n) 
        s
        (let-values ([(n m) (quotient/remainder n 10)])
          (sum (+ s m) n))))
  
  (sum 0 (fact n)))
  
  