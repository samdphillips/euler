#lang racket/base

#|
Find the difference between the sum of the squares of the 
first one hundred natural numbers and the square of the sum.
|#

(define (solve n)
  (let-values ([(sum-squ squ-sum)
                (for/fold ([sum-squ 0] [squ-sum 0]) ([i (in-range 1 (add1 n))])
                  (values (+ sum-squ (* i i))
                          (+ squ-sum i)))])
    (- (* squ-sum squ-sum) sum-squ)))