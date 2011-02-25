#lang racket/base

#|
The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.

Find the last ten digits of the series, 
1^1 + 2^2 + 3^3 + ... + 1000^1000.

|#

#|
More effort can be saved by just calculating the needed
portion of the expt probably.
|#

(define (solve n m)
  (let ([t (expt 10 m)])
    (for/fold ([a 0]) ([i (in-range 1 (add1 n))])
      (modulo (+ a (expt i i)) t))))