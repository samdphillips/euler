#lang racket/base

#|
2520 is the smallest number that can be divided by each of the numbers 
from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all
of the numbers from 1 to 20?

The original version I wrote was before the addition of the math/number-theory library.
This is much faster using the speed tuned library.

|#

(require math/number-theory
         racket/match)

(define (merge-factors f1* f2*)
  (match* {f1* f2*}
    [{'() f2*} f2*]
    [{f1* '()} f1*]
    [{(cons (list f e1) f1*) (cons (list f e2) f2*)}
     (cons (list f (max e1 e2))
           (merge-factors f1* f2*))]
    [{(cons (and p1 (list f1 _)) f1**) (cons (and p2 (list f2 _)) f2**)}
     (if (< f1 f2)
         (cons p1 (merge-factors f1** f2*))
         (cons p2 (merge-factors f1* f2**)))]))

(define (solve n)
  (for/fold ([f null] #:result (defactorize f))
            ([i (in-range 2 (add1 n))])
    (merge-factors f (factorize i))))
