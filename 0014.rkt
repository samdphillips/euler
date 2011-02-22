#lang racket/base

#|
The following iterative sequence is defined for the set of 
positive integers:

n → n/2 (n is even)
n → 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the 
following sequence:

13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1

It can be seen that this sequence (starting at 13 and 
finishing at 1) contains 10 terms. Although it has not been 
proved yet (Collatz Problem), it is thought that all starting 
numbers finish at 1.

Which starting number, under one million, produces the 
longest chain?

NOTE: Once the chain starts the terms are allowed to go above 
one million.
|#

(define-values (collatz-length table)
  (let ([table (make-hash (list (cons 1 1)))])
    (values
     (lambda (n [path null])
       (cond [(hash-ref table n #f) 
              => (lambda (v)
                   (for/fold ([v v]) ([n (in-list path)])                     
                     (hash-set! table n (add1 v))
                     (add1 v)))]
             [(even? n)
              (collatz-length (/ n 2) (cons n path))]
             [else
              (collatz-length (add1 (* 3 n)) (cons n path))]))                     
     table)))

(define (solve [n 1000000])
  (for/fold ([x 1] [p 1]) ([m (in-range 1 (add1 n))])
    (let ([c (collatz-length m)])
      (if (> c x)
          (values c m)
          (values x p)))))


