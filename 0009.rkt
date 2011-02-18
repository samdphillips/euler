#lang racket/base

#|
A Pythagorean triplet is a set of three natural numbers, 
a < b < c, for which,

a^2 + b^2 = c^2

For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.

|#

(require racket/stream)

(define triples
  (apply stream-append
         (for/list ([c (in-range 335 997)])
           (let ([d (- 1000 c)])
             (stream-filter
              (lambda (a b c)
                (< a b c))
              (stream-map
               (lambda (a)
                 (values a (- d a) c))
               (in-range 1 (quotient d 2))))))))

(define (solve)
  (apply * (for/or ([(a b c) triples])
             (and (= (+ (* a a) (* b b)) (* c c))
                  (list a b c)))))