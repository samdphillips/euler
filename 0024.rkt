#lang racket/base

#|
A permutation is an ordered arrangement of objects. For 
example, 3124 is one possible permutation of the digits 1, 2, 
3 and 4. If all of the permutations are listed numerically or 
alphabetically, we call it lexicographic order. The 
lexicographic permutations of 0, 1 and 2 are:

012   021   102   120   201   210

What is the millionth lexicographic permutation of the digits 
0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
|#

(require racket/function
         racket/stream)

(define (integer->factoradic n d [m 2] [a '(0)])
  (if (zero? n)
      (if (< (length a) d)
          (append (build-list (- d (length a)) (const 0))
                  a)
          a)
      (let-values ([(q r) (quotient/remainder n m)])
        (integer->factoradic q d (add1 m) (cons r a)))))

(define (pick n* i [head null])
  (if (zero? i)
      (values (car n*) 
              (append (reverse head)
                      (cdr n*)))
      (pick (cdr n*) (sub1 i) (cons (car n*) head))))

(define (choose-digits i* n*)  
  (if (null? i*)
      null
      (let-values ([(n n*) (pick n* (car i*))])
        (cons n (choose-digits (cdr i*) n*)))))

(define (solve n m)
  (let ([m* (stream->list (in-range 0 m))]
        [f* (integer->factoradic (sub1 n) m)])
    (choose-digits f* m*)))

