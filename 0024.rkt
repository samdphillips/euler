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

(require racket/list)

(define (integer->factoradic n d)
  (define (build-number n m a)
    (cond
      [(zero? n) a]
      [else
       (define-values (q r) (quotient/remainder n m))
       (build-number q (add1 m) (cons r a))]))
  (define (pad acc)
    (define len (length acc))
    (if (< len d)
        (for/fold ([acc acc]) ([i (in-range len d)]) (cons 0 acc))
        acc))
  (pad (build-number n 2 '(0))))

;; pick :: List.of(Any) * NonnegInt -> values(Any, List.of(Any)
;; extracts the nth element of a List
(define (pick n* i)
  (cond
    [(null? n*) (error 'pick "not enough values in list")]
    [(zero? i) (values (car n*) (cdr n*))]
    [else
     (define-values (n tl) (pick (cdr n*) (sub1 i)))
     (values n (cons (car n*) tl))]))

(define (choose-digits i* n*)
  (cond
    [(null? i*) null]
    [else
     (define-values (n n*^) (pick n* (car i*)))
     (cons n (choose-digits (cdr i*) n*^))]))

;; solve :: NonnegInt * NonnegInt -> List.of(Digit)
;; Takes the permutation to generate and the number of digits to use
(define (solve n m)
  (define m* (range 0 m))
  (define f* (integer->factoradic (sub1 n) m))
  (choose-digits f* m*))
