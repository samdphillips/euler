#lang racket/base

#|
The Fibonacci sequence is defined by the recurrence relation:

    F(n) = F(n−1) + F(n−2), where F(1) = 1 and F(2) = 1.

Hence the first 12 terms will be:

    F(1) = 1
    F(2) = 1
    F(3) = 2
    F(4) = 3
    F(5) = 5
    F(6) = 8
    F(7) = 13
    F(8) = 21
    F(9) = 34
    F(10) = 55
    F(11) = 89
    F(12) = 144

The 12th term, F(12), is the first term to contain three digits.

What is the first term in the Fibonacci sequence to contain 
1000 digits?

|#

#;
(define (fib n)
  (define (f n a b)
    (if (<= n 0)
        (+ a b)
        (f (sub1 n) (+ a b) a)))
  (f (- n 2) 1 0))

(define (fib n)
  (call-with-values
   (lambda ()
     (for/fold ([a 1] [b 0]) ([i (in-range 1 n)])
       (values (+ a b) a)))
   (lambda (a b) a)))

(define (solve n)
  (define t (expt 10 (sub1 n)))
  
  (define (f i a b)
    (if (> a t)
        i
        (f (add1 i) (+ a b) a)))
  (f 1 1 0))