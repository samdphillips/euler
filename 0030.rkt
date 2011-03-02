#lang racket/base

#|
Surprisingly there are only three numbers that can be written
as the sum of fourth powers of their digits:

    1634 = 1^4 + 6^4 + 3^4 + 4^4
    8208 = 8^4 + 2^4 + 0^4 + 8^4
    9474 = 9^4 + 4^4 + 7^4 + 4^4

As 1 = 1^4 is not a sum it is not included.

The sum of these numbers is 1634 + 8208 + 9474 = 19316.

Find the sum of all the numbers that can be written as the sum
of fifth powers of their digits.
|#

#;
(define (backtrack)
  (error 'backtrack "no more choices"))

#;
(define-syntax amb
  (syntax-rules ()
    [(_) (backtrack)]
    [(_ c choices ...)     
     (let/cc bt
       (let ([old-bt backtrack])
         (set! backtrack
               (lambda ()
                 (set! backtrack old-bt)
                 (bt (amb choices ...))))
         c))]))

#;
(define (solve-bt)
  (define (f i d n)   
    (cond [(and (not (zero? n)) 
                (not (= 1 d))
                (= n d)) d]
          [(> n d) (backtrack)]
          [else           
           (f (amb 9 8 7 6 4 5 3 2 1 0) (+ d (expt i 4)) (+ (* 10 n) i))]))
  
  (let ([i (amb 1 2 3 4 5 6 7 8 9)])
    (f i 0 0)))


(define (integer->digits n)  
  (if (zero? n)
      null
      (let-values ([(q r) (quotient/remainder n 10)])
        (cons r (integer->digits q)))))

(define (solve x)
  (define (f i p)
    (- (expt i x) (* i (expt 10 p))))
  
  (define (value? x)
    (zero? 
     (apply + (for/list ([v (in-list (integer->digits x))]
                         [i (in-naturals)])
                (f v i)))))
  
  (for/fold ([s 0]) ([i (in-range 10 (expt 10 6))])
    (if (value? i)
        (+ i s)
        s)))