#lang racket/base

#|
A palindromic number reads the same both ways. The largest 
palindrome made from the product of two 2-digit numbers is 
9009 = 91 × 99.

Find the largest palindrome made from the product of two 
3-digit numbers.
|#

(require racket/stream)

(define (digits n)
  (stream-map
   (lambda (i) (- (sub1 (expt 10 n)) i))
   (in-range (* 9 (expt 10 (sub1 n))))))

(define (reverse-num n [a 0])
  (if (< n 10)
      (+ n (* a 10))
      (let-values ([(q r) (quotient/remainder n 10)])
        (reverse-num q (+ r (* a 10))))))

(define (palindrome n)
  (let ([m (quotient n 2)])
    (stream-map
     (lambda (x)
       (+ (* x (expt 10 m))
          (reverse-num x)))
     (digits m))))

(define (solve [n 6])
  (let ([h  (quotient n 2)])        
    (let next-p ([p* (palindrome n)])
      (let ([p  (stream-first p*)]
            [p* (stream-rest p*)])
        (let step-t ([t (inexact->exact (ceiling (sqrt p)))])
          (if (> t (expt 10 h))
              (next-p p*)
              (let-values ([(q r) (quotient/remainder p t)])
                (if (zero? r)
                    (values p t q)
                    (step-t (add1 t))))))))))
    