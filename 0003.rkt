#lang racket/base

#|
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
|#

(define (rho n [k 10])
  (define c (random 4294967087))
  
  (define (f x)
    (modulo (+ (expt x 2) c) n))    
  
  (define (r x y d)
    (cond [(= 1 d) (let* ([x (f x)]
                          [y (f (f y))]
                          [d (gcd (abs (- x y)) n)])
                     (r x y d))]
          [(= n d) (rho n (sub1 k))]
          [else    d]))
  
  (if (zero? k)
      #f
      (r 2 2 1)))

(define (solve n)
  (let loop ([n n] [f* null])
    (cond [(rho n) => (lambda (f)
                        (loop (/ n f) (cons f f*)))]
          [(null? f*) (list n)]
          [else       (for/fold ([a null]) ([n (in-list (cons n f*))])
                        (append a (solve n)))])))
