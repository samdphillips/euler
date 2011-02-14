#lang racket/base

#|
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
|#

#|
My initial solution using Pollard-rho.  It works...
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

#|
This is the solution sketched out in the solution pdf.
|#

(define (solve2 n)
  (define (rec-div n f)
    (if (zero? (modulo n f))
        (rec-div (/ n f) f)
        n))
  
  (define (rec n f maxf lastf)
    (printf "~a ~a ~a ~a~%" n f maxf lastf)
    (cond [(> f maxf)           (if (= n 1) lastf n)]
          [(zero? (modulo n f)) (let ([n (rec-div n f)])
                                  (rec n (+ f 2) (sqrt n) f))]
          [else                 (rec n (+ f 2) maxf lastf)]))
  
  (let-values ([(n lastf) (if (even? n)
                              (values (rec-div n 2) 2)
                              (values n 1))])
    (rec n 3 (sqrt n) lastf)))

