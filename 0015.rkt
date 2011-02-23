#lang racket/base

#|
Starting in the top left corner of a 2×2 grid, there are 6 
routes (without backtracking) to the bottom right corner.

How many routes are there through a 20×20 grid?
|#

(define (solve1 n)
  (define w (add1 n))
  (define v (* w w))
  
  (define (succ i)    
    (cond [(= (sub1 v) i)              (list i)]
          [(<= (- v w) i)              (list (add1 i))]
          [(zero? (modulo (add1 i) w)) (list (+ i w))]
          [else                        (list (add1 i) (+ i w))]))
  
  (define (final? st)    
    (for/and ([n (in-list st)])
      (= n (sub1 v))))
  
  (define (step st0)
    (for/fold ([st1 null]) ([n (in-list st0)])
      (append st1 (succ n))))
  
  #;(printf "n: ~a w: ~a v: ~a~%" n w v)  
  (let loop ([st (list 0)])
    #;(printf "~a~%" st)
    (if (final? st)
        (length st)
        (loop (step st)))))

(define (solve2 n)
  (define w (add1 n))
  (define v (* w w))
  
  (define (succ i)    
    (cond [(= (sub1 v) i)              (list i)]
          [(<= (- v w) i)              (list (add1 i))]
          [(zero? (modulo (add1 i) w)) (list (+ i w))]
          [else                        (list (add1 i) (+ i w))]))
  
  (define (term-edge? s)
    (= 1 (length (succ s))))
  
  (define (step st0 c)
    (for/fold ([st1 null] [c c]) ([n (in-list st0)])
      (if (term-edge? n)
          (values st1 (add1 c))
          (values (append st1 (succ n)) c))))      
  
  (let loop ([st (list 0)] [c 0])
    #;(printf "~a ~a~%" (length st) c)
    (if (null? st)
        c
        (call-with-values
         (lambda () (step st c))
         loop))))


