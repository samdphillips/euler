#lang racket/base

#|
By starting at the top of the triangle below and moving to 
adjacent numbers on the row below, the maximum total from top 
to bottom is 23.

3
7 4
2 4 6
8 5 9 3

That is, 3 + 7 + 4 + 9 = 23.

Find the maximum total from top to bottom in triangle.txt 
(right click and 'Save Link/Target As...'), a 15K text file
containing a triangle with one-hundred rows.

NOTE: This is a much more difficult version of Problem 18. It 
is not possible to try every route to solve this problem, as 
there are 299 altogether! If you could check one 
trillion (1012) routes every second it would take over twenty 
billion years to check them all. There is an efficient 
algorithm to solve it. ;o)

|#

(require racket/port)

(define the-triangle
  (call-with-input-file "0067-triangle.txt"
    (lambda (in)
      (for/vector ([line (in-lines in)])
        (call-with-input-string line
          (lambda (line-in)
            (for/vector ([n (in-port read line-in)]) n)))))))        

(define (triangle-ref x y)
  (vector-ref (vector-ref the-triangle y) x))

(define (triangle-left x y)
  (values x (add1 y)))

(define (triangle-right x y)
  (values (add1 x) (add1 y)))

(define (triangle-bottom? y)
  (= (sub1 (vector-length the-triangle)) y))

(define-values (triangle-max table)
  (let ([table (make-hash)])
    (values
     (lambda (x y)
       (cond [(hash-ref table (cons x y) #f) => values]             
             [(triangle-bottom? y) (triangle-ref x y)]
             [else
              (let-values ([(lx ly) (triangle-left x y)]
                           [(rx ry) (triangle-right x y)])
                (let ([v (+ (triangle-ref x y)
                            (max (triangle-max lx ly)
                                 (triangle-max rx ry)))])
                  (hash-set! table (cons x y) v)
                  v))]))
     table)))

(define (solve)
  (triangle-max 0 0))