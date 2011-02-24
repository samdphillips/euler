#lang racket/base

#|
By starting at the top of the triangle below and moving to 
adjacent numbers on the row below, the maximum total from 
top to bottom is 23.

3
7 4
2 4 6
8 5 9 3

That is, 3 + 7 + 4 + 9 = 23.

Find the maximum total from top to bottom of the triangle below:

75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23

NOTE: As there are only 16384 routes, it is possible to solve
this problem by trying every route. However, Problem 67, is 
the same challenge with a triangle containing one-hundred rows; 
it cannot be solved by brute force, and requires a clever 
method! ;o)
|#

(define the-triangle
  (vector
   (vector 75)
   (vector 95 64)
   (vector 17 47 82)
   (vector 18 35 87 10)
   (vector 20 04 82 47 65)
   (vector 19 01 23 75 03 34)
   (vector 88 02 77 73 07 63 67)
   (vector 99 65 04 28 06 16 70 92)
   (vector 41 41 26 56 83 40 80 70 33)
   (vector 41 48 72 33 47 32 37 16 94 29)
   (vector 53 71 44 65 25 43 91 52 97 51 14)
   (vector 70 11 33 28 77 73 17 78 39 68 17 57)
   (vector 91 71 52 38 17 14 91 43 58 50 27 29 48)
   (vector 63 66 04 68 89 53 67 30 73 16 69 87 40 31)
   (vector 04 62 98 27 23 09 70 98 73 93 38 53 60 04 23)))

#;
(define the-triangle
  (vector
   (vector 3)
   (vector 7 4)
   (vector 2 4 6)
   (vector 8 5 9 3)))

#;
(define the-triangle
  (vector
   (vector 01)
   (vector 02 01)
   (vector 01 01 09)
   (vector 01 01 01 09)))

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