#lang racket/base

#|
A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:

1/2	= 	0.5
1/3	= 	0.(3)
1/4	= 	0.25
1/5	= 	0.2
1/6	= 	0.1(6)
1/7	= 	0.(142857)
1/8	= 	0.125
1/9	= 	0.(1)
1/10	= 	0.1

Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.

Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.

|#

(require racket/format
         racket/match)

(struct state
  (numerator
   denominator
   position
   memo
   digits)
  #:transparent)

(struct cycle
  (start end)
  #:transparent)

(define (div n d c)
  (define finished?
    (match-lambda [(state 0 _ _ _ _) #t]
                  [(cycle _ _)       #t]
                  [_                 #f]))
  
  (define (run s c)
    #;(displayln (~a s))
    (cond [(finished? s) s]
          [(zero? c) (list 'partial s)]
          [else
           (run (div1 s) (sub1 c))]))
  
  (run (state n d 0 (hash) null) c))

(define (div1 s)
  (match-define (state n d p m i*) s)
  (cond [(zero? n) s]
        [(hash-ref m n #f) => (lambda (o) (cycle o p))]
        [(> d n) (state (* 10 n) d (add1 p) (hash-set m n p) (cons 0 i*))]
        [else
         (let*-values ([(i n2) (quotient/remainder n d)])
           (state (* 10 n2) d (add1 p) (hash-set m n p) (cons i i*)))]))

(define (solve)
  (for/fold ([v 0] [m 0]) ([i (in-range 1 1001)])
    (match (div 10 i 1000)
      [(cycle a b)
       (define n (- b a))
       (if (> n m)
           (values i n)
           (values v m))]
      [_ (values v m)])))
      
