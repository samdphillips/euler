#lang racket/base

#|
If we list all the natural numbers below 10 that are multiples 
of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples 
is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
|#

(module solution1 racket/base
  (provide solve)

  (define (mult-of? m n)
    (zero? (modulo m n)))

  (define (mult-of-3-or-5? n)
    (or (mult-of? n 3)
        (mult-of? n 5)))

  (define (solve [m 1000])
    (for/fold ([sum 0]) ([n (in-range m)]
                         #:when (mult-of-3-or-5? n))
      (+ sum n)))
  )

(module solution2 racket/base
  (provide (all-defined-out))

  (define (solve [m 1000])
    (define (loop mult3 mult5 sum)
      (cond
        [(and (>= mult3 m) (>= mult5 m)) sum]
        [(< mult3 mult5) (loop (+ 3 mult3) mult5 (+ mult3 sum))]
        [(< mult5 mult3) (loop mult3 (+ 5 mult5) (+ mult5 sum))]
        [else (loop (+ 3 mult3) (+ 5 mult5) (+ mult5 sum))]))
    (loop 3 5 0))
  )
