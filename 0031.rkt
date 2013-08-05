#lang racket/base

#|

In England the currency is made up of pound, £, and pence, p, and there
are eight coins in general circulation:

1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).

It is possible to make £2 in the following way:

1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p

How many different ways can £2 be made using any number of coins?

|#

(require (only-in unstable/sequence
                  in-slice)
         racket/format
         racket/generator)

(define coins
  '(200 100 50 20 10 5 2 1))

(define memos
  (make-vector (* (length coins) 200) #f))

(define (show-memos)
  (for ([row (in-slice (length coins) memos)]
        [elem (in-naturals 1)])
    (printf "~a ~a~%" (~a elem #:width 3) row)))

(define (in-list* ls)
  (in-generator
    (define (once ls)
      (unless (null? ls)
        (yield ls)
        (once (cdr ls))))
    (once ls)))

(define-syntax-rule (memoize (amt use) body ...)
  (begin
    (define k (+ (* (length coins) (sub1 amt))
                 (sub1 (length use))))
    (cond [(vector-ref memos k) => values]
          [else
            (let ([v (begin body ...)])
              (vector-set! memos k v)
              v)])))

(define (comb amt use)
  (memoize (amt use)
    (for/sum ([c* (in-list* use)])
      (define c (car c*))
      (cond [(= amt c) 1]
            [(< amt c) 0]
            [else
              (comb (- amt c) c*)]))))

(define (solve)
  (comb 200 coins))

