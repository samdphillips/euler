#lang racket/base

#|
The number, 1406357289, is a 0 to 9 pandigital number because it is made
up of each of the digits 0 to 9 in some order, but it also has a rather
interesting sub-string divisibility property.

Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we
note the following:

- d2d3d4=406 is divisible by 2
- d3d4d5=063 is divisible by 3
- d4d5d6=635 is divisible by 5
- d5d6d7=357 is divisible by 7
- d6d7d8=572 is divisible by 11
- d7d8d9=728 is divisible by 13
- d8d9d10=289 is divisible by 17

Find the sum of all 0 to 9 pandigital numbers with this property.

|#

(require (only-in unstable/list 
                  check-duplicate)
         racket/generator
         racket/match
         racket/sequence
         racket/set)

(module+ test
  (require rackunit))

;; digits :: integer -> listof 0-9
(define (digits n)
  (define (unfold n d*)
    (cond [(zero? n) d*]
          [else
            (let-values ([(q r) (quotient/remainder n 10)])
              (unfold q (cons r d*)))]))
  (unfold n null))

(module+ test
  (check-equal? (digits 1000) '(1 0 0 0))
  (check-equal? (digits 123) '(1 2 3))
  (check-equal? (digits 0)   '()))

;; no-duplicates? :: listof integer -> boolean
(define (no-duplicates? n*)
  (not (check-duplicate n*)))

(module+ test
  (check-true  (no-duplicates? '(1 4 0 6 3 5 7 2 8 9)))
  (check-true  (no-duplicates? '(4 0 6)))
  (check-true  (no-duplicates? '(0 6 3)))
  (check-false (no-duplicates? '(6 6 6)))
  (check-false (no-duplicates? '(1 0 0))))

;; in-multiples :: integer -> sequenceof integer
(define (in-multiples n)
  (sequence-map (lambda (i) (* n i))
                (in-naturals 1)))

(module+ test
  (for ([m (in-multiples 3)]
        [i (in-range 1 50)])
    (check-equal? m (* i 3)))

  (for ([m (in-multiples 2)]
        [i (in-range 1 20)])
    (check-equal? m (* i 2))))

(define (generate-multiples n [filter values])
  (sequence->list
    (filter
      (filter-duplicates
        (pad-digits
          (sequence-map 
            digits 
            (stop-before
              (in-multiples n)
              (lambda (n)
                (>= n 1000)))))))))

(define (filter-duplicates seq)
  (sequence-filter no-duplicates? seq))

(define (pad-digits seq)
  (sequence-map
    (lambda (ls)
      (case (length ls)
        [(1) (list* 0 0 ls)]
        [(2) (cons 0 ls)]
        [else ls]))
    seq))

(define (constrain pos val*)
  (define (pred? ls)
    (and (memq (list-ref ls pos) val*) #t))
  (lambda (seq)
    (sequence-filter pred? seq)))

(define (find-matches a b exclude v*)
  (define (include? v)
    (not (memq v exclude)))

  (sequence-map
    caddr
    (sequence-filter
      (lambda (v)
        (and (= a (car v))
             (= b (cadr v))
             (include? (caddr v))))
      v*)))

(define (find-missing n*)
  (define c (for/set ([i (in-range 10)]) i))

  (define rest
    (for/fold ([c c]) ([n n*])
      (set-remove c n)))

  (if (= 1 (set-count rest))
      (set-first rest)
      (error 'find-missing "multiple items left in set: ~a" rest)))

(define pos2 (generate-multiples 2))
(define pos3 (generate-multiples 3  (constrain 1 '(0 2 4 6 8))))
(define pos4 (generate-multiples 5  (constrain 0 '(0 2 4 6 8))))
(define pos5 (generate-multiples 7  (constrain 1 '(0 5))))
(define pos6 (generate-multiples 11 (constrain 0 '(0 5))))
(define pos7 (generate-multiples 13))
(define pos8 (generate-multiples 17))

(define (make-results)
  (in-generator
    (for ([p2 pos2])
      (match-define (list a b c) p2)
      (for* ([d (find-matches b c (list a) pos3)]
             [e (find-matches c d (list a b) pos4)]
             [f (find-matches d e (list a b c) pos5)]
             [g (find-matches e f (list a b c d) pos6)]
             [h (find-matches f g (list a b c d e) pos7)]
             [i (find-matches g h (list a b c d e f) pos8)])
        (define n (list a b c d e f g h i))
        (yield (cons (find-missing n) n))))))

(define (solve)
  (for/sum ([r (make-results)])
    (for/sum ([n r]
              [i (in-range 9 -1 -1)])
      (* n (expt 10 i)))))
    
