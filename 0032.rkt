#lang racket/base

(require racket/stream
         threading)

(module+ test
  (require rackunit))

(define (stream-skip-items xs st)
  (for/fold ([st st]) ([x (in-list xs)])
    (stream-filter (lambda~> (= x) not) st)))

(module+ test
  (check-equal?
    (~>> (in-range 1 10)
         (stream-skip-items '(1 4 8))
         stream->list)
    '(2 3 5 6 7 9))

  (check-equal?
    (~>> (in-range 1 3)
         (stream-skip-items '())
         stream->list)
    '(1 2)))

(define (add-digit st n)
  (cond
    [(stream-empty? st) st]
    [else
      (define fst (stream-first st))
      (define inner (stream-skip-items fst (in-range 1 (add1 n))))
      (stream-append
        (stream-map (lambda~> (cons fst)) inner)
        (add-digit (stream-rest st) n))]))

(module+ test
  (check-equal?
    (~> (list null)
        (add-digit 2)
        (add-digit 2)
        stream->list)
    '((2 1) (1 2))))

(define (pandigit-list-stream n [m n])
  (for/fold ([st (list null)]) ([_i (in-range 1 (add1 m))])
    (add-digit st n)))

(~>> (pandigit-list-stream 9 4)
     (stream-for-each displayln))
