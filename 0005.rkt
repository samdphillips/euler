#lang racket/base

(require racket/stream
         racket/generator)

(define (in-primes)
  (make-do-sequence
   (lambda ()
     (values 
      (lambda (s)
        (stream-first s))
      (lambda (s)
        (let ([p (stream-first s)])
          (stream-filter
           (lambda (n)
             (not (zero? (remainder n p))))
           (stream-rest s))))      
      (in-naturals 2)
      (lambda a #t)
      (lambda a #t)
      (lambda a #t)))))

(define-values (factors ftable)
  (let ([table (make-hash '((1)))])
    (values 
     (lambda (n)
       (cond [(hash-ref table n #f) => values]
             [else
              (let loop ([p* (in-primes)])
                (let ([p (stream-first p*)])
                  (if (zero? (remainder n p))
                      (let ([f* (cons p (factors 
                                         (quotient n p)))])
                        (hash-set! table n f*)
                        f*)
                      (loop (stream-rest p*)))))]))
     table)))

(define (collect-factors f* [p* (in-primes)] [c 0] [c* null])
  (cond [(null? f*)            (if (zero? c)
                                   c*
                                   (append c* (list c)))]
        [(= (car f*)
            (stream-first p*)) (collect-factors (cdr f*)
                                                p*
                                                (add1 c)
                                                c*)]
        [else                  (collect-factors f*
                                                (stream-rest p*)
                                                0
                                                (append c* (list c)))]))
                                
(define (max-factors f1* f2* [acc null])
  (cond [(null? f1*) (append acc f2*)]
        [(null? f2*) (append acc f1*)]
        [else
         (max-factors (cdr f1*) (cdr f2*)
                      (append acc
                              (list (max (car f1*)
                                         (car f2*)))))]))

(define (solve n)
  (let ([f* (for/fold ([f* null]) ([i (in-range 2 (add1 n))])
              (max-factors f* (collect-factors (factors i))))])
    (for/fold ([r 1]) ([p (in-primes)] [f (in-list f*)])
      (* r (expt p f)))))



