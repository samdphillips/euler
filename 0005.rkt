#lang racket/base

(require racket/stream
         racket/generator)
#|
(define (try-solve-1 [i 10])
  (define (check-solution? n)
    (for/and ([j (in-range 1 (add1 i))])
      (zero? (remainder n j))))
  (let/ec break
    (for ([j (in-naturals 1)])
      (when (check-solution? j)
        (break j)))))


(define (try-solve-2 [i 10])
  (if (= 1 i)
      1
      (let ([p (try-solve-2 (sub1 i))])
        (if (zero? (remainder p i))
            p
            (* p i)))))
|#

(define (in-primes)
  (in-generator
   (define (next-prime n s)
     (define (factor? i)
       (zero? (remainder i n)))
     
     (let* ([s (stream-filter (lambda (i) (not (factor? i))) s)]
            [n (stream-first s)])
       (yield n)
       (next-prime n s)))
   
   (yield 2)
   (next-prime 2 (in-naturals 2))))

(define (prime? n)
  (let/ec break
    (for ([p (in-primes)])
      (when (> p n) (break #f))
      (when (= p n) (break #t)))))

(define (factorize1 n)
  (let/ec break
    (for/fold ([n n] [f* null]) ([p (in-primes)])
      (when (prime? n)
        (break (cons n f*)))
      
      (let loop ([n n] [f* f*])
        (if (prime? n)
            (break (cons n f*))
            (let-values ([(q r) (quotient/remainder n p)])
              (if (zero? r)
                  (loop q (cons p f*))
                  (values n f*))))))))


      