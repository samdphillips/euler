#lang racket/base

#|
Let d(n) be defined as the sum of proper divisors of n (numbers 
less than n which divide evenly into n).
If d(a) = b and d(b) = a, where a ≠ b, then a and b are an 
amicable pair and each of a and b are called amicable numbers.

For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 
11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The 
proper divisors of 284 are 1, 2, 4, 71 and 142; so 
d(284) = 220.

Evaluate the sum of all the amicable numbers under 10000.
|#

(require (except-in racket/set set-map)
         racket/list)

(define primes (list 2))

(define (generate-primes! n [p* primes] [c (add1 (last primes))])
  (cond [(zero? n)  
         (void)]
        
        [(null? p*) 
         (set! primes (append primes (list c)))
         (generate-primes! (sub1 n) primes (add1 c))]
        
        [(zero? (modulo c (car p*)))
         (generate-primes! n primes (add1 c))]
        
        [else
         (generate-primes! n (cdr p*) c)]))

(define (next-prime p)
  (let ([r (member p primes)])
    (if (= 1 (length r))
        (begin 
          (generate-primes! (floor (sqrt p)))
          (next-prime p))
        (cadr r))))

(define-values (factors ftable)
  (let ([table (make-hash)])
    (values
     (lambda (n)
       (cond [(hash-ref table n #f) => values]
             [else
              (let ([st (set-add (pfactors n 2 (ceiling (sqrt n))) 1)])
                (hash-set! table n st)
                st)]))
     table)))

;; set-map in the library returns a list...
(define (set-map st f)
  (for/set ([e (in-set st)]) (f e)))

(define (pfactors n p limit)
  (cond [(zero? (modulo n p))
         (let* ([d  (/ n p)]
                [f* (factors d)])
           (set-union (set-map f* (lambda (x) (* p x)))
                      f*
                      (set d p)))]
        [(< p limit)         
         (pfactors n (next-prime p) limit)]
        [else (set)]))

(define-values (amicable? atable)
  (let ([table (make-hash)])    
    (define (d n)
      (for/fold ([s 0]) ([i (in-set (factors n))])
        (+ i s)))
    
    (values
     (lambda (a)
       (cond [(hash-ref table a #f) => values]
             [else
              (let* ([b   (d a)]
                     [flg (and (not (= a b))
                               (= (d b) a))])
                (hash-set! table a flg)
                (when flg
                  (hash-set! table b flg))
                flg)]))
     table)))

(define (solve n)
  (for/fold ([s 0]) ([i (in-range 1 n)])
    (if (amicable? i) (+ s i) s)))

