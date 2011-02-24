#lang racket/base

#|
The decimal number, 585 = 10010010012 (binary), is 
palindromic in both bases.

Find the sum of all numbers, less than one million, which are 
palindromic in base 10 and base 2.

(Please note that the palindromic number, in either base, may 
not include leading zeros.)
|#

(require racket/function
         racket/list
         racket/match
         racket/stream)

(define (digits-sub1 n*)
  (cond [(null? n*) n*]
        [(zero? (car n*))
         (cons 9 (digits-sub1 (cdr n*)))]
        [else
         (cons (sub1 (car n*))
               (cdr n*))]))

(define digits-end?
  (match-lambda
    [(list (? zero?) ... 1) #t]
    [_ #f]))

(define (digits n)
  (make-do-sequence
   (lambda ()     
     (values
      values
      digits-sub1
      (make-list n 9)
      (const #t)        
      (const #t)
      (lambda (pos ignored)
        (not (digits-end? pos)))))))

(define (palindrome n)
  (define (digits->number n*)
    (for/fold ([n 0]) ([d (in-list n*)])
      (+ d (* n 10)))) 
  
  (if (even? n)
      (stream-map
       (lambda (n*)
         (digits->number
          (append (reverse n*) n*)))
       (digits (/ n 2)))
      
      (make-do-sequence
       (lambda ()
         (let-values ([(more? next) 
                       (sequence-generate
                        (digits (/ (sub1 n) 2)))])
           (values
            (match-lambda
              [(cons mid cur)
               (digits->number
                (append (reverse cur) (list mid) cur))])
            (match-lambda
              [(cons 0 cur)   (cons 9 (next))]
              [(cons mid cur) (cons (sub1 mid) cur)])
            (cons 9 (next))
            (const #t)
            (const #t)
            (match-lambda*
              [(list (cons 0 cur) _) (more?)]
              [_ #t])))))))

(define the-nums
    (stream-filter
     odd?
     (for/fold ([s empty-stream]) ([i (in-range 2 7)])
       (stream-append s (palindrome i)))))

