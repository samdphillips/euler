#lang racket/base

#|
The decimal number, 585 = 10010010012 (binary), is 
palindromic in both bases.

Find the sum of all numbers, less than one million, which are 
palindromic in base 10 and base 2.

(Please note that the palindromic number, in either base, may 
not include leading zeros.)
|#

(require racket/generator
         racket/sequence)

(define (igits b)
  (define (rec n [a null])
    (cond [(zero? n) a]
          [else
            (define-values (q r) (quotient/remainder n b))
            (rec q (cons r a))]))
  rec)

(define digits (igits 10))
(define bits   (igits 2))

(define (build-int n*)
  (for/fold ([v 0]) ([n n*])
    (+ n (* v 10))))

(define (dec-palindromes n)
  (define middle
    (if (odd? n)
        (sequence-map list 10)
        (list null)))
  (define n/2   (quotient n 2))
  (define end   (expt 10 n/2))
  (define start (expt 10 (sub1 n/2)))
  (in-generator
    (for* ([x (in-range start end)]
           [m middle])
      (define d (digits x))
      (yield (build-int (append d m (reverse d)))))))

(define (binary-palindrome? n)
  (define b (bits n))
  (equal? b (reverse b)))

(define (two-base-palindromes)
  (sequence-filter 
    binary-palindrome?
    (for/fold ([p* '(1 3 5 7 9)]) ([p (in-range 2 7)])
      (sequence-append p*
                       (sequence-filter
                         odd?
                         (dec-palindromes p))))))

(define (solve)
  (for/sum ([n (two-base-palindromes)]) n))

