#lang racket/base

#|

If the numbers 1 to 5 are written out in words: one, two,
three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19
letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive
were written out in words, how many letters would be used?

NOTE: Do not count spaces or hyphens. For example, 342 (three
hundred and forty-two) contains 23 letters and 115 (one
hundred and fifteen) contains 20 letters. The use of "and"
when writing out numbers is in compliance with British usage.

|#

(require racket/dict)

(define (build-table start noms)
  (for/list ([nom (in-list noms)]
             [n   (in-naturals start)])
    (cons n nom)))

(define ones
  (let ([table (build-table 1 (list "one"   "two"   "three" 
                                    "four"  "five"  "six" 
                                    "seven" "eight" "nine"))])           
    (lambda (n)
      (dict-ref table n))))

(define teens
  (let ([table (build-table 10 (list "ten"      "eleven"    "twelve"   
                                     "thirteen" "fourteen"  "fifteen"
                                     "sixteen"  "seventeen" "eighteen" 
                                     "nineteen"))])           
    (lambda (n)
      (dict-ref table n))))

(define tens
  (let ([table (build-table 2 (list "twenty" "thirty" "forty" 
                                    "fifty"  "sixty"  "seventy"
                                    "eighty" "ninety"))])           
    (lambda (n)
      (cond [(< n 10) (ones n)]
            [(< n 20) (teens n)]
            [else
             (let-values ([(q r) (quotient/remainder n 10)])
               (string-append 
                (dict-ref table q) (if (zero? r) "" (ones r))))]))))

(define (hundreds n)
  (let-values ([(q r) (quotient/remainder n 100)])
    (string-append (ones q) "hundred" 
                   (if (zero? r)
                       ""
                       (string-append "and" (tens r))))))

(define (number->name n)
  (cond [(= n 1000) "onethousand"]
        [(> n 99)   (hundreds n)]
        [(> n 9)    (tens n)]
        [else       (ones n)]))

(define (solve)
  (for/fold ([s 0]) ([i (in-range 1 1001)])
    #;(printf "~a~%" i)
    (+ s (string-length (number->name i)))))

