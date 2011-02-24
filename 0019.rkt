#lang racket/base

#|
You are given the following information, but you may prefer to
do some research for yourself.

    * 1 Jan 1900 was a Monday.
    * Thirty days has September,
      April, June and November.
      All the rest have thirty-one,
      Saving February alone,
      Which has twenty-eight, rain or shine.
      And on leap years, twenty-nine.
    * A leap year occurs on any year evenly divisible by 4, 
      but not on a century unless it is divisible by 400.

How many Sundays fell on the first of the month during the 
twentieth century (1 Jan 1901 to 31 Dec 2000)?
|#

(define (leap-year? y)
  (and (zero? (modulo y 4))
       (let ([mod100 (zero? (modulo y 100))])
         (or (not mod100)
             (and mod100 (zero? (modulo y 400)))))))

(define (month-days m y)
  (case m
    [(4 6 9 11) 30]
    [(2) (if (leap-year? y) 29 28)]
    [else 31]))

(define (solve)
  (for*/fold ([d 1] [c 0]) ([y (in-range 1900 2001)]
                            [m (in-range 1 13)])
    (values (modulo (+ d (month-days m y)) 7)
            (if (and (> y 1900) (zero? d))
                (add1 c) c))))