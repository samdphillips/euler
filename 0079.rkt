#lang racket/base

#|
A common security method used for online banking is to ask the 
user for three random characters from a passcode. For example, 
if the passcode was 531278, they may ask for the 2nd, 3rd, and 
5th characters; the expected reply would be: 317.

The text file, keylog.txt, contains fifty successful login 
attempts.

Given that the three characters are always asked for in order, 
analyse the file so as to determine the shortest possible 
secret passcode of unknown length.
|#

(require racket/match
         racket/list
         racket/set)

(define (digits n)
  (let-values ([(a r) (quotient/remainder n 100)])
    (let-values ([(b c) (quotient/remainder r 10)])
      (list a b c))))

(define init-state
  (list
   (cons null         
         (let ([tries
                (list 319 680 180 690 129 620 762 689 762 318 
                      368 710 720 710 629 168 160 689 716 731 
                      736 729 316 729 729 710 769 290 719 680 
                      318 389 162 289 162 718 729 319 790 680 
                      890 362 319 760 316 729 380 319 728 716)])
           (for/list ([t (in-list tries)])
             (digits t))))))

(define (sum x*) (apply + x*))

(define (factorial n [a 1])
  (if (zero? n) a (factorial (sub1 n) (* a n))))

(define score
  (match-lambda
    [(cons _ x*)
     (sum (map length x*))]))

(define (sort-score st*)  
  (sort st* < #:key score #:cache-keys? #t))

(define expand
  (match-lambda
    [(cons s* r*)
     
     (define f* (for/set ([r (in-list r*)]) (first r)))
     
     (define (find-children f)
       (for/fold ([c* null]) ([r (in-list r*)])
         (if (= f (first r))
             (if (null? (cdr r))
                 c*
                 (cons (cdr r) c*))
             (cons r c*))))
     
     (for/list ([f (in-set f*)])
       (cons (append s* (list f))
             (find-children f)))]))


(define (expand* st*)
  (sort-score
   (for/fold ([st* null]) ([st (in-list st*)])
     (append st* (expand st)))))

(define (solve [st* init-state])
  (let ([st* (take (expand* st*) 4)])
    (printf "~a~%" (map score st*))
    (if (null? (cdar st*))
        (caar st*)
        (solve st*))))
