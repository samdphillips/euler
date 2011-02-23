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

(require racket/list
         racket/set)

(struct leaf (rest) #:transparent)
(struct node (value children) #:transparent)

(define (digits n)
  (let-values ([(a r) (quotient/remainder n 100)])
    (let-values ([(b c) (quotient/remainder r 10)])
      (list a b c))))

(define init-state
  (leaf
   (let ([tries
          (list 319 680 180 690 129 620 762 689 762 318 368 710 720
                710 629 168 160 689 716 731 736 729 316 729 729 710
                769 290 719 680 318 389 162 289 162 718 729 319 790
                680 890 362 319 760 316 729 380 319 728 716)])
     (for/list ([t (in-list tries)])
       (digits t)))))

#|

(define (explore/step t)
  (cond [(list? t) (map explore/step-node t)]
        [(leaf? t) (explore/step-leaf t)]))

(define (explore/step-leaf lf)
  (define r* (leaf-rest lf))
  (define f* (for/set ([r (in-list r*)]) (first r)))
  
  (define (find-children f)
    (leaf
     (for/fold ([c* null]) ([r (in-list r*)])
       (if (= f (first r))
           (if (null? (cdr r))
               c*
               (cons (cdr r) c*))
           (cons r c*)))))
  
  (for/list ([f (in-set f*)])
    (node f (find-children f))))

(define (explore/step-node n)
  (struct-copy node n
    [children (explore/step (node-children n))]))

(define (fully-explored? t)
  (cond [(null? t) #t]
        [(list? t) (andmap fully-explored? t)]
        [(leaf? t) #f]
        [(node? t) (fully-explored? (node-children t))]
        [else (error 'fully-explored "expected a tree got: ~a" t)]))

(define (explore st)
  (let loop ([st (explore/step st)])    
    (let-values ([(c st) (partition fully-explored? st)])
      (if (null? c)
          (loop (explore/step st))
          c))))
|#