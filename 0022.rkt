#lang racket/base

#|
Using names.txt  (right click and 'Save Link/Target As...'), a
46K text file containing over five-thousand first names, begin
by sorting it into alphabetical order. Then working out the
alphabetical value for each name, multiply this value by its
alphabetical position in the list to obtain a name score.

For example, when the list is sorted into alphabetical order,
COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th
name in the list. So, COLIN would obtain a score of 938 × 53 =
49714.

What is the total of all the name scores in the file?
|#

(require racket/file)

(define (quote? c)
  (and (char? c) (char=? c #\")))

(define (read-name port)
  (if (eof-object? (peek-char port))
      eof
      (begin
        (read-char port)
        (let ([name (for/fold ([s ""]) ([c (stop-before 
                                            (in-input-port-chars port) 
                                            quote?)])
                      (string-append s (string c)))])
          (read-char port)
          name))))

(define (name->score s)
  (for/fold ([sc 0]) ([c (in-string s)])
    (+ sc (- (char->integer c) 64))))

(define (solve)
  (define names
    (sort (file->list 
           "0022-names.txt"
           read-name)
          string<?))
  
  (for/fold ([s 0]) ([i (in-naturals 1)]
                     [n (in-list names)])
    (+ s (* i (name->score n)))))