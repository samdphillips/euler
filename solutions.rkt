#lang racket/base

(require racket/base
         rackunit

         (for-syntax racket/base
                     racket/format
                     threading)
         syntax/parse/define)

(define-syntax-parser mb
  [(_ #f body ...)
   #'(#%module-begin body ...)]
  [(_ solution-num:exact-positive-integer body ...)
   #:with file-name
   (~> (syntax->datum #'solution-num)
       (~r #:min-width 4 #:pad-string "0")
       (~a "solutions/" _ ".rktd")
       (datum->syntax #'solution-num _))

   #:with solution
   (datum->syntax #'solution-num 'solution)

   #'(#%module-begin
      (define solution
        (call-with-input-file file-name read))
      body ...)])

(provide (except-out (all-from-out racket/base)
                     #%module-begin)
         (rename-out [mb #%module-begin])
         (all-from-out rackunit))
