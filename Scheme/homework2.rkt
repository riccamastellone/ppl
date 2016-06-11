#lang racket

; https://github.com/gioenn/pl/blob/master/homework2.txt

; exercise 1

; define the "unzip" function.
; unzip takes a zipped list (see exercise 5 of homework 1) and returns the a list that contains the original two lists.

; examples:
; (unzip '((1 4) (2 5) (3 6))) -> '((1 2 3) (4 5 6))
; (unzip '((a 2) (4 c))) -> '((a 4) (2 c))


(define (unzip L)
  (define (unziph L l1 l2)
    (if (null? L)
        (list l1 l2)
        (unziph (cdr L) (append l1 (list (caar L))) (append l2 (list (cadar L))))
     )       
  )
  (unziph L '() '())
)

;exercise 2

;define "zip*", a zip function that takes an indefinite number of lists to be zipped instead of only two.

;(zip* '(1 2 3) '(4 5 6) '(7 8 9)) -> '((1 4 7) (2 5 8) (3 6 9))
;(zip* '(1 2 3)) -> '((1) (2) (3))


(define (zip* . L)
  (if (empty? (car L))
      '()
      (append (cons (map car L) (apply zip* (map cdr L)))
  )
))

;exercise 3

;define the "make-iterator" function that takes a list as argument and returns a closure. 
;this closure is an iterator object with the two well-known methods has-next? and next. 
;use the 'closures as objects' technique seen in class.


;example:

;(define my-it (make-iterator '(1 2 3)))
;(my-it 'has-next?) -> #t
;(my-it 'next) -> 1
;(my-it 'next) -> 2
;(my-it 'has-next?) -> #t
;(my-it 'next) -> 3
;(my-it 'has-next?) -> #f
;(my-it 'next) -> error

(define (make-iterator L)
  (let ((lst (cons '() L) ))
    (define (next)
      (if (has-next)
          (begin
            (set! lst (cdr lst))
            (car lst)
          )
          "error"
      )
    )
    (define (has-next)
      (if (empty? (cdr lst))
          #f
          #t
      )
     )
      (lambda(method . args)
            (apply (case method
              ((has-next?) has-next)
              ((next) next)
            ) args)
      )
  )
)

;exercise 4

;define the "iter" macro.

;iter uses a closure-iterator to iterate through a list. iter has the following syntax:
;(iter <the iterator> -> <a variable symbol> <code>)

;example:
;(define my-it (make-iterator '(1 2 3 4)))

;(iter my-it -> x
;      (display x)
;      (newline))

(define-syntax iter
  (syntax-rules(->)
    ((_ it -> x body ...)
     (let loop ((x (it 'next)))
       (begin
         body ...
         (when (it 'has-next?) (loop (it 'next)))
       )
     )
    )
  )
)


; exercise 5

;given this code:
;
(define (list-iter-cc lst)
  (call/cc 
   (lambda (return) 
     (for-each               
      (lambda (x)
        (call/cc (lambda (next-step)
                   (return (cons x next-step))))) 
      lst)
     'end)))
;
;
;enrich the iter macro using the list-iter-cc function with the following syntax.
;
;(iter <a variable symbol> in <a list> <code>)
;
;example:
;
;(iter x in '(1 2 3) 
;	(display x)
;	(newline))


(define-syntax iter2
  (syntax-rules(-> in)
    ((_ it -> x body ...)
     (let loop ((x (it 'next)))
       (begin
         body ...
         (when (it 'has-next?) (loop (it 'next)))
       )
     )
    )
    ((_ var in lst body ...)
     (let loop ((head (list-iter-cc lst)))
       (unless (eq? head 'end)
         (let ((var (car head)))
           body ... 
           (loop ((cdr head)))
           )
         )
       )
     )
  )
)
 