#lang racket

; Define a procedure called rep which takes a list L of elements and returns a list of the elements of L that are
; repeated at least twice. The procedure must have linear time complexity, and it can be imperative and use imperative
; data structures.
; E.g. (rep ’(3 2 "hi" 2 "hello" hello "hi")) is (2 "hi").

(define (rep L)
  (define (rep-h L1 L2)
    (if (null? L1)
        '()
        (if (eq? (member (car L1) L2) #f)
            (rep-h (cdr L1) (append L2 (list (car L1) )))
            (cons (car L1) (rep-h (cdr L1) L2))
        )
    )
  )
  (rep-h L '())
)


(rep '(3 2 "hi" 2 "hello" hello "hi")) 