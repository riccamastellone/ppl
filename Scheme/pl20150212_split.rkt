#lang racket

; Define a tail-recursive function split, which, given a list L and a number n, returns a vector of two elements:
; the prefix sublist of L, with elements up to the n-th, and the remaining suffix.
; E.g.
; > (split ’(0 1 2 3 4) 2)
; ’#((0 1) (2 3 4))

(define (split L I)
  (define (splith L I L1)
    (if (= I 0)
        (cons L1 (cons L '()))
        (splith (cdr L) (- I 1) (append L1 (cons (car L) '())))
        )
    )
  (splith L I '()) 
)

; Use split to define the function 3-factors, which, given a list L, returns all the possible contiguous sublists
; A, B, C, such that (equal? L (append A B C)). A,B,C, cannot be empty.

(define (3-factors L)
  (let ((l (length L)))
    (3-factors-1 L (- l 1))
    )
)

(define (3-factors-1 L n)
  (if (> n 0)
      (let* (
             (2-list (split L n))
             (other (3-factors-2 (car 2-list) (- (length (car 2-list) ) 1) (cdr 2-list) ))
             )
       (append other (3-factors-1 L (- n 1)))
      )
      '()
  )
)

(define (3-factors-2 L n other)
  (if (> n 0)
        (cons (append (split L n) other) (3-factors-2 L (- n 1) other))
      '()
  )
)


(3-factors '(0 1 2 3 4))
(3-factors '(1 2 3 4 5 6))
