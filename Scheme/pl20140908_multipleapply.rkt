#lang racket
; Define a procedure called multiple-apply which takes another procedure f, a natural number n and an item x,
; and applies f n times to x, i.e. it should return fn(x).

(define (multiple-apply f n x)
  (if (eq? n 0)
      x
      (f (multiple-apply f (- n 1) x))
  )
)

; Define a procedure called position-of-max, that takes a list l and returns the position of l which contains the maximum
; value present in l. E.g. (position-of-max ’(2 3 1 -2)) is 1.
; Note: remember that max in Scheme accepts a variable number of arguments, at least one. E.g. (max 2 3 1 -2) is 3.

(define (position-of-max l)
  (let ((mas (apply max l)))
    (let loop ((list l) (pos 0))
      (if (eq? (car list) mas)
          pos
          (loop (cdr list) (+ 1 pos))
          )
      )
    )
)

;(position-of-max '(2 3 1 -2))

; Consider a definition of norm, where the norm of a number is the number itself, while the norm of a string is its length.
; Write a procedure called max-of-the-longest, that takes a list of lists, containing either strings or numbers, and returns
; the maximum norm of the elements in the longest of the lists.
; E.g. (max-of-the-longest ’((99 0) (2 3 "hi, there!") (3 "hi there" 1 -1 -1))) is 8.


(define (max-of-the-longest l)
  (define (motlh l) (apply max (map (lambda(x) (if (string? x) (string-length x) x) ) l) ) )
  (let ((mas 0) (res 0))
    (for-each (lambda(x) (when (> (length x) mas) (begin(set! res (motlh x))(set! mas (length x))))) l)
    res
  )
)

(max-of-the-longest '((99 0) (2 3 "hi, there!") (3 "hi there" 1 -1 -1)))
  