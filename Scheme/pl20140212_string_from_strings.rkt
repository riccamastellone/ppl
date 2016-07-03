#lang racket
; Consider a procedure string-from-strings that receives as input a list of objects, keeps all the objects
; that are strings, discarding all the others, and returns the ordered concatenation of all such strings.
; E.g. (string-from-strings ’(1 "hello" ", " 2 "world")) is "hello, world"

; Define a functional (non tail) recursive version of string-from-strings (without using map, filter, fold).

(define (string-from-strings l)
  (if (null? l)
      ""
      (if (string? (car l))
          (string-append (car l) (string-from-strings (cdr l)))
          (string-from-strings (cdr l))
      )
   )
)

; Define a tail recursive version of string-from-strings (without using map, filter, fold).

(define (string-from-strings-tl l)
  (define (sfsh l a)
    (if (null? l)
        a
        (sfsh (cdr l) (if (string? (car l)) (string-append a (car l)) a))
    )
  )
  (sfsh l "")
)

; Give an implementation of string-from-strings using the classical functional higher order functions, i.e.
; map, filter, fold...

(define (string-from-strings-ho l)
  (foldr string-append "" (filter string? l))
)