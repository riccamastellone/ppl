#lang racket

;Define a procedure (called vecstrings) that accepts two parameters: a vector V and a list L of strings.
;vecstrings is used to put every string s in L in V, depending on its length: s is placed at position V[|s|],
;while strings too long are discarded. If more than one strings have the same length, they are collected in a list.

(define (vecstrings V L)
  (let ((max (vector-length V)))
    (for-each (lambda (x)
        (let ((len (string-length x)))
        (if (>= len max)
            '()
            (if (list? (vector-ref V len) )
                (vector-set! V len (append (vector-ref V len) (list x)))
                (if (string? (vector-ref V len))
                    (vector-set! V len (list (vector-ref V len) x))
                    (vector-set! V len x)
                )
            )
            )
          )
        )
     L)
   )
)


(define ex '("hi" "there" "have" "an" "interesting" "day"))
(define v1 (make-vector 7 #f))
(vecstrings v1 ex)

; #(#f #f (“an” “hi”) “day” “have” “there” #f)


;Define the procedure make-vecstring, which is a variant of vecstrings returning a closure over V.
;Such closure has one parameter that must be a string s and works like vecstrings, by putting s in V.
;When the closure is called with the parameter 'return, it must return the current value of V.

(define (make-vecstring V)
  (define (return) V)
  (lambda (x)
    (if (eq? x 'return)
        (return)
        (vecstrings V (list x))
    )
   )
)

(define my-v (make-vecstring v1))
(my-v "another")
(my-v "member")
(my-v "no")
(my-v 'return)