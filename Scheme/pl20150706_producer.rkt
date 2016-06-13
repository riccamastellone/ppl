#lang racket


(define (producer ag1 ag2)
  (let loop ((i 1))
    (if (< i 10)
        (begin
          ((if (odd? i) ag1 ag2) i)
          (loop (+ i 1)))
        (cons
         (ag2 'end)
         (ag1 'end)))))

;Define two closures clos1 and clos2 for passing them to producer such that it returns the list ’(20 9 7 5 3 1)
;(note that 20 = 2 + 4 + 6 + 8).

(define (close1)
  (let ((list '()))
    (lambda(arg)
      (if (eq? arg 'end)
          list
          (set! list (cons arg list))
      )
    )
  )
)

(define (close2)
  (let ((sum 0))
    (lambda(arg)
      (if (eq? arg 'end)
        sum
        (set! sum (+ arg sum))
      )
    )
  )
)

;(producer (close1) (close2))


; Define a macro multiple-apply, (multiple-apply (fun-1 fun-2 ...) to list-1 list-2 ...) where
; fun-i are functions and to is a keyword, which returns a list containing the result of applying fun-i to
; list-i.
; E.g.
; > (multiple-apply (+ - *) to ’(1 2 3) ’(3 4) ’(9 -2))
; (6 -1 -18)

(define-syntax multiple-apply
  (syntax-rules(to)
    ((_ (f ...) to l ...)
      (list (apply f l) ...))
     )
    )

;(multiple-apply (+ - *) to '(1 2 3) '(3 4) '(9 -2))

