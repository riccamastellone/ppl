#lang racket

;
(define (urmax lists)
  (define (urmax-h lists index max)
    (if (null? lists)
        max
        (let* (
               (list (car lists))
               (list_max (list-ref list index)))
          (if (> max list_max)
              (urmax-h (cdr lists) (+ 1 index) max)
              (urmax-h (cdr lists) (+ 1 index) list_max)
              )
          )
    ))
  (urmax-h lists 0 (car (car lists)))
)

;(urmax '((-1)(1 2)(1 2 3)(10 2 3 -4))) ;3 
;(urmax '((-1)(1 8)(1 2 3)(10 2 3 -4))) ;8
;(urmax '((-1)(1 8)(1 2 3)(10 2 3 -4)(5 6 7 8 9))) ;9


(define (murmax lists)
  (define index -1)
  
  (apply max (map (lambda(x) (begin
                    (set! index (+ 1 index))
                    (list-ref x index)
                    )) lists)))


(murmax '((-1)(1 2)(1 2 3)(10 2 3 -4))) ;3 
(urmax '((-1)(1 8)(1 2 3)(10 2 3 -4))) ;8
(urmax '((-1)(1 8)(1 2 3)(10 2 3 -4)(5 6 7 8 9))) ;9