#lang racket
(define (co-sublist List i j)
  (let ((diff (- j i)))
    (if (eq? diff -1) ; j index included!
        List
        (if (eq? i 0)
            (co-sublist (cdr List) 0 (- j 1))
            (cons (car List) (co-sublist (cdr List) (- i 1) (- j 1)))
        )
    )
  )
)

;(co-sublist '(7 2 3 4 5 6) 1 3)


(define -> 'START_L)
(define <- 'START_L)

(define (subl x . xs)
  (if (eq? x ->)
      (subl-h xs)
      (apply subl xs)))
(define (subl-h list)
      (if (eq? (car list) <-)
          '()
          (cons (car list) (subl-h (cdr list)))))

;(subl 1 -> 2 3 4 <- 5 6)