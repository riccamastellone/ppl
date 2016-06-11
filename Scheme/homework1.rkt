#lang racket

; https://github.com/gioenn/pl/blob/master/homework1.txt

; exercise 1

; define the "gcd" function.
; gcd takes two numbers as input and computes the great commom divisor between the two. 

; examples:

; (gcd 8 28) -> 4
; (gcd 11 98) -> 1

(define (gcd n1 n2)
  (if (= n1 n2)
      n1
      (if (> n1 n2)
          (gcd (- n1 n2) n2)
          (gcd n1 (- n2 n1))
      )
   )
)

; exercise 2

; define the "nested-length" function.
; nested-length takes a list as argument and returns the number of element inside the list and all the nested lists.

; examples:
; (nested-length '(1 2 3)) -> 3
; (nested-length '(1 2 (2 (5 4)) 5 6)) -> 7

(define (nested-length L)
  (if (null? L)
      0
      (if (list? (car L))
          (+ (nested-length (car L)) (nested-length (cdr L)))
          (+ 1 (nested-length (cdr L)))
      )
   )
)

; exercise 3

; define the "print-matrix" function.
; print-matrix takes a matrix as argument (a matrix is a vector of vectors, e.g. #(#(1 2) #(3 4)) is a 2x2 matrix) 
; and prints out the content of the matrix as in following examples.

; examples:
; > (print-matrix #(#(1 2) #(3 4)))
; 1    2
; 3    4

(define (print-matrix V)
  (vector-for-each (lambda (row)
                     (vector-for-each (lambda (col)
                                        (begin
                                               (display col)
                                               (display "  ")
                                        )
                                        ) row)
                     (newline)
                     ) V)
)

( define ( vector-for-each body vect )
   (let (( max (- ( vector-length vect ) 1)))
     (let loop (( i 0))
       ( body ( vector-ref vect i )) ; vect [i] in C
       ( when ( < i max )
          ( loop (+ i 1))))))




; exercise 4

; define the "vector*!" function.

; vector*! takes three vectors (v1 v2 v3) as arguments. 
; this function will consider v2 a Nx1 matrix, v3 a 1xM matrix; v1 must be a mutable vector of size N.
; this function computes the multiplication between v2 and v3 and saves the resulting NxM matrix in v1.

; example:

; > (define v1 (make-vector 2))      
; > (vector*! v1 #(3 2) #(4 5))
; > (print-matrix v1)
; 12   15
; 8    10
; > (vector*! v1 #(5 2) #(1 1 0))
; > (print-matrix v1)
; 5    5    0
; 2    2    0


(define (vector*! v1 v2 v3) 
  (let loop ((i 0))
    (unless (= i (vector-length v2))
      (vector-set! v1 i (make-vector (vector-length v3)))
      (let ((n (vector-ref v2 i)) (v (vector-ref v1 i)))
        (let mul ((j 0))
          (unless (= j (vector-length v3))
            (vector-set! v j (* n (vector-ref v3 j)))
            (mul (+ j 1)))))
      (loop (+ i 1)))))

(define v1 (make-vector 2))      
(vector*! v1 #(3 2) #(4 5 7))


; exercise 5

; define the "zip" function.

; zip takes two lists as arguments. it returns a list of lists, where the i-th list contains the i-th element from each input list. 
; the returned list is truncated in length to the length of the shortest argument list. 

; examples: 

; (zip '(1 2 3) '(4 5 6)) -> '((1 4) (2 5) (3 6))
; (zip '(a 4 d) '(2 c)) -> '((a 2) (4 c))

(define (zip L1 L2)
  (if (< (length L1) (length L2))
      (if (null? L1)
          '()
          (cons (list (car L1) (car L2)) (zip (cdr L1) (cdr L2)))
      )
      (if (null? L2)
          '()
          (cons (list (car L1) (car L2)) (zip (cdr L1) (cdr L2)))
      )
  )
)


; exercise 6

; define the "split" function.

; split takes two arguments: a list and a splitter value. 
; it returns a list of lists in which each list is the sublist of the input list between two occurences of the splitter value.

; examples: 

; (split '(a b c x d x e x f) 'x) -> '((a b c) (d) (e) (f))
; (split '(0 1 2 0 2 0 3 0) 0) -> '((1 2) (2) (3))

(define (split lst splitter)
  (define (split-aux lst acc res)
    (cond 
      ((null? lst) (if (null? acc) res (append res (list acc))))
      ((equal? (car lst) splitter)
       (split-aux (cdr lst) '() (append res (if (null? acc) acc (list acc)))))
      (else (split-aux (cdr lst) (append acc (list (car lst))) res))))
  (split-aux lst '() '()))


; exercise 7

; define the "takewhile" function.

; takewhile takes two arguments: a predicate and a list. it returns the elements of the list until the predicate is violated.

; examples: 

; (takewhile odd? '(1 5 7 8 1 4 6 9)) -> '(1 5 7)
; (takewhile positive? '(5 11 4 2 -2 5 6) -> '(5 11 4 2)

(define (takewhile pred L)
  (if (and (not (null? L)) (pred (car L)))
        (cons (car L) (takewhile pred (cdr L)))
        '()
    )
)


; exercise 8

; define the "flatmap" function.
; flatmap is similar to map but it works also for nested lists. it returns a flatten list.

; examples: 
; (flatmap square '(((1 2 3) 4) 0 (6 7) 8)) -> '(1 4 9 16 0 36 49 64)
; (flatmap (lambda(x) (+ x 1)) '(5 (11) ((12 23) 5))) -> '(6 12 13 24 6)

(define (flatmap f l)
  (define (flat l)
    (if (null? l)
        '()
        (if (list? (car l))
            (append (flat (car l)) (flat (cdr l)))
            (cons (car l) (flat (cdr l)))
        )
     )
  )
  (let ((fl (flat l)))
    (map f fl)
  )
)


; exercise 9

; define the "on-sign" macro.

; on-sign choose between three branches depending if the value considered is greater than zero, equals to zero, or less than zero.
; this macro has the following syntax:
; (on-sign <my expr> pos: <block of code> zero: <block of code> neg: <block of code>) 

; examples:

; (on-sign 2
;         pos: 'a
;         zero: 'b
;         neg: 'c) -> 'a

; > (on-sign (- 5 6)
;          pos: (begin (display "positive") (newline))
;          zero: (begin (display "zero") (newline))
;          neg: (begin (display "negative") (newline)))
; negative

(define-syntax on-sign
  (syntax-rules (zero: neg: pos:)
    ((_ num
        pos: body1
        zero: body2
        neg: body3)
     (case num
           ((positive?) body1)
           ((negative?) body2)
           (else body3)
         )
     )))
