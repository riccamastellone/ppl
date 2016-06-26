#lang racket
; A program collects data from different nodes of the network and put them in a list containing elements of
; different types - we call this list “unsorted”. E.g. (3 "bob" #(6 6 1) 4 #(1 2) -2 end 9).
; We want to take from the unsorted list all the elements that are numbers or vectors: the numbers are
; summed, while the vectors are collected in another list (it is not necessary to maintain the order of the
; original list). To memorize the data, we introduce a structure called demuxed, that has two fields named num
; and vec. E.g. for the previous case: 5 and (#(6 6 1) #(1 2)), respectively.

; Define the demuxed data strucure, and a procedure (called demux-imperative) that has two parameters d
; and l. d is a demuxed data strucure, while l is an unsorted list. This procedure must update d with data from
; l, stopping when the element end is found, if present.

(struct demuxed (
          (num #:mutable)
          (vec #:mutable)
        )
)

(define end 'end)

(define (demux-imperative d l)
    (if (or (null? l) (eq? (car l) end))
        d
        (begin 
                (cond ((vector? (car l)) (set-demuxed-vec! d (cons (car l) (demuxed-vec d) ) ) )
                      ((number? (car l)) (set-demuxed-num! d (+ (demuxed-num d) (car l))) ) )
                (demux-imperative d (cdr l))
           
        )
  )
)

(define test (demuxed 0  '()))
(demux-imperative test '(3 "bob" #(6 6 1) 4 #(1 2) -2 end 9))
(display (demuxed-num test))
(newline)
(display (demuxed-vec test))

; Assume that demuxed is immutable. Define a functional, tail-recursive procedure demux-tail-rec that takes
; an unsorted list and returns a demuxed data structure containing the data, processed as in before. You may
; use as many additional parameters as you need, but you must specify their initial value.

(define (demux-tail-ref l n v)
  (if (or (null? l) (eq? (car l) end))
      (demuxed n v)
      (demux-tail-ref (cdr l) (if (number? (car l)) (+ n (car l)) n) (if (vector? (car l)) (cons (car l) v) v))
  )
)
