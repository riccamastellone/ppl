#lang racket
; A simply-linked circular list (called Clist from now on) is a list in which the last node points to the first
; node (see figure). It is sometimes useful to have a sentinel last node, i.e. a node that does not contain
; data. The sentinel is used e.g. to check if we have traversed the whole list. An empty list contains only the
; sentinel node, that points to itself.

; Define a data structure for Clists (hint: use struct), together with a constructor for an empty Clist, and a
; variant of the cons operation for Clists, which adds a new element as the head of the previous Clist.

; Define the Clist cnode structure (mutable)
(struct cnode ( (value #:mutable) (next #:mutable) ))

; Define the end-of-list token
(define *end* '--end--)

; Build and end-of-list sentinel
(define (cend)
  (let ((node (cnode *end* #f)))
    (set-cnode-next! node node)
    node))

; Check if we reached the end
(define (cend? clist)
  (and (cnode? clist)
       (eq? (cnode-value clist) *end*)))

; Get the end of the list
(define (get-end clist)
  (if (cend? clist)
      clist
      (get-end (cnode-next clist))))

; Define a ccons for clist cons
(define (ccons x node)
  (if (cend? node)
      (let ((out (cnode x #f)))
        (set-cnode-next! out node)
        (set-cnode-next! node out)
        out)
      (let ((end (get-end node))
            (out (cnode x node)))
        (set-cnode-next! end out)
        out)))
            
; cmap a map operation for Clists.
(define (cmap f clist)
  (if (cend? clist)
      clist
      (begin
       (set-cnode-value! clist (f (cnode-value clist)))
       (cmap f (cnode-next clist))
       clist)
      ))


; Clist show
(define (clist-show list)
  (if (cend? list)
      (begin 
        (display "end ---- pointing to: ")
        (display (cnode-value (cnode-next list))))
      (begin
        (display (cnode-value list))
        (newline)
        (clist-show (cnode-next list)))))
  

; TEST
(define tlist (ccons 13 (ccons 12 (cend))))
(clist-show tlist)
(cmap (lambda(x) (+ 1 x)) tlist)
(clist-show tlist)

