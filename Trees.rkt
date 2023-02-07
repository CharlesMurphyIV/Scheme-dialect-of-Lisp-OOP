; Useful starter code for tree manipulation.
(define (make-tree value left right)
(list value left right))
(define (value T) (car T))
(define (right T) (caddr T))
(define (left T) (cadr T))

; Inserts element x into a tree T keeping all the properties of a tree intact. 
(define (insert x T)
(cond ((null? T) (make-tree x '() '()))
((eq? x (value T)) T)
((< x (value T)) (make-tree (value T)
(insert x (left T))
(right T)))
((> x (value T)) (make-tree (value T)
(left T)
(insert x (right T))))))


 

; Computes the size of tree T, i.e. the number of nodes in tree T.
(define (tree-size T)
  (if (null? T)
      0 
      (+ 1 (tree-size (right T)) (tree-size (left T)))))

; Takes a tree T as input and computes its depth from the root node.
(define (tree-depth T)
  (if (null? T)
     -1
     (+ 1 (max (tree-depth (right T)) (tree-depth (left T))))))




(define (count-pred P tree)
  (if (null? tree) 0
      (if (equal? (P (value tree)) #t) (+ 1 (count-pred P (right tree)) (count-pred P (left tree)))
          (+ 0 (count-pred P (right tree)) (count-pred P (left tree))))))






; Takes a tree as input and calculates the number of nodes that have one child.
(define (count-one-child T)
    (cond ((and (null? (left T))
                (null? (right T))) 0)
          ((and (not (null? (left T)))
                (not (null? (right T))))
           (+ (count-one-child (left T))
              (count-one-child (right T))))
          ((null? (left T)) 
           (+ 1 (count-one-child (right T))))
          (else (+ 1 (count-one-child (left T))))))  



 



; Takes a tree T and inverts the left and right subtrees.
; An inverted tree will have the left and riht children of all non-leaf nodes interchanged
; such that the resulting inverted tree is a mirror of the input.
(define (invert-bst T)
  (if (null? T)
      (list)
      (make-tree (value T) (invert-bst (right T))
                 (invert-bst (left T))))) 


(invert-bst '(12 (6 (3 (2 (1 () ()) ()) (5 (4 () ()) ()))(9 (7 () (8 () ())) (10 () (11 () ()))))(18 (15 (13 () (14 () ())) ())(21 (19 () (20 () ())) (27 (25 () ()) (29 () ()))))))

