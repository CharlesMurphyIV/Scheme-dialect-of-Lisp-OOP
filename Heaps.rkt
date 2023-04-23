; Starter code for a min-heap
(define (create-heap v H1 H2)
  (list v H1 H2))

(define (h-min H) (car H))
(define (left H) (cadr H))
(define (right H) (caddr H))

(define (insert x H)
  (if (null? H)
      (create-heap x '() '())
      (let ((child-value (max x (h-min H)))
            (root-value (min x (h-min H))))
        (create-heap root-value
                     (right H)
                     (insert child-value (left H))))))
(define (combine-heaps H1 H2)
  (cond ((null? H1) H2)
        ((null? H2) H1)
        ((< (h-min H1) (h-min H2))
         (create-heap (h-min H1)
                      (combine-heaps (left H1) (right H1))
                      H2))
        (else
         (create-heap (h-min H2)
                      (combine-heaps (left H2) (right H2))
                      H1))))
                      
(define (remove-min H)
  (combine-heaps (left H) (right H)))

(define (heap-insert f x H)
  (if (null? H)
     (list x (list) (list))
  (if (f x (h-min H))  
       (create-heap x (right H) (heap-insert f (h-min H) (left H)))   
       (create-heap (h-min H) (right H) (heap-insert f x (left H)))))) 

(heap-insert < 15 '(0 (3 (8 () ()) (6 () (19 () ())))(1 (14 () (17 () ())) (2 () (5 () ())))))

(define (empty? H)
  (if (null? H)
      #t
      #f))


(define (combine f Ha Hb) 
  (cond ((null? Ha) Hb)
         ((null? Hb) Ha)
          
         ((f (h-min Ha) (h-min Hb))
          (create-heap (h-min Ha) (combine f (left Ha) (right Ha)) Hb))
         (else  
          (create-heap (h-min Hb) (combine f (left Hb) (right Hb)) Ha))))
   
   
(combine < '(0 (3 (8 () ()) (6 () (19 () ())))(1 (14 () (17 () ())) (2 () (5 () ())))) (list))
 

(define (heap-remove f H)
  (combine f (left H) (right H)))
