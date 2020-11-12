;;;; Eliminate consecutive duplicates of list elements.
;;;; If a list contains repeated elements they should be replaced with a single copy of the element. 
;;;; The order of the elements should not be changed.

(load (merge-pathnames "p05.lisp" *load-truename*)) ; reverse-list 

(defun compress-list (list) 
    (cond
        ((equal list nil) nil)
        (t (reverse-list (compress-list-fun list '())))
    ))

(defun compress-list-fun (list compressed)
    (cond
        ((equal list nil) compressed)
        ((equal (car list) (car (cdr list))) (compress-list-fun (cdr list) compressed))
        (t (compress-list-fun (cdr list) (cons (car list) compressed)))
    ))

(assert (equal (compress-list '()) '())) 
(assert (equal (compress-list '(A)) '(A))) 
(assert (equal (compress-list '(A B C D)) '(A B C D))) 
(assert (equal (compress-list '(A A B A A C D)) '(A B A C D))) 
(assert (equal (compress-list '(A B C D D D D)) '(A B C D))) 
(assert (equal (compress-list '(A A A B B B B B C C D C)) '(A B C D C)))