(load (merge-pathnames "p05.lisp" *load-truename*)) ;; reverse-list 
(load (merge-pathnames "../utils/append-list.lisp" *load-truename*))

(defun flatten-list (list) 
    (cond
        ((equal list nil) nil)
        (t (reverse-list (flatten-list-fun list '())))
    ))

(defun flatten-list-fun (list flatten) 
    (cond
        ((equal list nil) flatten)
        ((atom (car list)) (flatten-list-fun (cdr list) (cons (car list) flatten)))
        (t (flatten-list-fun (cdr list) (append-list (flatten-list-fun (car list) '()) flatten)))
    ))


(assert (equal (flatten-list '()) '())) 
(assert (equal (flatten-list '(A)) '(A))) 
(assert (equal (flatten-list '(A (B C))) '(A B C))) 
(assert (equal (flatten-list '(A (B (C)))) '(A B C))) 
(assert (equal (flatten-list '(A ((B C) D) E (F G) )) '(A B C D E F G))) 