;;;; Duplicate the elements of a list.

(load (merge-pathnames "p05.lisp" *load-truename*)) ; reverse-list

(defun duplicate-list (list)
    (cond
        ((equal list nil) nil)
        (t (reverse-list (duplicate-list-fun list '())))
    ))

(defun duplicate-list-fun (list duplicated)
    (cond
        ((equal list nil) duplicated)
        (t (duplicate-list-fun (cdr list) (cons (car list) (cons (car list) duplicated))))
    ))

(assert (equal (duplicate-list '()) '())) 
(assert (equal (duplicate-list '(A)) '(A A))) 
(assert (equal (duplicate-list '(A B C D)) '(A A B B C C D D))) 
(assert (equal (duplicate-list '(A A B A A C D)) '(A A A A B B A A A A C C D D))) 