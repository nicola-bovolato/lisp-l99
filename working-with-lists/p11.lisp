;;;; Modified run-length encoding.
;;;; Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. 
;;;; Only elements with duplicates are transferred as (N E) lists.

(load (merge-pathnames "p04.lisp" *load-truename*)) ; element-number
(load (merge-pathnames "p05.lisp" *load-truename*)) ; reverse-list 
(load (merge-pathnames "p08.lisp" *load-truename*)) ; compress-list
(load (merge-pathnames "p09.lisp" *load-truename*)) ; pack-list

(defun encode-list-improved (list)
    (cond
        ((equal list nil) nil)
        (t (reverse-list (encode-list-improved-fun (pack-list list) '())))
    ))

(defun encode-list-improved-fun (list encoded)
    (cond
        ((equal list nil) encoded)
        (t (encode-list-improved-fun (cdr list) (cons (encode-compress-list (car list)) encoded)))
    ))

(defun encode-compress-list (list)
    (cond
        ((equal list nil) nil)
        ((= (element-number list) 1) (car list))
        (t (cons (element-number list) (compress-list list)))
    ))

(assert (equal (encode-list-improved '()) '()))
(assert (equal (encode-list-improved '(A A A)) '((3 A))))
(assert (equal (encode-list-improved '(A B C D)) '(A B C D)))
(assert (equal (encode-list-improved '(A A B A A C D)) '((2 A) B (2 A) C D)))
(assert (equal (encode-list-improved '(A B C D D D D)) '(A B C (4 D))))
(assert (equal (encode-list-improved '(A A A B B B B B C C D C)) '((3 A) (5 B) (2 C) D C)))