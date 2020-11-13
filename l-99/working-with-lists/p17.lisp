;;;; Split a list into two parts; the length of the first part is given.
;;;; ---
;;;; Do not use any predefined predicates.

(load (merge-pathnames "p04.lisp" *load-truename*)) ; element-number
(load (merge-pathnames "../../utils/skip.lisp" *load-truename*))
(load (merge-pathnames "../../utils/drop.lisp" *load-truename*))

(defun split-list (list length)
    (cond
        ((equal list nil) nil)
        ((<= length 0) list)
        ((> length (element-number list)) nil)
        (t (cons (drop list (- (element-number list) length)) (cons (skip list length) nil)))
    ))

(assert (equal (split-list '() 1) '()))
(assert (equal (split-list '(A B C D) 1) '((A) (B C D))))
(assert (equal (split-list '(A) 2) '()))
(assert (equal (split-list '(A B C D) 3) '((A B C) (D))))
(assert (equal (split-list '(A B C D E F G H I) 3) '((A B C) (D E F G H I))))
(assert (equal (split-list '(A A B B C C D D) 2) '((A A) (B B C C D D)))) 