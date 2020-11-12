(load (merge-pathnames "../l-99/working-with-lists/p05.lisp" *load-truename*)) ; reverse-list
(load (merge-pathnames "skip.lisp" *load-truename*))

(defun drop (list drop)
    (cond 
        ((equal list nil) nil)
        ((< drop 0) nil)
        ((= drop 0) list)
        (t (reverse-list (skip (reverse-list list) drop)))
    ))

(assert (equal (drop '() 0) '()))
(assert (equal (drop '(A B C) 0) '(A B C)))
(assert (equal (drop '(A B C) 1) '(A B)))
(assert (equal (drop '(A B C) 2) '(A)))
(assert (equal (drop '(A B C) 3) '()))