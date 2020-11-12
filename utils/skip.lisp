(defun skip (list skip)
    (cond 
        ((equal list nil) nil)
        ;; ((< skip 0) nil)
        ((<= skip 0) list)
        (t (skip (cdr list) (- skip 1)))
    ))

(assert (equal (skip '() 0) nil))
(assert (equal (skip '(A B C) 0) '(A B C)))
(assert (equal (skip '(A B C) -1) '(A B C)))
(assert (equal (skip '(A B C) 1) '(B C)))
(assert (equal (skip '(A B C) 2) '(C)))
(assert (equal (skip '(A B C) 3) '()))