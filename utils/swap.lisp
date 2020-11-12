(load (merge-pathnames "../l-99/working-with-lists/p03.lisp" *load-truename*)) ; element-at
(load (merge-pathnames "../l-99/working-with-lists/p20.lisp" *load-truename*)) ; remove-at
(load (merge-pathnames "../l-99/working-with-lists/p21.lisp" *load-truename*)) ; insert-at

(defun swap (list n1 n2) 
    (cond
        ((equal list nil) nil)
        ((= n1 n2) list)
        (t (swap-fun list (swap-calc-number n1 list) (swap-calc-number n2 list)))
    ))

(defun swap-calc-number (n list) 
    (cond
        ((< n 0) 1)
        ((> n (element-number list)) (element-number list))
        (t n)
    ))

(defun swap-fun (list n1 n2)
    (cond 
        ((= n1 n2) list)
        (t (insert-at (element-at list n1) 
            (remove-at (insert-at 
                (element-at list n2) (remove-at list n1) n1) 
                n2) 
            n2))
    ))

(assert (equal (swap '() 0 0) '()))
(assert (equal (swap '() 1 0) '()))
(assert (equal (swap '() -1 100) '()))
(assert (equal (swap '(A B C) 1 1) '(A B C)))
(assert (equal (swap '(A B C) 1 3) '(C B A)))
(assert (equal (swap '(A B C) 1 2) '(B A C)))
(assert (equal (swap '(A B C) -1 3) '(C B A)))
(assert (equal (swap '(A B C) 3 -1) '(C B A)))
(assert (equal (swap '(A B C) 1 100) '(C B A)))
(assert (equal (swap '(A B C D E F G H) 2 5) '(A E C D B F G H)))