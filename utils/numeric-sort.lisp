(load (merge-pathnames "swap.lisp" *load-truename*))
(load (merge-pathnames "../l-99/working-with-lists/p03.lisp" *load-truename*)) ; element-at
(load (merge-pathnames "../l-99/working-with-lists/p04.lisp" *load-truename*)) ; element-number

(defun numeric-sort (list) 
    (cond
        ((equal list nil) nil)
        ((< (element-number list) 2) list)
        (t (numeric-sort-fun list 1 2))
    ))

(defun numeric-sort-fun (list compared current) 
    (cond
        ((= compared (element-number list)) list)
        ((> current (element-number list)) (numeric-sort-fun list (+ compared 1) (+ compared 2)))
        ((> (element-at list compared) (element-at list current)) (numeric-sort-fun (swap list compared current) compared (+ current 1)))
        (t (numeric-sort-fun list compared (+ current 1)))
    ))

(assert (equal (numeric-sort '()) '()))
(assert (equal (numeric-sort '(1 2 3)) '(1 2 3)))
(assert (equal (numeric-sort '(3 2 1)) '(1 2 3)))
(assert (equal (numeric-sort '(1 3 2)) '(1 2 3)))
(assert (equal (numeric-sort '(3 1 2)) '(1 2 3)))
(assert (equal (numeric-sort '(2 1 3)) '(1 2 3)))
(assert (equal (numeric-sort '(2 1 3)) '(1 2 3)))
(assert (equal (numeric-sort '(10 2 1 11 4 5 9 6 3 7 8)) '(1 2 3 4 5 6 7 8 9 10 11)))
(assert (equal (numeric-sort '(1 2 3 3 2 1 2 1 2 3 2)) '(1 1 1 2 2 2 2 2 3 3 3)))
