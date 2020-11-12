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