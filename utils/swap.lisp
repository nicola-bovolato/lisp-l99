(load (merge-pathnames "../l-99/working-with-lists/p03.lisp" *load-truename*)) ; element-at
(load (merge-pathnames "../l-99/working-with-lists/p20.lisp" *load-truename*)) ; remove-at
(load (merge-pathnames "../l-99/working-with-lists/p21.lisp" *load-truename*)) ; insert-at

(defun swap (list n1 n2) 
    (insert-at (element-at list n1) 
        (remove-at (insert-at 
            (element-at list n2) 
                (remove-at list n1) 
            n1) 
        n2) 
    n2))