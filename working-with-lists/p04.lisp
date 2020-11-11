(defun element-number (list)
    (cond 
        ((equal list nil) 0)
        (t (+ 1 (element-number (cdr list))))
    ))

(assert (equal (element-number '()) 0))
(assert (equal (element-number '(A)) 1)) 
(assert (equal (element-number '(A B)) 2))
(assert (equal (element-number '(A B C)) 3))