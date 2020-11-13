(defun sum-of-list (list)
    (cond
        ((equal list nil) 0)
        (t (+ (car list) (sum-of-list (cdr list))))
    ))

(assert (equal (sum-of-list '()) 0))
(assert (equal (sum-of-list '(1 2 3)) 6))
(assert (equal (sum-of-list '(0 0 0)) 0))
(assert (equal (sum-of-list '(100 200)) 300))
(assert (equal (sum-of-list '(100 -200)) -100))