(defun abs-n (n) 
    (cond 
        ((>= n 0) n)
        (t (- 0 n))
    ))

(assert (equal (abs-n 1) 1))
(assert (equal (abs-n 0) 0))
(assert (equal (abs-n -1) 1))