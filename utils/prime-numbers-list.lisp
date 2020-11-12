(load (merge-pathnames "../l-99/working-with-lists/p05.lisp" *load-truename*)) ; reverse-list
(load (merge-pathnames "../l-99/arithmetic/p31.lisp" *load-truename*)) ; is-prime

(defun prime-numbers-list (start end) 
    (cond
        ((< start 1) nil)
        ((< end 1) nil)
        ((> start end) (reverse-list (prime-numbers-list-fun end start)))
        (t (prime-numbers-list-fun start end))
    ))

(defun prime-numbers-list-fun (current end)
    (cond
        ((> current end) nil)
        ((is-prime current) (cons current (prime-numbers-list-fun (+ current 1) end)))
        (t (prime-numbers-list-fun (+ current 1) end))
    ))

(assert (equal (prime-numbers-list 1 -1) nil))
(assert (equal (prime-numbers-list -1 1) nil))
(assert (equal (prime-numbers-list 1 1) '(1)))
(assert (equal (prime-numbers-list 1 2) '(1 2)))
(assert (equal (prime-numbers-list 1 3) '(1 2 3)))
(assert (equal (prime-numbers-list 3 1) '(3 2 1)))
(assert (equal (prime-numbers-list 1 10) '(1 2 3 5 7)))
(assert (equal (prime-numbers-list 1 20) '(1 2 3 5 7 11 13 17 19)))
(assert (equal (prime-numbers-list 7 20) '(7 11 13 17 19)))
(assert (equal (prime-numbers-list 8 20) '(11 13 17 19)))