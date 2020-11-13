
(load (merge-pathnames "../l-99/arithmetic/p31.lisp" *load-truename*)) ; is-prime

(defun is-list-prime (list)
    (cond
        ((equal list nil) nil)
        (t (is-list-prime-fun list))
    ))

(defun is-list-prime-fun (list)
    (cond
        ((equal list nil) t)
        ((is-prime (car list)) (is-list-prime-fun (cdr list)))
        (t nil)
    ))

(assert (equal (is-list-prime '()) nil))
(assert (equal (is-list-prime '(1 2 3)) t))
(assert (equal (is-list-prime '(0 0 0)) nil))
(assert (equal (is-list-prime '(100 200)) nil))
(assert (equal (is-list-prime '(100 -200)) nil))