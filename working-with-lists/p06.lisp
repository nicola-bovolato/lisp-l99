(load (merge-pathnames "p03.lisp" *load-truename*)) ;; element-at
(load (merge-pathnames "p04.lisp" *load-truename*)) ;; element-number

(defun is-palindrome (list) 
    (cond
        ((equal list nil) nil)
        (t (is-palindrome-fun list 1 (element-number list)))
    )
)

(defun is-palindrome-fun (list start end)
    (cond 
        ((equal start end) t)
        ((> start end) t)
        ((equal (element-at list start) (element-at list end)) (is-palindrome-fun list (+ start 1) (- end 1)))
        (t nil) 
    ))

(assert (equal (is-palindrome '(A)) t)) 
(assert (equal (is-palindrome '(A A)) t)) 
(assert (equal (is-palindrome '(A B C)) nil)) 
(assert (equal (is-palindrome '(A B B A)) t)) 
(assert (equal (is-palindrome '(A B C D)) nil)) 
(assert (equal (is-palindrome '(A B C B A)) t)) 