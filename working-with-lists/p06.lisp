(defun element-number (list)
    (cond 
        ((equal list nil) 0)
        (t (+ 1 (element-number (cdr list))))
    ))

(defun element-at (list n) 
    (cond
        ((equal list nil) nil)
        ((<= n 0) nil) 
        ((= n 1) (car list))
        ((> n 1) (element-at (cdr list) (- n 1)))
    )
)


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
    )
)

(format t "~S ~%" (is-palindrome '(A))  ) 
(format t "~S ~%" (is-palindrome '(A A))  ) 
(format t "~S ~%" (is-palindrome '(A B C))  ) 
(format t "~S ~%" (is-palindrome '(A B B A))  ) 
(format t "~S ~%" (is-palindrome '(A B C D))  ) 
(format t "~S ~%" (is-palindrome '(A B C B A))  ) 