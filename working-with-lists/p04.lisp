(defun element-number (list)
    (cond 
        ((equal list nil) 0)
        (t (+ 1 (element-number (cdr list))))
    )
)

(format t "~S ~%" (element-number nil) )
(format t "~S ~%" (element-number '()) )
(format t "~S ~%" (element-number '(A)) ) 
(format t "~S ~%" (element-number '(A B)) )
(format t "~S ~%" (element-number '(A B C)) )