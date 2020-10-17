(defun last-element (list) 
    (cond 
        ((equal (cdr list) nil) (car list))
        (t (last-element (cdr list))) 
    )
)

(format t "~S ~%" (last-element nil) )
(format t "~S ~%" (last-element '()) )
(format t "~S ~%" (last-element '(A)) ) 
(format t "~S ~%" (last-element '(A B)) ) 
(format t "~S ~%" (last-element '(A B C)) )