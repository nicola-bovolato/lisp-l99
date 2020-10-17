(defun last-but-one-element (list) 
    (cond 
        ((equal (cdr list) nil) nil)
        ((equal (cdr (cdr list)) nil) (car list))
        (t (last-but-one-element (cdr list))) 
    )
)

(format t "~S ~%" (last-but-one-element nil) )
(format t "~S ~%" (last-but-one-element '()) )
(format t "~S ~%" (last-but-one-element '(A)) ) 
(format t "~S ~%" (last-but-one-element '(A B)) )
(format t "~S ~%" (last-but-one-element '(A B C)) )
(format t "~S ~%" (last-but-one-element '(A B C D)) )