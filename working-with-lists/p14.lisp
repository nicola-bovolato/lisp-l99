(defun reverse-list (list) 
    (cond 
        ((equal list nil) nil)
        (t (reverse-list-fun list '()))
    ))

(defun reverse-list-fun (list reversed)
    (cond
        ((equal list nil) reversed)
        (t (reverse-list-fun (cdr list) (cons (car list) reversed)))
    ))

(defun duplicate-list (list)
    (cond
        ((equal list nil) nil)
        (t (reverse-list (duplicate-list-fun list '())))
    ))

(defun duplicate-list-fun (list duplicated)
    (cond
        ((equal list nil) duplicated)
        (t (duplicate-list-fun (cdr list) (cons (car list) (cons (car list) duplicated))))
    ))

(format t "~S ~%" (duplicate-list nil)  )
(format t "~S ~%" (duplicate-list '())  ) 
(format t "~S ~%" (duplicate-list '(A))  ) 
(format t "~S ~%" (duplicate-list '(A B C D))  ) 
(format t "~S ~%" (duplicate-list '(A A B A A C D))  ) 