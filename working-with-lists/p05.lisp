(defun reverse-list (list) 
    (cond 
        ((equal list nil) nil)
        (t (reverse-list-fun list '()))
    )
)

(defun reverse-list-fun (list reversed)
    (cond
        ((equal list nil) reversed)
        (t (reverse-list-fun (cdr list) (cons (car list) reversed)))
    )
)

(format t "~S ~%" (reverse-list nil)  )
(format t "~S ~%" (reverse-list '())  )
(format t "~S ~%" (reverse-list '(A))  ) 
(format t "~S ~%" (reverse-list '(A B))  )
(format t "~S ~%" (reverse-list '(A B C))  )
(format t "~S ~%" (reverse-list '(A B C D))  )