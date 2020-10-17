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

(defun encode-list (list)
    (cond
        ((equal list nil) nil)
        (t (reverse-list (encode-list-fun list '() nil 0)))
    ))

(defun encode-list-fun (list encoded element count)
    (cond
        ((equal list nil) (cond
                ((= count 1) (cons element encoded))
                (t (cons (cons count (cons element nil)) encoded))
            ))
        ((equal element (car list)) (encode-list-fun (cdr list) encoded element (+ count 1)))
        (t 
            (cond
                ((equal element nil) (encode-list-fun list encoded (car list) 0))
                ((= count 1) (encode-list-fun list (cons element encoded) nil 0))
                (t (encode-list-fun list (cons (cons count (cons element nil)) encoded) nil 0))
            )
        ) 
    ))

(format t "~S ~%" (encode-list nil)  )
(format t "~S ~%" (encode-list '())  ) 
(format t "~S ~%" (encode-list '(A A A))  ) 
(format t "~S ~%" (encode-list '(A B C D))  ) 
(format t "~S ~%" (encode-list '(A A B A A C D))  ) 
(format t "~S ~%" (encode-list '(A B C D D D D))  ) 
(format t "~S ~%" (encode-list '(A A A B B B B B C C D C))  ) 