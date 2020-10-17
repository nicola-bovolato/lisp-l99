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

(defun append-list (list1 list2)
    (cond
        ((equal list1 nil) list2)
        ((equal list2 nil) list1)
        (t (append-list-fun (reverse-list list1) (reverse-list list2) '()))
    ))

(defun append-list-fun (list1 list2 list3) 
    (cond
        ((equal list2 nil)
            (cond 
                ((equal list1 nil) list3)
                (t (append-list-fun (cdr list1) list2 (cons (car list1) list3)))
            ))
        (t (append-list-fun list1 (cdr list2) (cons (car list2) list3)))
    ))

(defun flatten-list (list) 
    (cond
        ((equal list nil) nil)
        (t (reverse-list (flatten-list-fun list '())))
    )
)

(defun flatten-list-fun (list flatten) 
    (cond
        ((equal list nil) flatten)
        ((atom (car list)) (flatten-list-fun (cdr list) (cons (car list) flatten)))
        (t (flatten-list-fun (cdr list) (append-list (flatten-list-fun (car list) '()) flatten)))
    )
)


(format t "~S ~%" (flatten-list nil)  ) 
(format t "~S ~%" (flatten-list '())  ) 
(format t "~S ~%" (flatten-list '(A))  ) 
(format t "~S ~%" (flatten-list '(A (B C)))  ) 
(format t "~S ~%" (flatten-list '(A (B (C))))  ) 
(format t "~S ~%" (flatten-list '(A ((B C) D) E (F G) ))  ) 