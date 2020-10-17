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
(defun skip (list skip)
    (cond 
        ((equal list nil) nil)
        ((< skip 0) nil)
        ((= skip 0) list)
        (t (skip (cdr list) (- skip 1)))
    ))

(defun drop (list drop)
    (cond 
        ((equal list nil) nil)
        ((< drop 0) nil)
        ((= drop 0) list)
        (t (reverse-list (skip (reverse-list list) drop)))
    ))

(defun element-number (list)
    (cond 
        ((equal list nil) 0)
        (t (+ 1 (element-number (cdr list))))
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


(defun insert-at (element list n)
    (cond 
        ((equal list nil) nil)
        ((< n 1) nil)
        ((> n (element-number list)) nil)
        ((= n 1) (cons element list))
        ((= n (element-number list)) (reverse-list (cons element (reverse-list list))))
        (t (append-list (drop list (- (element-number list) n)) (cons element (skip list n))))
    )
)

(format t "~S~%" (insert-at 'A nil 0 )  )
(format t "~S~%" (insert-at 'A '() 1 )  ) 
(format t "~S~%" (insert-at 'E '(A B C D) 4 )  ) 
(format t "~S~%" (insert-at 'A '(B C D E F ) 1)  ) 
(format t "~S~%" (insert-at 'F '(A B C D E G H I) 5)  ) 