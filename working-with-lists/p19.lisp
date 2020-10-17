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


(defun last-element (list) 
    (cond 
        ((equal (cdr list) nil) (car list))
        (t (last-element (cdr list))) 
    ))

(defun rotate-list (list n)
    (cond
        ((equal list nil) nil)
        ((< n 0) nil)
        ((= n 0) list)
        (t (rotate-list (cons (last-element list) (drop list 1)) (- n 1)))   
    )
)

(format t "~S~%" (rotate-list nil 0 )  )
(format t "~S~%" (rotate-list '() 1 )  ) 
(format t "~S~%" (rotate-list '(A B C D) 2 )  ) 
(format t "~S~%" (rotate-list '(A B C D E F G H I) 4)  ) 