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

(defun drop-nth-element (list skip)
    (cond
        ((equal list nil) nil)
        ((< skip 0) nil)
        (t (reverse-list (drop-nth-element-fun list skip '() (- skip 1))))
    ))

(defun drop-nth-element-fun (list skip dropped count)
    (cond
        ((equal list nil) dropped)
        ((= count 0) (drop-nth-element-fun (cdr list) skip dropped (- skip 1)))
        (t (drop-nth-element-fun (cdr list) skip (cons (car list) dropped) (- count 1)))
    ))

(format t "~S ~%" (drop-nth-element nil 0)  )
(format t "~S ~%" (drop-nth-element '() 1)  ) 
(format t "~S ~%" (drop-nth-element '(A B C D) 1)  ) 
(format t "~S ~%" (drop-nth-element '(A) 2)  ) 
(format t "~S ~%" (drop-nth-element '(A B C D) 3)  ) 
(format t "~S ~%" (drop-nth-element '(A B C D E F G H I) 3)  ) 
(format t "~S ~%" (drop-nth-element '(A A B B C C D D) 2)  ) 