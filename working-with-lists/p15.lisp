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

(defun replicate-list (list times)
    (cond
        ((equal list nil) nil)
        ((< times 0) nil)
        (t (reverse-list (replicate-list-fun list times '() times)))
    ))

(defun replicate-list-fun (list times replicated count)
    (cond
        ((equal list nil) replicated)
        ((= count 0) (replicate-list-fun (cdr list) times replicated times))
        (t (replicate-list-fun list times (cons (car list) replicated) (- count 1)))
    ))

(format t "~S ~%" (replicate-list nil 0)  )
(format t "~S ~%" (replicate-list '() 1)  ) 
(format t "~S ~%" (replicate-list '(A) 2)  ) 
(format t "~S ~%" (replicate-list '(A B C D) 3)  ) 
(format t "~S ~%" (replicate-list '(A A B A A C D) 2)  ) 