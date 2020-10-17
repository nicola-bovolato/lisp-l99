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

(defun element-number (list)
    (cond 
        ((equal list nil) 0)
        (t (+ 1 (element-number (cdr list))))
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

(defun split-list (list length)
    (cond
        ((equal list nil) nil)
        ((<= length 0) list)
        ((> length (element-number list)) nil)
        (t (cons (drop list (- (element-number list) length)) (cons (skip list length) nil)))
    ))

(format t "~S ~%" (split-list nil 0)  )
(format t "~S ~%" (split-list '() 1)  ) 
(format t "~S ~%" (split-list '(A B C D) 1)  ) 
(format t "~S ~%" (split-list '(A) 2)  ) 
(format t "~S ~%" (split-list '(A B C D) 3)  ) 
(format t "~S ~%" (split-list '(A B C D E F G H I) 3) ) 
(format t "~S ~%" (split-list '(A A B B C C D D) 2)  ) 