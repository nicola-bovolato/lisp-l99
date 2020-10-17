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
(defun element-at (list n) 
    (cond
        ((equal list nil) nil)
        ((<= n 0) nil) 
        ((= n 1) (car list))
        ((> n 1) (element-at (cdr list) (- n 1)))
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

(defun slice-list (list start end)
    (cond
        ((equal list nil) nil)
        ((< start 1) nil)
        ((> end (element-number list)) nil)
        ((> start end) nil)
        ((= start end) (cons (element-at list start) nil))
        (t (skip (drop list (- (element-number list) end)) (- start 1)))
    )
)

(format t "~S ~%" (slice-list nil 0 0))
(format t "~S ~%" (slice-list '() 1 0)) 
(format t "~S ~%" (slice-list '(A B C D) 2 2)) 
(format t "~S ~%" (slice-list '(A B C D E F G H I) 3 7)) 