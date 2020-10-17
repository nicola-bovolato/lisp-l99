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

(defun repeat-element (element times)
    (cond
        ((<= times 0) nil)
        (t (repeat-element-fun element times '()))
    ))

(defun repeat-element-fun (element times repeated)
    (cond
        ((= times 0) repeated)
        (t (repeat-element-fun element (- times 1) (cons element repeated)))
    ))

(defun skip (list skip)
    (cond 
        ((equal list nil) nil)
        ((< skip 0) nil)
        ((= skip 0) list)
        (t (skip (cdr list) (- skip 1)))
    ))

(defun pack-list (list) 
    (cond
        ((equal list nil) nil)
        (t (reverse-list (pack-list-fun list '())))
    ))

(defun pack-list-fun (list packed)
    (cond
        ((equal list nil) packed)
        (t (pack-list-fun (skip list (pack-list-count-fun list 1)) (cons (repeat-element (car list) (pack-list-count-fun list 1)) packed)))
    ))

(defun pack-list-count-fun (list count)
    (cond
        ((equal (cdr list) nil) count)
        ((equal (car list) (car (cdr list))) (pack-list-count-fun (cdr list) (+ count 1)))
        (t count)
    ))

(format t "~S ~%" (pack-list nil)  )
(format t "~S ~%" (pack-list '())  ) 
(format t "~S ~%" (pack-list '(A A A ))  ) 
(format t "~S ~%" (pack-list '(A B C D))  ) 
(format t "~S ~%" (pack-list '(A A B A A C D))  ) 
(format t "~S ~%" (pack-list '(A B C D D D D))  ) 
(format t "~S ~%" (pack-list '(A A A B B B B B C C D C))  ) 