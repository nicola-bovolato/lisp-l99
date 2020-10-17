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
    ))

(defun flatten-list-fun (list flatten) 
    (cond
        ((equal list nil) flatten)
        ((atom (car list)) (flatten-list-fun (cdr list) (cons (car list) flatten)))
        (t (flatten-list-fun (cdr list) (append-list (flatten-list-fun (car list) '()) flatten)))
    ))

(defun decode-list (list)
    (cond
        ((equal list nil) nil)
        (t (reverse-list (flatten-list (decode-list-fun list '()))))
    ))

(defun decode-list-fun (list decoded)
    (cond
        ((equal list nil) decoded)
        (t (decode-list-fun (cdr list) (cons (decode-decompress-list (car list)) decoded)))
    ))

(defun decode-decompress-list (list)
    (cond
        ((equal list nil) nil)
        ((atom list) list)
        (t (repeat-element (cdr list) (car list)))
    )
)

(format t "~S ~%" (decode-list nil)  )
(format t "~S ~%" (decode-list '())  ) 
(format t "~S ~%" (decode-list '(A ))  ) 
(format t "~S ~%" (decode-list '((2 A)))  ) 
(format t "~S ~%" (decode-list '(A B C))  ) 
(format t "~S ~%" (decode-list '((2 A) (3 B)))  ) 
(format t "~S ~%" (decode-list '((2 A) B (3 C)))  ) 
(format t "~S ~%" (decode-list '((2 A) B (3 C) D))  ) 
(format t "~S ~%" (decode-list '(A (2 B) C (3 D) E))  ) 
(format t "~S ~%" (decode-list '(A (2 B) C (3 D) (4 E)))  ) 