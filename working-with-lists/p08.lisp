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

(defun compress-list (list) 
    (cond
        ((equal list nil) nil)
        (t (reverse-list (compress-list-fun list '())))
    ))

(defun compress-list-fun (list compressed)
    (cond
        ((equal list nil) compressed)
        ((equal (car list) (car (cdr list))) (compress-list-fun (cdr list) compressed))
        (t (compress-list-fun (cdr list) (cons (car list) compressed)))
    ))

;; (defun find-element (list element) 
;;     (cond
;;         ((equal list nil) nil)
;;         ((equal (car list) element) t)
;;         (t (find-element (cdr list) element))
;;     )
;; )


(format t "~S ~%" (compress-list nil)  )
(format t "~S ~%" (compress-list '())  ) 
(format t "~S ~%" (compress-list '(A))  ) 
(format t "~S ~%" (compress-list '(A B C D))  ) 
(format t "~S ~%" (compress-list '(A A B A A C D))  ) 
(format t "~S ~%" (compress-list '(A B C D D D D))  ) 
(format t "~S ~%" (compress-list '(A A A B B B B B C C D C))  ) 