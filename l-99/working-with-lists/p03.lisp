;;;; Find the K'th element of a list.
;;;; The first element in the list is number 1

(defun element-at (list n) 
    (cond
        ((equal list nil) nil)
        ((<= n 0) nil) 
        ((= n 1) (car list))
        ((> n 1) (element-at (cdr list) (- n 1)))
    ))

(assert (equal (element-at '() 0) nil))
(assert (equal (element-at '(A) -1) nil)) 
(assert (equal (element-at '(A) 0) nil)) 
(assert (equal (element-at '(A) 1) 'A))
(assert (equal (element-at '(A B) 2) 'B))
(assert (equal (element-at '(A B C D E) -1) nil))
(assert (equal (element-at '(A B C D E) 1) 'A))
(assert (equal (element-at '(A B C D E) 3) 'C))
(assert (equal (element-at '(A B C D E) 5) 'E))