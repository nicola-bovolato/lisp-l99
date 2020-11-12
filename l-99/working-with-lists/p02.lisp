;;;; Find the last but one box of a list.

(defun last-but-one-element (list) 
    (cond 
        ((equal (cdr list) nil) nil)
        ((equal (cdr (cdr list)) nil) (car list))
        (t (last-but-one-element (cdr list))) 
    ))

(assert (equal (last-but-one-element '()) nil))
(assert (equal (last-but-one-element '(A)) nil))
(assert (equal (last-but-one-element '(A B)) 'A))
(assert (equal (last-but-one-element '(A B C)) 'B))
(assert (equal (last-but-one-element '(A B C D)) 'C))