(defun last-element (list) 
    (cond 
        ((equal (cdr list) nil) (car list))
        (t (last-element (cdr list))) 
    ))

(assert (equal (last-element '()) nil))
(assert (equal (last-element '(A)) 'A))
(assert (equal (last-element '(A B)) 'B))
(assert (equal (last-element '(A B C)) 'C))