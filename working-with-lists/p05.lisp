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

(assert (equal (reverse-list '()) '()))
(assert (equal (reverse-list '(A)) '(A))) 
(assert (equal (reverse-list '(A B)) '(B A)))
(assert (equal (reverse-list '(A B C)) '(C B A)))
(assert (equal (reverse-list '(A B C D)) '(D C B A)))