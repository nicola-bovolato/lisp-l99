(defun find-element (list element)
    (find-element-fun list element 0))

(defun find-element-fun (list element count)
    (cond
        ((equal list nil) 0)
        ((equal (car list) element) (+ count 1))
        (t (find-element-fun (cdr list) element (+ count 1)))
    ))

(assert (equal (find-element '() 0) 0))
(assert (equal (find-element '(A B C) 'A) 1))
(assert (equal (find-element '(A B C) 'B) 2))
(assert (equal (find-element '(A B C) 'C) 3))
(assert (equal (find-element '(A B C) 'D) 0))
(assert (equal (find-element '(A B C A) 'A) 1))