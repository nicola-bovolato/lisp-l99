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

(assert (equal (repeat-element nil 2) '(nil nil)))
(assert (equal (repeat-element 'A 0) '()))
(assert (equal (repeat-element 'A -1) '()))
(assert (equal (repeat-element 'A 1) '(A)))
(assert (equal (repeat-element 'A 3) '(A A A)))
(assert (equal (repeat-element '(A B C) 1) '((A B C))))
(assert (equal (repeat-element '(A B C) 4) '((A B C) (A B C) (A B C) (A B C))))