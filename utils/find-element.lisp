(defun find-element (list element)
    (find-element-fun list element 0))

(defun find-element-fun (list element count)
    (cond
        ((equal list nil) count)
        ((equal (car list) element) (+ count 1))
        (t (find-element-fun (cdr list) element (+ count 1)))
    ))