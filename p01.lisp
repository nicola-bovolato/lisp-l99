;; Find the last element of a list.

(defun last-element (elements)
    (cond
        ((equal (first elements) nil) (error "empty list"))
        ((equal (rest elements) nil) (first elements))
        (T (last-element (rest elements)))))

(format t "~a ~a ~C~%" "Last Element:" (last-element '(A)) #\return)
(format t "~a ~a ~C~%" "Last Element:" (last-element '(A B)) #\return)
(format t "~a ~a ~C~%" "Last Element:" (last-element '(A B C)) #\return)