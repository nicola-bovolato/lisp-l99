;; Find the last but one element of a list.

(defun last-but-one-element (elements)
    (cond
        ((equal (first elements) nil) (error "empty list"))
        ((equal (rest elements) nil) (error "not enough elements"))
        ((equal (rest (rest elements)) nil) (first elements))
        (T (last-but-one-element (rest elements)))))

(format t "~a ~a ~C~%" "Last But One Element:" (last-but-one-element '(A B)) #\return)
(format t "~a ~a ~C~%" "Last But One Element:" (last-but-one-element '(A B C)) #\return)
(format t "~a ~a ~C~%" "Last But One Element:" (last-but-one-element '(A B C D)) #\return)