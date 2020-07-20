;; Reverse a list

(defun reverse-elements (elements)
    (cond
        ((equal elements nil) (error "empty list"))
        ((equal (rest elements) nil) (cons (first elements) nil))
        (T (append (reverse-elements (rest elements)) (cons (first elements) nil)))))

(format t "~a ~a ~C~%" "Reverse:" (reverse-elements '(A)) #\return)
(format t "~a ~a ~C~%" "Reverse:" (reverse-elements '(A B)) #\return)
(format t "~a ~a ~C~%" "Reverse:" (reverse-elements '(A B C)) #\return)