;; Find the numbers of elements of a list

(defun element-number (elements)
    (cond
        ((equal elements nil) 0)
        (T (+ 1 (element-number (rest elements))))))

(format t "~a ~a ~C~%" "Length:" (element-number '()) #\return)
(format t "~a ~a ~C~%" "Length:" (element-number '(A)) #\return)
(format t "~a ~a ~C~%" "Length:" (element-number '(A B)) #\return)