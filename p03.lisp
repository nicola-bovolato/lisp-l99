;; Find the K'th element of a list

(defun element-at (elements n)
    (cond
        ((< n 1) (error "out of bounds"))
        ((equal (first elements) nil) (error "out of bounds"))
        ((= n 1) (first elements))
        (T (element-at (rest elements) (- n 1) ))))

(format t "~a ~a ~C~%" "Element at 1:" (element-at '(A B C) 1) #\return)
(format t "~a ~a ~C~%" "Element at 2:" (element-at '(A B C) 2) #\return)
(format t "~a ~a ~C~%" "Element at 3:" (element-at '(A B C) 3) #\return)