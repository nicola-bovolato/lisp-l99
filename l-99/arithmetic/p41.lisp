(load (merge-pathnames "p40.lisp" *load-truename*)) ; goldbach
(load (merge-pathnames "../working-with-lists/p05.lisp" *load-truename*)) ; reverse-list

(defun goldbach-list (start end) 
    (cond
        ((< start 2) nil)
        ((> start end) (reverse-list (goldbach-list end start)))
        (t (goldbach-list-fun start end ))
    ))

(defun goldbach-list-fun (start end) 
    (cond
        ((> start end) nil)
        ((= (modulus start 2) 1) (goldbach-list-fun (+ start 1) end)) 
        (t (cons (goldbach start) (goldbach-list-fun (+ start 1) end))) 
    ))

(defun test-goldbach-list (start end)
    (cond
        ((or (< start 2) (< end 2)) (assert (equal (goldbach-list start end) nil)))
        (t (assert (equal (length (goldbach-list start end)) (test-number-of-even (min start end) (max start end)))))   
    ))

(defun test-number-of-even (start end) 
    (+ (floor (- end start) 2) (mod (- end start) 2) (- 1 (mod start 2))))
    
(test-goldbach-list 2 10)
(test-goldbach-list 3 30)
(test-goldbach-list 50 10)
(test-goldbach-list 13 17)