;;;; A list of Goldbach compositions.
;;;; ----
;;;; Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.
;;;; In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. 
;;;; Very rarely, the primes are both bigger than say 50.
;;;; Try to find out how many such cases there are in the range 2..3000.

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

;; the functions are not optimized and it take ages to run this (~6 minutes)
(goldbach-list 2 3000) 