;;;; Goldbach's conjecture.
;;;; ---
;;;; Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers.
;;;; Example: 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case. 
;;;; It has been numerically confirmed up to very large numbers (much larger than we can go with our Prolog system).
;;;; Write a predicate to find the two prime numbers that sum up to a given even integer.

(load (merge-pathnames "../working-with-lists/p04.lisp" *load-truename*)) ; element-number
(load (merge-pathnames "../../utils/prime-numbers-list.lisp" *load-truename*))
(load (merge-pathnames "../../utils/is-list-prime.lisp" *load-truename*))
(load (merge-pathnames "../../utils/sum-of-list.lisp" *load-truename*))

(defun goldbach (n)
    (cond
        ((< n 2) nil)
        ((= (modulus n 2) 1) nil)
        (t (goldbach-fun n (prime-numbers-list 1 n) (prime-numbers-list n 1)))
    ))

(defun goldbach-fun (n prime-numbers prime-numbers-reversed) 
    (cond
        ((equal prime-numbers-reversed nil) (goldbach-fun n (cdr prime-numbers) (prime-numbers-list n 1)))
        ((= (+ (car prime-numbers) (car prime-numbers-reversed)) n) 
            (cons (car prime-numbers) (cons (car prime-numbers-reversed) nil)))
        (t (goldbach-fun n prime-numbers (cdr prime-numbers-reversed)))
    ))

(defun test-goldbach (n)
    (cond 
        ((= (modulus n 2) 1) (assert (equal (goldbach n) nil)))
        (t (assert 
            (and 
                (and (equal (sum-of-list (goldbach n)) n) (is-list-prime (goldbach n))) 
                (equal (element-number (goldbach n)) 2))
        ))
    ))

(test-goldbach 6)
(test-goldbach 3)
(test-goldbach 8)
(test-goldbach 28)
(test-goldbach 35)
(test-goldbach 36)