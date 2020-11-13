;;;; Calculate Euler's totient function phi(m) (improved).
;;;; ---
;;;; See problem P34 for the definition of Euler's totient function. 
;;;; If the list of the prime factors of a number m is known in the form of problem P36 then the function phi(m) can be efficiently calculated as follows: 
;;;; Let ((p1 m1) (p2 m2) (p3 m3) ...) be the list of prime factors (and their multiplicities) of a given number m. 
;;;; Then phi(m) can be calculated with the following formula:
;;;; phi(m) = (p1 - 1) * p1 ** (m1 - 1) + (p2 - 1) * p2 ** (m2 - 1) + (p3 - 1) * p3 ** (m3 - 1) + ...
;;;; Note that a ** b stands for the b'th power of a.

;;;; This is actually wrong, the correct formula to calculate euler's totient function is:
;;;; phi(m) = (p1 - 1) * p1 ** (m1 - 1) * (p2 - 1) * p2 ** (m2 - 1) * (p3 - 1) * p3 ** (m3 - 1) * ...

(load (merge-pathnames "../arithmetic/p36.lisp" *load-truename*)) ; prime-factors-improved
(load (merge-pathnames "../../utils/pow.lisp" *load-truename*))

(defun phi-improved (m) 
    (cond 
        ((< m 1) 0)
        ((= m 1) 1)
        (t (phi-improved-fun (prime-factors-improved m)))
    ))

(defun phi-improved-fun (prime-factors) 
    (cond 
        ((equal prime-factors nil) 1)
        (t 
            (* 
                (* 
                    (- (car (car prime-factors)) 1) 
                    (pow (car (car prime-factors)) (- (car (cdr (car prime-factors))) 1))
                ) 
                (phi-improved-fun (cdr prime-factors))
            ))
    ))

(assert (equal (phi-improved -1) 0))
(assert (equal (phi-improved 0) 0))
(assert (equal (phi-improved 1) 1))
(assert (equal (phi-improved 5) 4))
(assert (equal (phi-improved 6) 2))
(assert (equal (phi-improved 7) 6))
(assert (equal (phi-improved 8) 4))
(assert (equal (phi-improved 9) 6))
(assert (equal (phi-improved 10) 4))
(assert (equal (phi-improved 11) 10))