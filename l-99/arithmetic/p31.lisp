;;;; Determine whether a given integer number is prime.

(load (merge-pathnames "../../utils/modulus.lisp" *load-truename*))

(defun is-prime (n) 
    (cond 
        ((< n 1) nil)
        ((= n 1) t)
        (t (is-prime-fun n (- n 1)))
    ))

(defun is-prime-fun (n divisor) 
    (cond
        ((= divisor 1) t)
        ((= (modulus n divisor) 0) nil)
        (t (is-prime-fun n (- divisor 1)))
    ))

(assert (equal (is-prime 1) t))
(assert (equal (is-prime 2) t))
(assert (equal (is-prime 3) t))
(assert (equal (is-prime 4) nil))
(assert (equal (is-prime 5) t))
(assert (equal (is-prime 6) nil))
(assert (equal (is-prime 7) t))
(assert (equal (is-prime 8) nil))
(assert (equal (is-prime 9) nil))
(assert (equal (is-prime 10) nil))
(assert (equal (is-prime 11) t))
(assert (equal (is-prime 271) t))
(assert (equal (is-prime 300) nil))