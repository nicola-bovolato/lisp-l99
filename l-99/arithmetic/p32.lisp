;;;; Determine the greatest common divisor of two positive integer numbers.
;;;; Use Euclid's algorithm.

(load (merge-pathnames "../../utils/modulus.lisp" *load-truename*))

(defun great-common-divisor (n1 n2) 
    (cond
        ((<= n1 0) 1)
        ((<= n2 0) 1)
        (t (great-common-divisor-fun n1 n2))
    ))

(defun great-common-divisor-fun (n1 n2) 
    (cond
        ((= n2 0) n1)
        (t (great-common-divisor-fun n2 (modulus n1 n2)))
    ))

(assert (equal (great-common-divisor 1 10) 1))
(assert (equal (great-common-divisor 10 1) 1))
(assert (equal (great-common-divisor 10 10) 10))
(assert (equal (great-common-divisor 25 5) 5))
(assert (equal (great-common-divisor 5 25) 5))
(assert (equal (great-common-divisor 30 18) 6))
(assert (equal (great-common-divisor 18 30) 6))
(assert (equal (great-common-divisor 0 0) 1))
(assert (equal (great-common-divisor 0 18) 1))
(assert (equal (great-common-divisor 18 0) 1))
(assert (equal (great-common-divisor -18 30) 1))