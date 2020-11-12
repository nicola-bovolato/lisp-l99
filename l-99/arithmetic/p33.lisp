;;;; Determine whether two positive integer numbers are coprime.
;;;; Two numbers are coprime if their greatest common divisor equals 1.

(load (merge-pathnames "p32.lisp" *load-truename*)) ; great-common-divisor

(defun coprime (n1 n2) 
    (cond
        ((= (great-common-divisor-fun n1 n2) 1) t)
        (t nil)
    ))

(assert (equal (coprime 1 10) t))
(assert (equal (coprime 2 10) nil))
(assert (equal (coprime 10 10) nil))
(assert (equal (coprime 6 35) t))
(assert (equal (coprime 35 64) t))