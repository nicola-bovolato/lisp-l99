;;;; Calculate Euler's totient function phi(m).
;;;; ---
;;;; Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.

(load (merge-pathnames "p33.lisp" *load-truename*)) ; coprime

(defun phi (m) 
    (cond 
        ((< m 1) 0)
        ((= m 1) 1)
        (t (phi-fun m (- m 1)))
    ))

(defun phi-fun (m current) 
    (cond 
        ((= current 1) 1)
        ((equal (coprime current m) t) (+ 1 (phi-fun m (- current 1))))
        (t (phi-fun m (- current 1)))
    ))

(assert (equal (phi -1) 0))
(assert (equal (phi 0) 0))
(assert (equal (phi 1) 1))
(assert (equal (phi 5) 4))
(assert (equal (phi 6) 2))
(assert (equal (phi 7) 6))
(assert (equal (phi 8) 4))
(assert (equal (phi 9) 6))
(assert (equal (phi 10) 4))
(assert (equal (phi 11) 10))