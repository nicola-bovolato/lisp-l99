(defun integer-division (dividend divisor)
    (cond
        ((= divisor 0) nil)
        ((< dividend 0) (cond 
            ((> divisor 0) (- 0 (integer-division-fun (- 0 dividend) divisor)))
            (t (integer-division-fun (- 0 dividend) (- 0 divisor)))
        ))
        ((< divisor 0) (cond 
            ((> dividend 0) (- 0 (integer-division-fun dividend (- 0 divisor))))
            (t (integer-division-fun (- 0 dividend) (- 0 divisor)))
        ))
        (t (integer-division-fun dividend divisor))
    ))

(defun integer-division-fun (dividend divisor)
    (cond
        ((= dividend 0) 0)
        ((< dividend divisor) 0)
        (t (+ 1 (integer-division-fun (- dividend divisor) divisor)))
    ))

(assert (equal (integer-division 1 0) nil))
(assert (equal (integer-division 0 1) 0))
(assert (equal (integer-division 1 1) 1))
(assert (equal (integer-division 5 1) 5))
(assert (equal (integer-division 5 2) 2))
(assert (equal (integer-division 5 10) 0))
(assert (equal (integer-division -5 -1) 5))
(assert (equal (integer-division -5 1) -5))
(assert (equal (integer-division 5 -1) -5))