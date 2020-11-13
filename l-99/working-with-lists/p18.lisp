;;;; Extract a slice from a list.
;;;; ---
;;;; Given two indices, I and K, the slice is the list containing the elements between the I'th and K'th element of the original list (both limits included). 
;;;; Start counting the elements with 1.

(load (merge-pathnames "p03.lisp" *load-truename*)) ; element-at
(load (merge-pathnames "p04.lisp" *load-truename*)) ; element-number
(load (merge-pathnames "p05.lisp" *load-truename*)) ; reverse-list
(load (merge-pathnames "../../utils/skip.lisp" *load-truename*))
(load (merge-pathnames "../../utils/drop.lisp" *load-truename*))

(defun slice-list (list start end)
    (cond
        ((equal list nil) nil)
        ((< start 1) (slice-list list 1 end))
        ((> end (element-number list)) (slice-list list start (element-number list)))
        ((> start end) (slice-list list end start))
        ((= start end) (cons (element-at list start) nil))
        (t (skip (drop list (- (element-number list) end)) (- start 1)))
    ))

(assert (equal (slice-list '() 1 0) nil))
(assert (equal (slice-list '(A B C D) 2 2) '(B)))
(assert (equal (slice-list '(A B C D E F G H I) 3 7) '(C D E F G))) 
(assert (equal (slice-list '(A B C D E F G H I) 7 3) '(C D E F G))) 
(assert (equal (slice-list '(A B C D E F G H I) 3 9) '(C D E F G H I))) 
(assert (equal (slice-list '(A B C D E F G H I) -1 7) '(A B C D E F G))) 
(assert (equal (slice-list '(A B C D E F G H I) 1 7) '(A B C D E F G))) 