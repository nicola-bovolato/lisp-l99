;;;; Extract a given number of randomly selected elements from a list.
;;;; ---
;;;; The selected items shall be returned in a list.

(load (merge-pathnames "p03.lisp" *load-truename*)) ; element-at
(load (merge-pathnames "p04.lisp" *load-truename*)) ; element-number
(load (merge-pathnames "p05.lisp" *load-truename*)) ; reverse-list
(load (merge-pathnames "p20.lisp" *load-truename*)) ; remove-at
(load (merge-pathnames "../../utils/skip.lisp" *load-truename*))
(load (merge-pathnames "../../utils/drop.lisp" *load-truename*))
(load (merge-pathnames "../../utils/append-list.lisp" *load-truename*))

(setf *random-state* (make-random-state t))

(defun rnd-select (list n)
    (cond
        ((equal list nil) nil)
        ((<= n 0) nil)
        ((<= (element-number list) n) (rnd-select-fun list '() (element-number list) (+ (random (element-number list)) 1) ))
        (t (rnd-select-fun list '() n (+ (random (element-number list)) 1)))
    ))

(defun rnd-select-fun (list selected n pick)
    (cond
        ((equal list nil) selected)
        ((= n 1) (cons (element-at list pick) selected))
        (t (rnd-select-fun (remove-at list pick) (cons (element-at list pick) selected) (- n 1) (+ (random (- n 1)) 1)))
    ))

;; TODO: Test that the list contains unique elements
(assert (equal (element-number (rnd-select '() 1)) 0))
(assert (equal (element-number (rnd-select '(A B C) 1)) 1))
(assert (equal (element-number (rnd-select '(A B C D) 20)) 4))
(assert (equal (element-number (rnd-select '(A B C D E F G H) 8)) 8))