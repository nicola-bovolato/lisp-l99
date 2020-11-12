;;;; Generate a random permutation of the elements of a list.

(load (merge-pathnames "p03.lisp" *load-truename*)) ; element-at
(load (merge-pathnames "p04.lisp" *load-truename*)) ; element-number
(load (merge-pathnames "p05.lisp" *load-truename*)) ; reverse-list
(load (merge-pathnames "p20.lisp" *load-truename*)) ; remove-at
(load (merge-pathnames "p22.lisp" *load-truename*)) ; range
(load (merge-pathnames "p23.lisp" *load-truename*)) ; random-select
(load (merge-pathnames "../../utils/skip.lisp" *load-truename*))
(load (merge-pathnames "../../utils/drop.lisp" *load-truename*))
(load (merge-pathnames "../../utils/append-list.lisp" *load-truename*))

(defun rnd-permutation (list)
    (cond 
        ((equal list nil) nil)
        (t (rnd-select list (element-number list)))
    ))

;; TODO: Test that the list contains unique elements
(assert (equal (element-number (rnd-permutation '())) 0))
(assert (equal (element-number (rnd-permutation '(A B C))) 3))
(assert (equal (element-number (rnd-permutation '(A B C D))) 4))
(assert (equal (element-number (rnd-permutation '(A B C D E F G H))) 8))