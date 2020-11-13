;;;; A list of prime numbers.
;;;; Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.

;;;; I've already solved this in p35 and moved it to utils/prime-numbers-list.lisp, so I'm just going to use that.

(load (merge-pathnames "../../utils/prime-numbers-list.lisp" *load-truename*))

(assert (equal (prime-numbers-list 1 -1) nil))
(assert (equal (prime-numbers-list -1 1) nil))
(assert (equal (prime-numbers-list 1 1) '(1)))
(assert (equal (prime-numbers-list 1 2) '(1 2)))
(assert (equal (prime-numbers-list 1 3) '(1 2 3)))
(assert (equal (prime-numbers-list 3 1) '(3 2 1)))
(assert (equal (prime-numbers-list 1 10) '(1 2 3 5 7)))
(assert (equal (prime-numbers-list 1 20) '(1 2 3 5 7 11 13 17 19)))
(assert (equal (prime-numbers-list 7 20) '(7 11 13 17 19)))
(assert (equal (prime-numbers-list 8 20) '(11 13 17 19)))