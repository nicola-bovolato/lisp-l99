(load (merge-pathnames "p03.lisp" *load-truename*)) ;;element-at
(load (merge-pathnames "p04.lisp" *load-truename*)) ;;element-number
(load (merge-pathnames "p05.lisp" *load-truename*)) ;;reverse-list
(load (merge-pathnames "p20.lisp" *load-truename*)) ;;remove-at
(load (merge-pathnames "p22.lisp" *load-truename*)) ;;range
(load (merge-pathnames "p23.lisp" *load-truename*)) ;;random-select
(load (merge-pathnames "../utils/skip.lisp" *load-truename*))
(load (merge-pathnames "../utils/drop.lisp" *load-truename*))
(load (merge-pathnames "../utils/append-list.lisp" *load-truename*))

;;TODO
(defun lotto-select (n end) 
    (cond
        ((<= n 0) nil)
        ((<= end 0) nil)
        (t (rnd-select (range 1 end) n))
    ))

;; TODO: Test that the list contains unique elements
(assert (equal (element-number (lotto-select 1 0)) 0))
(assert (equal (element-number (lotto-select 0 1)) 0))
(assert (equal (element-number (lotto-select 1 10)) 1))
(assert (equal (element-number (lotto-select 6 10)) 6))
(assert (equal (element-number (lotto-select 10 6)) 6))
(assert (equal (element-number (lotto-select 10 10)) 10))
