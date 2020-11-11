(load (merge-pathnames "p04.lisp" *load-truename*)) ;;element-number
(load (merge-pathnames "p05.lisp" *load-truename*)) ;;reverse-list
(load (merge-pathnames "../utils/skip.lisp" *load-truename*))
(load (merge-pathnames "../utils/drop.lisp" *load-truename*))
(load (merge-pathnames "../utils/append-list.lisp" *load-truename*))

(defun remove-at (list n)
    (cond
        ((equal list nil) nil)
        ((< n 1) list)
        ((> n (element-number list) ) list)
        (t (append-list (drop list (+ (- (element-number list) n) 1)) (skip list n)))
    ))

(assert (equal (remove-at '() 5) '()))
(assert (equal (remove-at '(A) 1) '()))
(assert (equal (remove-at '(A B C D) 4) '(A B C)))
(assert (equal (remove-at '(A B C D) 1) '(B C D)))
(assert (equal (remove-at '(A B C D) 2) '(A C D)))