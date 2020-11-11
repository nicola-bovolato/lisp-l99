(load (merge-pathnames "p04.lisp" *load-truename*)) ;;element-number
(load (merge-pathnames "p05.lisp" *load-truename*)) ;;reverse-list
(load (merge-pathnames "../utils/skip.lisp" *load-truename*))
(load (merge-pathnames "../utils/drop.lisp" *load-truename*))
(load (merge-pathnames "../utils/append-list.lisp" *load-truename*))

(defun insert-at (element list n)
    (cond 
        ((equal list nil) (cons element nil))
        ((<= n 1) (cons element list))
        ((> n (element-number list)) (reverse-list (cons element (reverse-list list))))
        (t (append-list (drop list (- (element-number list) (- n 1))) (cons element (skip list (- n 1)))))
    ))

(assert (equal (insert-at 'A '() 0) '(A)))
(assert (equal (insert-at 'A '() 5) '(A)))
(assert (equal (insert-at 'D '(A B C E) 4) '(A B C D E)))
(assert (equal (insert-at 'A '(B C D E F) -1) '(A B C D E F)))
(assert (equal (insert-at 'A '(B C D E F) 1) '(A B C D E F)))
(assert (equal (insert-at 'E '(A B C D F G H I) 5) '(A B C D E F G H I)))
(assert (equal (insert-at 'F '(A B C D E) 20) '(A B C D E F)))