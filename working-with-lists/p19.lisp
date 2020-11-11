(load (merge-pathnames "p01.lisp" *load-truename*)) ;;last-element
(load (merge-pathnames "p04.lisp" *load-truename*)) ;;element-number
(load (merge-pathnames "p05.lisp" *load-truename*)) ;;reverse-list
(load (merge-pathnames "../utils/abs-n.lisp" *load-truename*))
(load (merge-pathnames "../utils/skip.lisp" *load-truename*))
(load (merge-pathnames "../utils/drop.lisp" *load-truename*))

(defun rotate-list (list n)
    (cond
        ((equal list nil) nil)
        ((< n 0) (rotate-list list (- (element-number list) (abs-n n)) ))
        ((= n 0) list)
        (t (rotate-list (cons (last-element list) (drop list 1)) (- n 1)))   
    ))

(assert (equal (rotate-list '() 1) '()))
(assert (equal (rotate-list '(A B C D) -1) '(B C D A)))
(assert (equal (rotate-list '(A B C D) 0) '(A B C D)))
(assert (equal (rotate-list '(A B C D) 2) '(C D A B)))
(assert (equal (rotate-list '(A B C D E F G H I) 4) '(F G H I A B C D E)))
(assert (equal (rotate-list '(A B C D E F G H I) -3) '(D E F G H I A B C)))