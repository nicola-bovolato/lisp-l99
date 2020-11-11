(load (merge-pathnames "p05.lisp" *load-truename*)) ;; reverse-list 
(load (merge-pathnames "../utils/repeat-element.lisp" *load-truename*))
(load (merge-pathnames "../utils/skip.lisp" *load-truename*))

(defun pack-list (list) 
    (cond
        ((equal list nil) nil)
        (t (reverse-list (pack-list-fun list '())))
    ))

(defun pack-list-fun (list packed)
    (cond
        ((equal list nil) packed)
        (t (pack-list-fun (skip list (pack-list-count-fun list 1)) (cons (repeat-element (car list) (pack-list-count-fun list 1)) packed)))
    ))

(defun pack-list-count-fun (list count)
    (cond
        ((equal (cdr list) nil) count)
        ((equal (car list) (car (cdr list))) (pack-list-count-fun (cdr list) (+ count 1)))
        (t count)
    ))

(assert (equal (pack-list '()) '())) 
(assert (equal (pack-list '(A A A )) '((A A A )))) 
(assert (equal (pack-list '(A B C D)) '((A) (B) (C) (D)))) 
(assert (equal (pack-list '(A A B A A C D)) '((A A) (B) (A A) (C) (D)))) 
(assert (equal (pack-list '(A B C D D D D)) '((A) (B) (C) (D D D D)))) 
(assert (equal (pack-list '(A A A B B B B B C C D C)) '((A A A) (B B B B B) (C C) (D) (C)))) 