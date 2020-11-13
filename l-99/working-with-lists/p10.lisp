;;;; Run-length encoding of a list.
;;;; ---
;;;; Use the result of problem P09 to implement the so-called run-length encoding data compression method. 
;;;; Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

(load (merge-pathnames "p04.lisp" *load-truename*)) ; element-number
(load (merge-pathnames "p05.lisp" *load-truename*)) ; reverse-list 
(load (merge-pathnames "p08.lisp" *load-truename*)) ; compress-list
(load (merge-pathnames "p09.lisp" *load-truename*)) ; pack-list

(defun encode-list (list)
    (cond
        ((equal list nil) nil)
        (t (reverse-list (encode-list-fun (pack-list list) '())))
    ))

(defun encode-list-fun (list encoded)
    (cond
        ((equal list nil) encoded)
        (t (encode-list-fun (cdr list) (cons (cons (element-number (car list)) (compress-list (car list))) encoded)))
    ))

(assert (equal (encode-list '()) '()))
(assert (equal (encode-list '(A A A)) '((3 A))))
(assert (equal (encode-list '(A B C D)) '((1 A) (1 B) (1 C) (1 D))))
(assert (equal (encode-list '(A A B A A C D)) '((2 A) (1 B) (2 A) (1 C) (1 D))))
(assert (equal (encode-list '(A B C D D D D)) '((1 A) (1 B) (1 C) (4 D))))
(assert (equal (encode-list '(A A A B B B B B C C D C)) '((3 A) (5 B) (2 C) (1 D) (1 C))))