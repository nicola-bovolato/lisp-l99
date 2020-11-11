;;;; Run-length encoding of a list (direct solution).
;;;; Implement the so-called run-length encoding data compression method directly. 
;;;; I.e. don't explicitly create the sublists containing the duplicates, as in problem P09, but only count them. 
;;;; As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.

(load (merge-pathnames "p05.lisp" *load-truename*)) ; reverse-list

(defun encode-list-improved-alt (list)
    (cond
        ((equal list nil) nil)
        (t (reverse-list (encode-list-improved-alt-fun list '() nil 0)))
    ))

(defun encode-list-improved-alt-fun (list encoded element count)
    (cond
        ((equal list nil) (cond
                ((= count 1) (cons element encoded))
                (t (cons (cons count (cons element nil)) encoded))
            ))
        ((equal element (car list)) (encode-list-improved-alt-fun (cdr list) encoded element (+ count 1)))
        (t 
            (cond
                ((equal element nil) (encode-list-improved-alt-fun list encoded (car list) 0))
                ((= count 1) (encode-list-improved-alt-fun list (cons element encoded) nil 0))
                (t (encode-list-improved-alt-fun list (cons (cons count (cons element nil)) encoded) nil 0))
            )
        ) 
    ))

(assert (equal (encode-list-improved-alt '()) '()))
(assert (equal (encode-list-improved-alt '(A A A)) '((3 A))))
(assert (equal (encode-list-improved-alt '(A B C D)) '(A B C D)))
(assert (equal (encode-list-improved-alt '(A A B A A C D)) '((2 A) B (2 A) C D)))
(assert (equal (encode-list-improved-alt '(A B C D D D D)) '(A B C (4 D))))
(assert (equal (encode-list-improved-alt '(A A A B B B B B C C D C)) '((3 A) (5 B) (2 C) D C)))