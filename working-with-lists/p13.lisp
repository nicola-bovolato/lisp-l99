(load (merge-pathnames "p05.lisp" *load-truename*)) ;;reverse-list

(defun encode-list (list)
    (cond
        ((equal list nil) nil)
        (t (reverse-list (encode-list-fun list '() nil 0)))
    ))

(defun encode-list-fun (list encoded element count)
    (cond
        ((equal list nil) (cond
                ((= count 1) (cons element encoded))
                (t (cons (cons count (cons element nil)) encoded))
            ))
        ((equal element (car list)) (encode-list-fun (cdr list) encoded element (+ count 1)))
        (t 
            (cond
                ((equal element nil) (encode-list-fun list encoded (car list) 0))
                ((= count 1) (encode-list-fun list (cons element encoded) nil 0))
                (t (encode-list-fun list (cons (cons count (cons element nil)) encoded) nil 0))
            )
        ) 
    ))

(assert (equal (encode-list '()) '()))
(assert (equal (encode-list '(A A A)) '((3 A))))
(assert (equal (encode-list '(A B C D)) '(A B C D)))
(assert (equal (encode-list '(A A B A A C D)) '((2 A) B (2 A) C D)))
(assert (equal (encode-list '(A B C D D D D)) '(A B C (4 D))))
(assert (equal (encode-list '(A A A B B B B B C C D C)) '((3 A) (5 B) (2 C) D C)))