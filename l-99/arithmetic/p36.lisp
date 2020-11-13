;;;; Determine the prime factors of a given positive integer (2).
;;;; Construct a list containing the prime factors and their multiplicity.

(load (merge-pathnames "p31.lisp" *load-truename*)) ; is-prime
(load (merge-pathnames "../working-with-lists/p04.lisp" *load-truename*)) ; element-number
(load (merge-pathnames "../working-with-lists/p05.lisp" *load-truename*)) ; reverse-list
(load (merge-pathnames "../working-with-lists/p08.lisp" *load-truename*)) ; compress-list
(load (merge-pathnames "../working-with-lists/p09.lisp" *load-truename*)) ; pack-list
(load (merge-pathnames "../working-with-lists/p21.lisp" *load-truename*)) ; insert-at
(load (merge-pathnames "../../utils/modulus.lisp" *load-truename*))
(load (merge-pathnames "../../utils/integer-division.lisp" *load-truename*))
(load (merge-pathnames "../../utils/prime-numbers-list.lisp" *load-truename*))

(defun prime-factors-improved (n) 
    (cond
        ((< n 1) nil)
        ((is-prime n) (cons (cons n (cons 1 nil)) nil))
        (t (prime-factors-improved-encode (reverse-list (prime-factors-improved-fun n (prime-numbers-list (- n 1) 1) '()))))
    ))

(defun prime-factors-improved-fun (n prime-numbers list) 
    (cond
        ((= (car prime-numbers) 1) list)
        ((= (modulus n (car prime-numbers)) 0) 
            (prime-factors-improved-fun 
                (integer-division n (car prime-numbers))
                (prime-numbers-list (integer-division n (car prime-numbers)) 1)
                (insert-at (car prime-numbers) list (+ (element-number list) 1))
            ))
        (t (prime-factors-improved-fun n (cdr prime-numbers) list))
    ))

(defun prime-factors-improved-encode (list)
    (cond
        ((equal list nil) nil)
        (t (reverse-list (prime-factors-improved-encode-fun (pack-list list) '())))
    ))

(defun prime-factors-improved-encode-fun (list encoded)
    (cond
        ((equal list nil) encoded)
        (t (prime-factors-improved-encode-fun (cdr list) (cons (cons (car (car list)) (cons (element-number (car list)) nil)) encoded)))
    ))

(assert (equal (prime-factors-improved -1) nil))
(assert (equal (prime-factors-improved 0) nil))
(assert (equal (prime-factors-improved 1) '((1 1))))
(assert (equal (prime-factors-improved 2) '((2 1))))
(assert (equal (prime-factors-improved 3) '((3 1))))
(assert (equal (prime-factors-improved 4) '((2 2))))
(assert (equal (prime-factors-improved 5) '((5 1))))
(assert (equal (prime-factors-improved 6) '((2 1) (3 1))))
(assert (equal (prime-factors-improved 7) '((7 1))))
(assert (equal (prime-factors-improved 8) '((2 3))))
(assert (equal (prime-factors-improved 9) '((3 2))))
(assert (equal (prime-factors-improved 10) '((2 1) (5 1))))
(assert (equal (prime-factors-improved 12) '((2 2) (3 1))))
(assert (equal (prime-factors-improved 13) '((13 1))))
(assert (equal (prime-factors-improved 30) '((2 1) (3 1) (5 1))))
(assert (equal (prime-factors-improved 300) '((2 2) (3 1) (5 2))))
(assert (equal (prime-factors-improved 315) '((3 2) (5 1) (7 1))))