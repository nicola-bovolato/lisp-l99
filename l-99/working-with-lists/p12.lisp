;;;; Decode a run-length encoded list.
;;;; Given a run-length code list generated as specified in problem P11. Construct its uncompressed version.

(load (merge-pathnames "p05.lisp" *load-truename*)) ; reverse-list 
(load (merge-pathnames "p07.lisp" *load-truename*)) ; flatten-list 
(load (merge-pathnames "../../utils/repeat-element.lisp" *load-truename*))

(defun decode-list (list)
    (cond
        ((equal list nil) nil)
        (t (reverse-list (flatten-list (decode-list-fun list '()))))
    ))

(defun decode-list-fun (list decoded)
    (cond
        ((equal list nil) decoded)
        (t (decode-list-fun (cdr list) (cons (decode-decompress-list (car list)) decoded)))
    ))

(defun decode-decompress-list (list)
    (cond
        ((equal list nil) nil)
        ((atom list) list)
        (t (repeat-element (cdr list) (car list)))
    ))

(assert (equal (decode-list '()) '())) 
(assert (equal (decode-list '(A)) '(A))) 
(assert (equal (decode-list '((2 A))) '(A A))) 
(assert (equal (decode-list '(A B C)) '(A B C))) 
(assert (equal (decode-list '((2 A) (3 B))) '(A A B B B))) 
(assert (equal (decode-list '((2 A) B (3 C))) '(A A B C C C))) 
(assert (equal (decode-list '((2 A) B (3 C) D)) '(A A B C C C D))) 
(assert (equal (decode-list '(A (2 B) C (3 D) E)) '(A B B C D D D E))) 
(assert (equal (decode-list '(A (2 B) C (3 D) (4 E))) '(A B B C D D D E E E E))) 