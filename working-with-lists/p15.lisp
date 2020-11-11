(load (merge-pathnames "p05.lisp" *load-truename*)) ;;reverse-list

(defun replicate-list (list times)
    (cond
        ((equal list nil) nil)
        ((< times 0) nil)
        (t (reverse-list (replicate-list-fun list times '() times)))
    ))

(defun replicate-list-fun (list times replicated count)
    (cond
        ((equal list nil) replicated)
        ((= count 0) (replicate-list-fun (cdr list) times replicated times))
        (t (replicate-list-fun list times (cons (car list) replicated) (- count 1)))
    ))

(assert (equal (replicate-list '() 1) '())) 
(assert (equal (replicate-list '(A) 2) '(A A))) 
(assert (equal (replicate-list '(A B C D) 3) '(A A A B B B C C C D D D))) 
(assert (equal (replicate-list '(A A B A A C D) 2) '(A A A A B B A A A A C C D D)))
(assert (equal (replicate-list '(A A B A A C D) 1) '(A A B A A C D)))
(assert (equal (replicate-list '(A A B A A C D) -1) nil))