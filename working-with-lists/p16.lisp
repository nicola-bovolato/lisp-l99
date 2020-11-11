(load (merge-pathnames "p05.lisp" *load-truename*)) ;;reverse-list

(defun drop-every-nth-element (list skip)
    (cond
        ((equal list nil) nil)
        ((< skip 0) nil)
        (t (reverse-list (drop-every-nth-element-fun list skip '() (- skip 1))))
    ))

(defun drop-every-nth-element-fun (list skip dropped count)
    (cond
        ((equal list nil) dropped)
        ((= count 0) (drop-every-nth-element-fun (cdr list) skip dropped (- skip 1)))
        (t (drop-every-nth-element-fun (cdr list) skip (cons (car list) dropped) (- count 1)))
    ))

(assert (equal (drop-every-nth-element '() 1) '())) 
(assert (equal (drop-every-nth-element '(A B C D) 1) '())) 
(assert (equal (drop-every-nth-element '(A) 2) '(A))) 
(assert (equal (drop-every-nth-element '(A B C D) 3) '(A B D))) 
(assert (equal (drop-every-nth-element '(A B C D E F G H I) 3) '(A B D E G H))) 
(assert (equal (drop-every-nth-element '(A A B B C C D D) 2) '(A B C D))) 