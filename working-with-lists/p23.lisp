(defun element-at (list n) 
    (cond
        ((equal list nil) nil)
        ((<= n 0) nil) 
        ((= n 1) (car list))
        ((> n 1) (element-at (cdr list) (- n 1)))
    ))
(defun reverse-list (list) 
    (cond 
        ((equal list nil) nil)
        (t (reverse-list-fun list '()))
    ))
(defun reverse-list-fun (list reversed)
    (cond
        ((equal list nil) reversed)
        (t (reverse-list-fun (cdr list) (cons (car list) reversed)))
    ))

(defun skip (list skip)
    (cond 
        ((equal list nil) nil)
        ((< skip 0) nil)
        ((= skip 0) list)
        (t (skip (cdr list) (- skip 1)))
    ))

(defun drop (list drop)
    (cond 
        ((equal list nil) nil)
        ((< drop 0) nil)
        ((= drop 0) list)
        (t (reverse-list (skip (reverse-list list) drop)))
    ))

(defun element-number (list)
    (cond 
        ((equal list nil) 0)
        (t (+ 1 (element-number (cdr list))))
    ))

(defun append-list (list1 list2)
    (cond
        ((equal list1 nil) list2)
        ((equal list2 nil) list1)
        (t (append-list-fun (reverse-list list1) (reverse-list list2) '()))
    ))

(defun append-list-fun (list1 list2 list3) 
    (cond
        ((equal list2 nil)
            (cond 
                ((equal list1 nil) list3)
                (t (append-list-fun (cdr list1) list2 (cons (car list1) list3)))
            ))
        (t (append-list-fun list1 (cdr list2) (cons (car list2) list3)))
    ))

(defun remove-at (list n)
    (cond
        ((equal list nil) nil)
        ((< n 1) list)
        ((> n (element-number list) ) list)
        (t (append-list (drop list (+ (- (element-number list) n) 1)) (skip list n)))
    ))

(setf *random-state* (make-random-state t))

(defun rnd-select (list n)
    (cond
        ((equal list nil) nil)
        ((<= n 0) nil)
        ((<= (element-number list) n) (rnd-select-fun list '() (element-number list) (+ (random (element-number list)) 1) ))
        (t (rnd-select-fun list '() n (+ (random (element-number list)) 1)))
    ))

(defun rnd-select-fun (list selected n pick)
    (cond
        ((equal list nil) selected)
        ((= n 1) (cons (element-at list pick) selected))
        (t (rnd-select-fun (remove-at list pick) (cons (element-at list pick) selected) (- n 1) (+ (random (- n 1)) 1)))
    ))


(format t "~S~%" (rnd-select nil 0))
(format t "~S~%" (rnd-select '() 1)) 
(format t "~S~%" (rnd-select '(A B C) 1)) 
(format t "~S~%" (rnd-select '(A B C D) 20)) 
(format t "~S~%" (rnd-select '(A B C D E F G H) 8)) 

;; '(A B C D) '() 4 3
;; '(A B C D) '() 3 3