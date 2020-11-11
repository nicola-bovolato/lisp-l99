(load (merge-pathnames "p05.lisp" *load-truename*)) ;;reverse-list
    
(defun range (start end)
    (cond
        ((= start end) (cons start nil))
        ((> start end) (reverse-list (range-fun end start)))
        (t (range-fun start end))
    ))

(defun range-fun (start end)
    (cond
        ((= start end) (cons start nil))
        (t (cons start (range-fun (+ start 1) end)))
    ))

(assert (equal (range 0 0) '(0)))
(assert (equal (range 1 1 ) '(1)))
(assert (equal (range 1 5 ) '(1 2 3 4 5)))
(assert (equal (range 5 1) '(5 4 3 2 1)))
(assert (equal (range 1 2) '(1 2)))