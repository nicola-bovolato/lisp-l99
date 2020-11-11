(defun skip (list skip)
    (cond 
        ((equal list nil) nil)
        ((< skip 0) nil)
        ((= skip 0) list)
        (t (skip (cdr list) (- skip 1)))
    ))