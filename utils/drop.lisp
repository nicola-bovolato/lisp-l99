(load (merge-pathnames "../working-with-lists/p05.lisp" *load-truename*)) ; reverse-list

(defun drop (list drop)
    (cond 
        ((equal list nil) nil)
        ((< drop 0) nil)
        ((= drop 0) list)
        (t (reverse-list (skip (reverse-list list) drop)))
    ))