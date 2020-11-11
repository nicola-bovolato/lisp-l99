(defun repeat-element (element times)
    (cond
        ((<= times 0) nil)
        (t (repeat-element-fun element times '()))
    ))

(defun repeat-element-fun (element times repeated)
    (cond
        ((= times 0) repeated)
        (t (repeat-element-fun element (- times 1) (cons element repeated)))
    ))