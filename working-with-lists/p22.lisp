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
    
(defun range (start end)
    (cond
        ((= start end) (cons start nil))
        ((> start end) (reverse-list (range-fun end start)))
        (t (range-fun start end))
    )
)

(defun range-fun (start end)
    (cond
        ((= start end) (cons start nil))
        (t (cons start (range-fun (+ start 1) end)))
    )
)

(format t "~S~%" (range 0 0)  )
(format t "~S~%" (range 1 1 )  ) 
(format t "~S~%" (range 1 5 )  ) 
(format t "~S~%" (range 5 1)  ) 
(format t "~S~%" (range 1 2)  ) 