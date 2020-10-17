(defun range (start end)
    (cond
        ((> start end) nil)
        ((= start end) (cons start nil))
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