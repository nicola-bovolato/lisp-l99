;;;; Sorting a list of lists according to length of sublists
;;;; a) We suppose that a list contains elements that are lists themselves. 
;;;;    The objective is to sort the elements of this list according to their length. 
;;;;    E.g. short lists first, longer lists later, or vice versa.
;;;; b) Again, we suppose that a list contains elements that are lists themselves. 
;;;;    But this time the objective is to sort the elements of this list according to their length frequency;
;;;;    i.e., in the default, where sorting is done ascendingly, lists with rare lengths are placed first, others with a more frequent length come later.

(load (merge-pathnames "p03.lisp" *load-truename*)) ; element-at
(load (merge-pathnames "p04.lisp" *load-truename*)) ; element-number
(load (merge-pathnames "p10.lisp" *load-truename*)) ; encode-list
(load (merge-pathnames "p20.lisp" *load-truename*)) ; remove-at
(load (merge-pathnames "p21.lisp" *load-truename*)) ; insert-at
(load (merge-pathnames "../../utils/swap.lisp" *load-truename*))
(load (merge-pathnames "../../utils/numeric-sort.lisp" *load-truename*))


;; version a

(defun lsort (list)
    (cond
        ((<= (element-number list) 1) list)
        (t (lsort-fun list 1 2))
    ))

(defun lsort-fun (list compared current) 
    (cond
        ((= compared (element-number list)) list)
        ((> current (element-number list)) (lsort-fun list (+ compared 1) (+ compared 2)))
        ((> (element-number (element-at list compared)) (element-number (element-at list current))) 
            (lsort-fun (swap list compared current)  compared (+ current 1)
        ))
        (t (lsort-fun list compared (+ current 1)))
    ))


;; version b
;; okay i think this one needs some comments
;; in words, my solution is this:
;;  - you are given a list of elements
;;  - you insert in a separate list (occurrencies) how many elements there are and how long they are
;;  - you use your new list of occurrencies to keep track of how many elements are present in the list
;;  - you read your initial list in search of the longest element
;;  - when you find it you remove it from that list and add to a new one (sorted), you also update how many elements there are in the occurrencies list
;;  - going from longest tho shortest, you cycle the whole list until empty

(defun lfsort (list)
    (cond
        ((equal list nil) nil)
        (t (lfsort-fun list '() (lfsort-sort-occurrencies (lfsort-calc-occurrencies list '())) ))
    ))

;; this will return the occurrencies list e.g : '((A) (A B) (B C D) (B) (C) (A B C)) => '((3 1) (1 2) (2 3))
;; Each element has this format: (n1 n2)
;;   n1: number of times the element appears in the list
;;   n2: length of the element

(defun lfsort-calc-occurrencies (list occurrencies) 
    (cond
        ((equal list nil) (encode-list (numeric-sort occurrencies)))
        (t (lfsort-calc-occurrencies (cdr list) (insert-at (element-number (car list)) occurrencies 1)))
    ))

;; sorts the occurrencies list (n1 n2) based on the value of n1

(defun lfsort-sort-occurrencies (occurrencies) 
    (cond
        ((< (element-number occurrencies) 2) occurrencies)
        (t (lfsort-sort-occurrencies-fun occurrencies 1 2))
    ))

(defun lfsort-sort-occurrencies-fun (occurrencies compared current) 
    (cond
        ((= compared (element-number occurrencies)) occurrencies)
        ((> current (element-number occurrencies)) (lfsort-sort-occurrencies-fun occurrencies (+ compared 1) (+ compared 2)))
        ((> (car (element-at occurrencies compared)) (car (element-at occurrencies current))) 
            (lfsort-sort-occurrencies-fun (swap occurrencies compared current) compared (+ current 1)
        ))
        (t (lfsort-sort-occurrencies-fun occurrencies compared (+ current 1)))
    ))

;; the real deal
;; - looks at the occurrencies
;; - populates the sorted list
;; - removes elements from the original list
;; - updates the number of elements in the occurrencies list

(defun lfsort-fun (list sorted occurrencies) 
    (cond
        ((equal list nil) sorted)
        ((= (car (car occurrencies)) 0) (lfsort-fun list sorted (cdr occurrencies)))
        (t (lfsort-fun 
            (lfsort-remove-element list (car (cdr (car occurrencies))) 1) 
            (cons (lfsort-first-element list (car (cdr (car occurrencies))) 1) sorted) 
            (insert-at (cons (- (car (car occurrencies)) 1) (cdr (car occurrencies))) (remove-at occurrencies 1) 1)
        ))
    ))

;; finds the first element with a 'element-length' length in the list
(defun lfsort-first-element (list element-length index)
    (cond
        ((= (element-number (element-at list index)) element-length) (element-at list index))
        (t (lfsort-first-element list element-length (+ index 1)))
    ))

;; removes the first element with a 'element-length' length in the list
(defun lfsort-remove-element (list element-length index)
    (cond
        ((= (element-number (element-at list index)) element-length) (remove-at list index))
        (t (lfsort-remove-element list element-length (+ index 1)))
    ))

;; TODO: Replace with tests

(format t "Version a: (order the elements according to their length) ~%")
(format t "~S~%" (lsort '((A) (B) (C) (A B C) (A B) (B C D) (A))))
(format t "~S~%" (lsort '((A) (A B) (A B C))))
(format t "~S~%" (lsort '((A B C) (A) (A B))))
(format t "~S~%" (lsort '((A B C) (A B) (A))))
(format t "~%")
(format t "Version b: (order the elements according to how many elements with that length are in the list) ~%")
(format t "~S~%" (lfsort '((A) (B) (C) (A B C) (A B) (B C D) (A))))
(format t "~S~%" (lfsort '((A) (A B) (A B C))))
(format t "~S~%" (lfsort '((A B C) (A) (A B))))
(format t "~S~%" (lfsort '((A B C) (A B) (A))))