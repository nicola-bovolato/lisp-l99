;;;; Compare the two methods of calculating Euler's totient function.
;;;; ---
;;;; Use the solutions of problems P34 and P37 to compare the algorithms. 
;;;; Take the number of logical inferences as a measure for efficiency.
;;;; Try to calculate phi(10090) as an example.

(load (merge-pathnames "../arithmetic/p34.lisp" *load-truename*)) ; phi
(load (merge-pathnames "../arithmetic/p37.lisp" *load-truename*)) ; phi-improved

(format t "[phi(10090)] Time to solve: " )
(time (phi 10090))
(format t "[phi-improved(10090)] Time to solve: " )
(time (phi-improved 10090))