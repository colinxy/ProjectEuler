;; project euler 150
;; Searching a triangular array for a sub-triangle having minimum-sum


(defconstant +size+  1000)
(defconstant +total+ (/ (* +size+ (1+ +size+)) 2))

(defconstant random-numbers
  (loop with x = (make-array +total+ :fill-pointer 0)
        with tmp = 0
        for k from 1 to +total+
        do (progn
             (setf tmp (mod (+ 797807 (* tmp 615949)) (expt 2 20)))
             (vector-push (- tmp (expt 2 19)) x))
        finally (return x)))

;; (print random-numbers)
;; (print (reduce '+ random-numbers))


(defvar sum-cache
  ())

(defun min-sub-triangle-sum (triangular-array)
  ())
