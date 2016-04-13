;; project euler 150
;; Searching a triangular array for a sub-triangle having minimum-sum


(defconstant +size+ 500500)

(defvar random-numbers
  (loop with x = (make-array +size+ :fill-pointer 0)
        with tmp = 0
        for k from 1 to +size+
        do (progn
             (setf tmp (mod (+ 797807 (* tmp 615949)) (expt 2 20)))
             (vector-push (- tmp (expt 2 19)) x))
        finally (return x)))

;; (print random-numbers)
;; (print (reduce '+ random-numbers))


(defun min-sub-triangle-sum (triangular-array)
  ())
