;; project euler 150
;; Searching a triangular array for a sub-triangle having minimum-sum


(defconstant +size+  1000)
(defconstant +total+ (/ (* +size+ (1+ +size+)) 2))

(defvar *random-numbers*
  (loop with x = (make-array +total+ :fill-pointer 0)
     with tmp = 0
     for k from 1 to +total+
     do (progn
          (setf tmp (mod (+ 797807 (* tmp 615949)) (expt 2 20)))
          (vector-push (- tmp (expt 2 19)) x))
     finally (return x)))

;; (print *random-numbers*)
;; (print (reduce '+ *random-numbers*))

(defun access (row col)
  "both row and column are 1 based"
  (aref *random-numbers* (+ -1 col (/ (* row (1- row)) 2))))

(defun print-triangle (row)
  (dotimes (r row)
    (dotimes (c (1+ r))
      (format t "~8D " (access (1+ r) (1+ c))))
    (format t "~%")))


(defvar *sum-cache-row*
  "prefix sum of each row"
  ())

(defvar *num-cache-column*
  "prefix sum of each column"
  ())


(defun min-sub-triangle-sum ()
  ())
