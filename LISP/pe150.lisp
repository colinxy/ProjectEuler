;; project euler 150
;; Searching a triangular array for a sub-triangle having minimum-sum


(defconstant +size+  1000)
(defconstant +total+ (/ (* +size+ (1+ +size+)) 2))

(defparameter *random-numbers*
  (loop with numbers = (make-array +total+ :fill-pointer 0)
     with current = 0
     for k from 1 to +total+
     do (progn
          (setf current (mod (+ 797807 (* current 615949)) (expt 2 20)))
          (vector-push (- current (expt 2 19)) numbers))
     finally (return numbers)))

;; (print *random-numbers*)
;; (print (reduce '+ *random-numbers*))

(defun access (triangle row col)
  "both row and column are 1 based,
primarily used for *random-numbers*"
  (aref triangle (+ -1 col (/ (* row (1- row)) 2))))

(defun print-triangle (row)
  (dotimes (r row)
    (dotimes (c (1+ r))
      (format t "~8D " (access *random-numbers* (1+ r) (1+ c))))
    (format t "~%")))

(defparameter *sum-cache-row*
  ;; prefix sum of each row
  (loop with sum-cache-row = (make-array +size+ :fill-pointer 0)
     for row from 1 to +size+
     do
       (vector-push
        (loop with curr-row-sum = (make-array (1+ row) :initial-element 0)
           for col from 1 to row
           do (let ((curr-element (access *random-numbers* row col)))
                (setf (aref curr-row-sum col)
                      (+ curr-element (aref curr-row-sum (1- col)))))
           finally (return curr-row-sum))
        sum-cache-row)
     finally (return sum-cache-row)))

(defun get-sum (row from-column to-column)
  (let ((row-sums (aref *sum-cache-row* (1- row))))
    (- (aref row-sums to-column) (aref row-sums (1- from-column)))))

(defun triangle-sum (row col depth)
  "for testing only"
  (loop for d from 1 to depth
     sum (get-sum (+ row d -1) col (+ col d -1))))

(defun min-sub-triangle-sum (size)
  (loop for row from 1 to size
     minimize
       (loop for col from 1 to row
          minimize
            (loop with curr-sum = 0
               for depth from 1 to (1+ (- size row))
               do (setf curr-sum (+ curr-sum
                                    (get-sum (+ row depth -1)
                                             col
                                             (+ col depth -1))))
               minimize curr-sum))))

(print (min-sub-triangle-sum +size+))
