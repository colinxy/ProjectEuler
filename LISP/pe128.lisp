;; project euler 128
;; Hexagonal tile differences

(load "mathutil.lisp")

(defconstant +size+ 2000)

;; ring is represented as
;; (list start end)
(defun nth-ring (n)
  "precondition: n > 1"
  (let ((size (* 6 (1- n)))
        (start (+ 2 (* 3 (- n 1) (- n 2)))))
    (list start (+ start size -1))))

;; KEY OBSERVATION:
;; PD(n) = 3 only possible at 6 edges and the end of ring.
;; moreover, only possible at the start of the ring (1st-edge)
;; and the end of ring

;; TODO : memoize prime-p function

(defun prime-diff3-cumulative (n)
  "starting from the *3rd* ring upto nth ring"
  (loop
     with init = 3
     with size = (* 6 (1- init))
     and ring-start = (+ 2 (* 3 (- init 1) (- init 2)))
     for i from init to n
     ;; start of the ring
     when (and (prime-p (1- size))
               (prime-p (1+ size))
               (prime-p (+ 5 (* 2 size))))
     collect ring-start
     ;; end of the ring
     when (and (prime-p (1- size))
               (prime-p (+ 5 size))
               (prime-p (- (* 2 size) 7)))
     collect (+ ring-start size -1)
     do (setf ring-start (+ size ring-start)
              size (+ 6 size))))

(defun prime-diff3 (n)
  "pd3 at nth ring (n >= 3), return pd3 in nth ring"
  (let* ((size (* 6 (1- n)))
         (ring-start (+ 2 (* 3 (- n 1) (- n 2))))
         (p1 (prime-p (1- size)))
         (p2 (prime-p (1+ size)))
         (p3 (prime-p (+ 5 (* 2 size))))
         (p4 (prime-p (+ 5 size)))
         (p5 (prime-p (- (* 2 size) 7))))
    (cond ((and p1 p2 p3 p4 p5)
           (list ring-start (+ ring-start size -1)))
          ((and p1 p2 p3)
           (list ring-start))
          ((and p1 p4 p5)
           (list (+ ring-start size -1)))
          (t nil))))

(defun size-th-pd3 (size)
  (loop with count = 2 and curr = 2 ;1, 2 are pd3, but is not included
     for i from 3
     until (= size count)
     do (let ((pd3-in-ith-ring (prime-diff3 i))) ;length 0 or 1 or 2
          (setf count (+ (length pd3-in-ith-ring) count))
          (setf curr (car (last pd3-in-ith-ring)))
          (when (> count size)
            (setf curr (if (= (1- count) size)
                           (first pd3-in-ith-ring)
                           (second pd3-in-ith-ring)))
            (setf count size)))
     finally (return curr)))

(print (size-th-pd3 +size+))
