
;; TODO : use defpackage


(defun prime-p (n)
  (cond ((<= n 3) (>= n 2))
        ((or (zerop (logand n 1))
             (zerop (rem n 3)))
         nil)
        (t
         (loop for p from 5 to (isqrt n) by 6
            never (or (zerop (rem n p))
                      (zerop (rem n (+ 2 p))))))))


;; borrowed from rosetta code
(defun prime-sieve (upper-limit)
  (cons 2                               ;build from odd only numbers
        (loop
           with upper = (ash (1- upper-limit) -1)
           with is-prime-arr = (make-array (1+ upper)
                                           :element-type 'bit
                                           :initial-element 0)
           ;; index maps to number (1+ (* 2 index))
           ;; 0 means *is-prime*
           with stop = (ash (isqrt upper-limit) -1)

           for i from 1 to upper
           when (zerop (sbit is-prime-arr i))
           collect (1+ (ash i 1))
           and when (<= i stop) do
             (loop for j from (ash (* i (1+ i)) 1) to upper by (1+ (ash i 1))
                do (setf (sbit is-prime-arr j) 1)))))


(defun is-prime-array (upper-limit)
  "Return an array of booleans (bits). 0 means *not a prime*.
Access return value with 1 based index."
  (let ((is-prime-arr (make-array (1+ upper-limit)
                                  :element-type 'bit
                                  :initial-element 1)))
    (setf (sbit is-prime-arr 0) 0)
    (setf (sbit is-prime-arr 1) 0)

    (loop with stop = (isqrt upper-limit)
       for i from 1 to upper-limit
       when (and (not (zerop (sbit is-prime-arr i))) ;i is a prime
                 (<= i stop))
       do (loop for j from (* i i) to upper-limit by i
             do (setf (sbit is-prime-arr j) 0)))

    is-prime-arr))
