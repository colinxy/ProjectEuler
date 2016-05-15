
;; if

;; when
(defmacro my-when (condition &rest body)
  `(if ,condition
       (progn ,@body)))

;; unless
(defmacro my-unless (condition &rest body)
  `(if (not ,condition)
       (progn ,@body)))

;; cond
(cond ((= 1 1) 0)
      (t 1))


;; dolist
(dolist (x '(1 2 3))
  (print x)
  (if (evenp x)
      (return)))

;; dotimes
(dotimes (i 4)
  (print i))

;; do
(do ((n 0 (1+ n))
     (cur 0 next)
     (next 1 (+ cur next)))
    ((= n 10) cur))
(do ((i 0 (1+ i)))
    ((>= i 4) nil)
  (print i))

;; loop
(loop for i from 1 to 10
   collecting i)
(do ((nums nil)
     (i 1 (1+ i)))
    ((> i 10) (nreverse nums))
  (push i nums))

(loop for i from 1 to 10
   summing (expt 2 i))
(loop for x across "the quick brown fox jumps over the lazy dog"
   counting (find x "aeiou"))
(loop for i below 10
   and a = 0 then b
   and b = 1 then (+ a b)
   finally (return a))


;;; write macro

;; the distinction between the code that generates code (macros)
;; and the code that eventually makes up the program (everything else)

(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number)
       never (zerop (mod number fac)))))

(defun next-prime (number)
  (loop for n from number
     when (primep n)
     return n))

(do ((p (next-prime 0) (next-prime (1+ p)))
     (> p 19))
    (format t "~d " p))

(defmacro do-primes (var-and-range &rest body)
  (let ((var   (first  var-and-range))
        (start (second var-and-range))
        (end   (third  var-and-range)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
          (> ,var ,end))
         ,@body)))

(defmacro do-primes ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
        (> ,var ,end))
       ,@body))

(do-primes (p 0 19)
  (format t "~d " p))

(do-primes (p 0 (raandom 100))          ;LEAKY MACRO HERE!!!
  (formar t "~d " p))
