
(defun billion-test ()
  (let ((i 0))
    (declare (fixnum i)
             (optimize (speed 3) (safety 0) (debug 0))
             (inline +))
    (loop while (< i 1000000000) do
         (setq i (+ i 1)))))

(print "running billion test")
(time (billion-test))


(defun infinite ()
  (infinite))

(cond (nil (print "cond is eager"))
      (t (print "cond is lazy")))
(if nil
    (print "if is eager")
  (print "if is lazy"))


(defun fib (n)
  (cond ((= n 1) 1)
        ((= n 2) 1)
        (t
         (+ (fib (- n 1))
            (fib (- n 2))))))

(print (fib 12))


(defun 3-infix-eval (expression)
  "evaluate a infix arithmetic expression with 3 elements"
  (let ((left (car expression))
        (operator (nth 1 expression))
        (right (nth 2 expression)))
    ;; difference between ' and `
    ;; (eval (list operator left right))
    (eval `(,operator ,left ,right))))  ;expression followed by , are not evaled

(print (3-infix-eval '(1 + 2)))


(defun infix-eval-3 (expression)
  "evaluate infix expression"
  (if (atom expression)
      expression
    (let ((left (first expression))
          (operator (second expression))
          (right (third expression)))
      (eval (list operator
                  (infix-eval-3 left)
                  (infix-eval-3 right))))))

(print (infix-eval-3 '((1 + 1) * (3 * 4))))


(defun my-reverse (l)
  (if l
      (append (my-reverse (cdr l)) (list (car l)))
    l))

(print (my-reverse '(a b c d)))
