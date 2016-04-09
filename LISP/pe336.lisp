;; project euler 336
;; Maximix Arrangements

(defun permute (elements)
  (if elements
      (mapcan #'(lambda (x) (mapcar #'(lambda (y) (cons x y))
                                    (permute (remove x elements))))
              elements)
    '(())))

;; (print (permute '(1 2 3 4 5 6)))
;; (print (permute '(a b c)))


;; written in C style
(defun maximix (l)
  (let ((count 0)
        (curr-list l))
    (loop for x in (sort (copy-list l) #'<)
          do (progn
               (when (not (equal x (car curr-list)))
                 (when (not (equal x (car (last curr-list))))
                   (nreverse (member x curr-list)) ;reverse in place
                   ;; (print curr-list)
                   (incf count)           ;(setf count (1+ count))
                   )
                 (nreverse curr-list)
                 ;; (print curr-list)
                 (incf count))
               (setf curr-list (cdr curr-list))))
    count
    ))

;; (print (maximix '(1 2 3 4)))
;; (print (maximix '(4 1 3 2)))
;; (print (maximix '(4 2 1 3)))
;; (print (maximix '(4 6 1 5 3 2)))


(defun maximix-arrangement (elements count stop)
  (loop for perm in (permute elements)
        do (let ((curr-count (maximix (copy-list perm))))
             (if (equal count curr-count)
                 (print perm)
               ()))))

(setq arrangement '(1 2 3 4 5 6 7 8 9))
(maximix-arrangement arrangement
                     (- (* 2 (list-length arrangement)) 3))
