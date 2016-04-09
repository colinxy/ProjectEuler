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
;; (print (maximix '(3 6 5 4 2 1)))


(defun maximix-permute (num-carriage)
  "permutation optimized for maximix"
  ;; list that start with 1 cannot be maximix arrangement
  ;; list that start with 2 cannot be maximix arrangement
  (mapcar (lambda (y) (cons 3 y))
          (permute
           (remove 3 (loop for x from 1 to num-carriage
                           collect x)
                   :count 1))))


(defun maximix-arrangement (permutations worst-arrangement stop)
  (let ((count 0)
        (last '()))
    (loop for perm in permutations
          until (= count stop)
          when (= worst-arrangement (maximix (copy-list perm)))
          do (progn
               (setf last (copy-list perm))
               ;; (print perm)
               ;; (print (maximix (copy-list perm)))
               (incf count))
          ;; do (let ((curr-maximix (maximix (copy-list perm))))
          ;;      (if (= worst-arrangement curr-maximix)
          ;;          (incf count)
          ;;        ())
          ;;      )
          finally (return `(,last ,count))
          )))

(setq arrangement '(1 2 3 4 5 6 7 8 9 10 11))
(setq num-carriage (list-length arrangement))
(setq nth 2011)

(defun print-result (l)
    (format t "~{~A~}" (mapcar (lambda (x) (code-char (+ 64 x))) l)))

(setq result (maximix-arrangement (maximix-permute num-carriage)
                                  (- (* 2 num-carriage) 3) nth))
(print result)
(print-result (car result))
