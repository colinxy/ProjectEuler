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
  "return number of arrangement l has"
  (let ((count 0)
        (curr-list l))
    ;; implicit progn
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

(defvar arrangement '(1 2 3 4 5 6 7 8 9 10 11))
(defvar num-carriage (list-length arrangement))
(defvar how-many 2011)

(defun print-result (l)
    (format t "~{~A~}" (mapcar (lambda (x) (code-char (+ 64 x))) l)))

;; (setq result (maximix-arrangement (maximix-permute num-carriage)
;;                                   (- (* 2 num-carriage) 3) how-many))
;; (print result)
;; (print-result (car result))



;;; rewrite of the maximix with minor improvement

;; recursive macro
;; (defmacro min-list-default (l default)
;;   "return the part of l starting from the minimal item"
;;   `(if ,l
;;        (let ((curr (car ,l))
;;              (after (min-list-default (cdr ,l) ,default)))
;;          (if (<= curr (car after))
;;              ,l
;;            after))
;;      ,default))

;; (defmacro min-list (l)
;;   "wrapper around min-list-default"
;;   `(min-list-default ,l ,l)
;;   )

(defmacro min-list (l)
  `(let* ((curr-list ,l)
          (minimal curr-list))
     (loop while curr-list
           do (progn
                (when (< (car curr-list) (car minimal))
                  (setf minimal curr-list))
                (setf curr-list (cdr curr-list)))
           finally (return minimal))))

;; (print (min-list '(673 238 4387 2378 228 228 634 2733847 2343)))


(defun maximix-p (l)
  "return boolean based on if l is maximix"
  (if (cddr l)                          ;more than 2 elements
      (let ((minimal (min-list l)))
        (and (not (eq minimal l))
             (not (eq minimal (last l)))
             (progn
               (nreverse minimal)
               (nreverse l)
               (maximix-p (cdr l))
               )))
    (> (car l) (cadr l))))

;; (print (maximix-p '(1 2 3 4)))
;; (print (maximix-p '(4 1 3 2)))
;; (print (maximix-p '(4 2 1 3)))
;; (print (maximix-p '(4 6 1 5 3 2)))
;; (print (maximix-p '(3 6 5 4 2 1)))
;; (print (maximix-p '(3 1 7 2 9 8 5 6 10 4 11)))


(defun maximix-arrangement-1 (permutations stop)
  (let ((count 0)
        (last '()))
    (loop for perm in permutations
          until (= count stop)
          when (maximix-p (copy-list perm))
          do (progn
               (setf last (copy-list perm))
               (incf count))
          finally (return `(,last ,count))
          )))

(print-result (car (maximix-arrangement-1 (maximix-permute num-carriage)
                                          how-many)))


;;; rewrite of the maximix with minor improvement
