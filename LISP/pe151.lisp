;; project euler 150
;; Paper sheets of standard sizes: an expected-value problem

;; used for average probability
(defun average (&rest args)
  (/ (reduce #'+ args) (length args)))

;; A1: 16, A2: 8, A3: 4, A4: 2, A5: 1

(defun use-single-sheet (k)
  (cond ((= 1 k) nil)
        ((= 2 k) '(1))
        ((= 4 k) '(2 1))
        ((= 8 k) '(4 2 1))
        ((= 16 k) '(8 4 2 1))
        (t (error "invalid sheet size"))))

(defun split-nth (lst)
  "split nth element from lst, where n goes from 1 to (length lst),
and return a list of new list with nth element as the first element.
(tail sharing to save space)"
  (loop
     for l on lst                       ;successive cdr
     for index from 0
     collect (cons (car l)
                   (append (subseq lst 0 index) (cdr l)))))

(defun use-sheet (sheets)
  (mapcar #'(lambda (splitted)
              (merge 'list
                     (copy-seq (use-single-sheet (car splitted)))
                     (copy-seq (cdr splitted))
                     #'>))
          (split-nth sheets)))

;; cache: sheets -> vector
;; each vector represents the mapping num-of-single -> probability

(defparameter *single-table-cache*
  (make-hash-table :test 'equal))

(defun single-table (sheets)
  (cond ((gethash sheets *single-table-cache*) ;cache hit
         (multiple-value-bind (single-cache exists)
             (gethash sheets *single-table-cache*)
           single-cache))

        ((equal '(1) sheets)
         #(1 0 0 0 0 0 0 0
           0 0 0 0 0 0 0 0))

        ((= 1 (length sheets))
         (let ((single-map
                (copy-seq (single-table
                           (car (use-sheet sheets)))))) ;only 1 case
           (loop for i from 15 downto 1
              do (setf (aref single-map i) (aref single-map (1- i))))
           (setf (aref single-map 0) 0)
           ;; put into cache
           (setf (gethash sheets *single-table-cache*) single-map)
           single-map))

        (t
         (let ((single-map
                (apply 'map 'vector #'average
                       (mapcar #'single-table (use-sheet sheets)))))
           (setf (gethash sheets *single-table-cache*) single-map)
           single-map))))

(defun main ()
  (let* ((expected-single (single-table '(16)))
         (expectation
          (1-                     ;not counting first batch of the week
           (/ (loop
                 for exp across expected-single
                 for i from 0
                 sum (* i exp))
              (reduce #'+ expected-single)))))
    (print expected-single)
    ;; (print (float expectation))
    (format t "~%~,6f~%" (float expectation))))

(main)
