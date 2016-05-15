
;;; variables

;; binding
;; function parameters hold object reference

;; LET: lexical binding
;; global variable (DEFVAR, DEFPARAMETER): dynamic binding


;; closure
(let ((count 0))
  #'(lambda () (setf count (1+ count))))


;; global variable
(defvar *count* 0)          ;only create variable when undefined
(defparameter *gap* 0.01)   ;always assign initial value to variable

;; (setf place value)
(setf x 1 y 2)
;; (setf (aref a 0) 10)
(setf *count* (1+ *count*))
(incf *count*)


(rotatef x y)                           ;swap, return nil
;; rotatef does not have side effect, equivalent to
(let ((tmp x))
  (setf x y y tmp)
  nil)
(shiftf x y 10)                         ;x = y, y = 10, return original x
;; shiftf does not have side effect, equivalent to
(let ((tmp x))
  (setf x y y 10)
  tmp)
