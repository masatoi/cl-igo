(defpackage igo.matrix
  (:use :common-lisp)
  (:nicknames :mtx)
  (:shadow load)
  (:export load
	   link-cost
	   matrix))
(in-package :igo.matrix)

;;;;;;;;;;;
;;; declaim
(declaim (inline link-cost))

;;;;;;;;
;;; struct
(defstruct (matrix (:conc-name ""))
  (matrix   #() :type (simple-array (signed-byte 16)) :read-only t)
  (left-size  0 :type (unsigned-byte 16) :read-only t)
  (right-size 0 :type (unsigned-byte 16) :read-only t))

;;;;;;;;;;;;;;;;;;;;;
;;; external function 
(defun load (data-dir)
  (vbs:with-input-file (in (merge-pathnames "matrix.bin" data-dir))
    (let ((left-size  (vbs:read-byte in 4))
	  (right-size (vbs:read-byte in 4)))
      (make-matrix :left-size  left-size
		   :right-size right-size
		   :matrix     (vbs:read-sequence in 2 (* left-size right-size))))))

(defun link-cost (left-id right-id matrix)
  (declare ((unsigned-byte 16) left-id right-id))
  (the (signed-byte 16) 
    (aref (matrix matrix)
	  (+ (* (left-size matrix) right-id) left-id))))