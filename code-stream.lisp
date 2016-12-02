(defpackage igo.code-stream
  (:use :common-lisp :igo.type)
  (:nicknames :code-stream)
  (:shadow read 
	   position)
  (:export read
	   unread
	   make
	   end?
	   position
	   +TERMINATE-CODE+))
(in-package :igo.code-stream)

;;;;;;;;;;;
;;; declaim
(declaim (inline end? code low-surrogate high-surrogate make)
	 (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0))
	 (ftype (function (code-stream) utf16-code) read))

;;;;;;;;;;
;;; struct
(defstruct (code-stream (:constructor make (source &aux (position 0)))
			(:conc-name ""))
  
  (source    ""   :type charseq:charseq :read-only t)
  (position   0   :type array-index)
  (surrogate? nil :type boolean))

;;;;;;;;;;;;
;;; constant
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +TERMINATE-CODE+ 0))

;;;;;;;;;;;;;;;;;;;;;
;;; internal function
(defun code (code-stream)
  (char-code (charseq:ref (source code-stream) (position code-stream))))

(defun low-surrogate (code)
  (declare (character-code code))
  (+ #xDC00 (ldb (byte 10 0) code)))

(defun high-surrogate (code)
  (declare (character-code code))
  (+ #xD800 (- (ldb (byte 11 10) code) #b1000000)))

;;;;;;;;;;;;;;;;;;;;;
;;; external function
(defun end? (code-stream)
  (= (position code-stream) (charseq:length (source code-stream))))

(defun read (code-stream)
  (declare (code-stream code-stream))
  (symbol-macrolet ((position   (position code-stream))
		    (surrogate? (surrogate? code-stream)))
    (cond ((end? code-stream)
	   +TERMINATE-CODE+)

	  (t 
	   (let ((code (code code-stream)))
	     (incf position)
	     code)))))

(defun unread (code-stream)
  (declare (code-stream code-stream))
  (symbol-macrolet ((position   (position code-stream))
		    (surrogate? (surrogate? code-stream)))
    (decf position)))