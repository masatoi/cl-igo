(defpackage igo.viterbi-node
  (:use :common-lisp)
  (:nicknames :vn)
  (:export make make-bos/eos
	   cost prev word-id left-id right-id start end space?))
(in-package :igo.viterbi-node)

;;;;;;;;;;;
;;; declaim
(declaim (inline new-bos/eos make))

;;;;;;;;;;
;;; struct
(defstruct (viterbi-node (:constructor make (word-id start end left-id right-id space?))
			 (:conc-name "")
			 (:type vector))
  (cost 0     :type fixnum)
  (prev nil   :type t)
  (left-id 0  :type fixnum :read-only t)
  (right-id 0 :type fixnum :read-only t)
  (word-id 0  :type fixnum :read-only t)
  (start 0    :type fixnum :read-only t)
  (end 0      :type fixnum :read-only t)
  (space? nil :type boolean :read-only t))

;;;;;;;;;;;;;;;;;;;;;
;;; external function
(defun make-bos/eos () (make 0 0 0 0 0 nil))