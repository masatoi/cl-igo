(defpackage :igo.unknown
  (:use :common-lisp :igo.char-category)
  (:nicknames :unk)
  (:shadow load
	   search)
  (:export load
	   unknown
	   search))
(in-package :igo.unknown)

;;;;;;;;;;
;;; struct
(defstruct (unknown (:conc-name ""))
  (categorys nil :type category-set :read-only t)
  (space-id  0   :type fixnum :read-only t))

;;;;;;;;;;;;;;;;;;;;;
;;; external function
(defun load (root-dir)
  (let ((cts (igo.char-category:load root-dir)))
    (make-unknown :categorys cts 
		  :space-id (category-trie-id (category (char-code #\Space) cts)))))

(defun search (cs unk wdic result)
  (declare #.igo::*optimize-fastest*)
  (prog* ((start     (code-stream:position cs))
	  (code      (code-stream:read cs))
	  (categorys (categorys unk))
	  (ct        (category code categorys)))
    (when (and result (not (category-invoke? ct))) ; invoke
      (go :end))

    (let* ((trie-id (category-trie-id ct))
	   (space?  (= trie-id (space-id unk)))
	   (limit   (category-length ct)))
      (loop FOR len FROM 1 TO limit DO             ; length
        (setf result 
          (dic:search-from-trie-id trie-id start (code-stream:position cs) space? result wdic))
	(when (or (code-stream:end? cs)
		  (not (compatible? code (code-stream:read cs) categorys)))
	  (go :end)))
      
      (when (and (category-group? ct))             ; group
	(loop (when (code-stream:end? cs)
		(return))
	      (unless (compatible? code (code-stream:read cs) categorys)
		(code-stream:unread cs)
		(return)))
	(setf result 
	  (dic:search-from-trie-id trie-id start (code-stream:position cs) space? result wdic))))
    :end
    (setf (code-stream:position cs) start))
  result)