(defpackage igo.trie
  (:use :common-lisp :igo.type)
  (:nicknames :trie)
  (:shadow load)
  (:export trie
	   load
	   each-common-prefix))
(in-package :igo.trie)

;;;;;;;;;;;
;;; declaim
(declaim (inline id including-tail?))

;;;;;;;;;;
;;; struct
(defstruct (trie (:conc-name ""))
  (element-count 0 :type fixnum :read-only t)
  (begs #()        :type (simple-array (signed-byte 32)) :read-only t)
  (lens #()        :type (simple-array (signed-byte 16)) :read-only t)
  (base #()        :type (simple-array (signed-byte 32)) :read-only t)
  (chck #()        :type (simple-array (unsigned-byte 16)) :read-only t)
  (tail #()        :type (simple-array (unsigned-byte 16)) :read-only t))

;;;;;;;;;;;;;;;;;;;;;
;;; internal function
(defun id (node) (1- (- (the negative-fixnum node))))

(defun including-tail? (cs node trie &aux (id (id node)) (tail (tail trie)))
  (loop REPEAT (aref (lens trie) id)
	FOR i fixnum FROM (aref (begs trie) id)
    ALWAYS (= (aref tail i) (code-stream:read cs))))

(defmacro with-gensyms (vars &body body)
  `(let ,(mapcar (lambda (v) `(,v (gensym))) vars)
     ,@body))

;;;;;;;;;;;;;;;;;;;;;
;;; external function
(defun load (path)
  (vbs:with-input-file (in path)
    (let ((node-size (vbs:read-byte in 4))
	  (tind-size (vbs:read-byte in 4))
	  (tail-size (vbs:read-byte in 4)))
      (make-trie
       :element-count tind-size
       :begs (vbs:read-sequence in 4 tind-size)
       :base (vbs:read-sequence in 4 node-size)
       :lens (vbs:read-sequence in 2 tind-size)
       :chck (vbs:read-sequence in 2 node-size :signed nil)
       :tail (vbs:read-sequence in 2 tail-size :signed nil)))))

(defmacro each-common-prefix ((pos id cs trie) &body body)
  (with-gensyms (base chck node code idx loop-block)
    `(let* ((,base  (base ,trie))
	    (,chck  (chck ,trie))
	    (,node  (aref ,base 0)))
    (declare (fixnum ,node))
    (block ,loop-block
      (loop FOR ,code = (code-stream:read ,cs) DO
        (unless (= ,code code-stream:+TERMINATE-CODE+)
	  (let ((,idx (+ ,node code-stream:+TERMINATE-CODE+)))
	    (when (= (aref ,chck ,idx) code-stream:+TERMINATE-CODE+)
	      (let ((,pos (1- (code-stream:position ,cs)))
		    (,id  (id (aref ,base ,idx))))
		,@body))))

	(prog ((,idx (+ ,node ,code)))
	  (setf ,node (aref ,base ,idx))
	  (when (= (aref ,chck ,idx) ,code)
	    (if (plusp ,node) 
		(go :continue)
	      (when (including-tail? ,cs ,node ,trie)
		(let ((,pos (code-stream:position ,cs))
		      (,id  (id ,node)))
		  ,@body))))
	  (return-from ,loop-block)
	  
	  :continue))))))