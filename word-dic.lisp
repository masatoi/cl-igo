(defpackage igo.word-dic
  (:use :common-lisp)
  (:nicknames :dic)
  (:shadow load
	   search)
  (:export load
	   word-dic
	   word-data
	   word-cost
	   search
	   search-from-trie-id))
(in-package :igo.word-dic)

;;;;;;;;;;;
;;; declaim
(declaim (inline word-data word-cost left-id right-id 
		 high-surrogate-code? surrogate-char-code))

;;;;;;;;;;
;;; struct
(defstruct word-dic
  (trie         nil :type trie:trie :read-only t)
  (costs        #() :type (simple-array (signed-byte 16)) :read-only t)
  (left-ids     #() :type (simple-array (signed-byte 16)) :read-only t)
  (right-ids    #() :type (simple-array (signed-byte 16)) :read-only t)
  (data         #() :type (simple-array t) :read-only t)
  (indices      #() :type (simple-array (signed-byte 32)) :read-only t))

;;;;;;;;;;;;;;;;;;;;;
;;; internal function
(defun read-indices (path)
  (vbs:with-input-file (in path)
    (vbs:read-sequence in 4 (/ (vbs:file-size in) 4))))

(defun high-surrogate-code? (code) (<= #xD800 code #xDBFF))
(defun surrogate-code-char (high low)
  (code-char (+ (ash (+ (- high #xB800) #b1000000) 10)
		(- low #xDC00))))

(defun read-data (path)
  (vbs:with-input-file (in path)
    (vbs:read-sequence in 2 (/ (vbs:file-size in) 2) :signed nil)))

(defun data->string (data)
  (let* ((buf (make-array (length data) :element-type 'character :fill-pointer 0))
	 (high-sgt nil))
    ;; TODO: invalid surrogate code pair check
    (loop FOR code ACROSS data DO
      (cond ((high-surrogate-code? code) (setf high-sgt code))
	    (high-sgt (vector-push (surrogate-code-char high-sgt code) buf))
	    (t        (vector-push (code-char code)                    buf))))
    (copy-seq buf)))

(defun split-data (data offsets feature-parser)
  (declare #.igo::*optimize-fastest*
	   ((simple-array (signed-byte 32)) offsets)
	   ((simple-array (unsigned-byte 16)) data)
	   (function feature-parser))
  (let ((ary (make-array (1- (length offsets))))) 
    (dotimes (i (length ary) ary)
      (setf (aref ary i)
	    (funcall feature-parser 
		     (data->string (subseq data (aref offsets i) (aref offsets (1+ i)))))))))

(defun left-id (word-id wdic) (aref (word-dic-left-ids wdic) word-id))
(defun right-id (word-id wdic) (aref (word-dic-right-ids wdic) word-id))

;;;;;;;;;;;;;;;;;;;;;
;;; external function 
(defun load (root-dir &optional (feature-parser #'identity))
  (flet ((fullpath (name) (merge-pathnames root-dir name)))
    (vbs:with-input-file (in (fullpath "word.inf"))
      (let* ((word-count (/ (vbs:file-size in) (+ 4 2 2 2)))
	     (data    (read-data (fullpath "word.dat")))
	     (offsets (vbs:read-sequence in 4 word-count)))
	(make-word-dic
	 :trie    (trie:load    (fullpath "word2id"))
	 :indices (read-indices (fullpath "word.ary.idx"))
	 :data    (split-data data offsets feature-parser)
	 
	 :left-ids     (vbs:read-sequence in 2 word-count)
	 :right-ids    (vbs:read-sequence in 2 word-count)
	 :costs        (vbs:read-sequence in 2 word-count))))))

(defun search (cs result wdic)
  (declare #.igo::*optimize-fastest*)
  (let ((start   (code-stream:position cs))
	(indices (word-dic-indices wdic))
	(trie (word-dic-trie wdic)))
    (trie:each-common-prefix (end id cs trie)
      #+SBCL (assert (the T (< (1+ id) (length indices))))
      (loop FOR i OF-TYPE igo.type:array-index FROM (aref indices id) BELOW (aref indices (1+ id)) DO
        (push (vn:make i start end (left-id i wdic) (right-id i wdic) nil)
	      result)))
    (setf (code-stream:position cs) start))
  result)

(defun search-from-trie-id (id start end space? result wdic)
  (declare #.igo::*optimize-fastest*
	   (igo.type:array-index id))
  (let ((indices (word-dic-indices wdic)))
    #+SBCL (assert (the T (< (1+ id) (length indices))))
    (loop FOR i fixnum FROM (aref indices id) BELOW (aref indices (1+ id)) DO
      (push (vn:make i start end (left-id i wdic) (right-id i wdic) space?)
	    result)))
  result)

(defun word-cost (word-id wdic) (aref (word-dic-costs wdic) word-id))
(defun word-data (word-id wdic) (aref (word-dic-data wdic)  word-id))