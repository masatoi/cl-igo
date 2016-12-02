(in-package :igo)

;;;;;;;;;;;
;;; declaim
(declaim (inline coerce-to-simple-characters set-mincost-node)
	 #.*optimize-fastest*)

;;;;;;;;;;
;;; struct 
(defstruct tagger
  (wdc nil :type dic:word-dic :read-only t)
  (unk nil :type unk:unknown  :read-only t)
  (mtx nil :type mtx:matrix   :read-only t))
(defmethod print-object ((o tagger) stream)
  (print-unreadable-object (o stream :type t :identity t)))

;;;;;;;;;;;;
;;; constant
(eval-when (:compile-toplevel :load-toplevel :execute)
  (igo::defconst-once-only +BOS-NODES+ (list (vn:make-bos/eos))))

;;;;;;;;;;;;;;;;;;;;;
;;; internal function
(defmacro nconcf (lst1 lst2)
  `(setf ,lst1 (nconc ,lst1 ,lst2)))

(defun set-mincost-node (vn prevs mtx wdc &aux (left-id (vn:left-id vn)))
  (flet ((calc-cost (prev)
           (+ (vn:cost prev) (mtx:link-cost (vn:right-id prev) left-id mtx))))
    (declare (inline calc-cost))

    (let ((fst (first prevs)))
      (setf (vn:prev vn) fst
	    (vn:cost vn) (calc-cost fst)))

    (dolist (p (cdr prevs))
      (let ((cost (calc-cost p)))
	(when (< cost (vn:cost vn))
	  (setf (vn:prev vn) p
		(vn:cost vn) cost))))

    (incf (vn:cost vn) (dic:word-cost (vn:word-id vn) wdc))
    vn))

(defun parse-impl (tagger cs len)
  (declare (fixnum len))
  (let ((nodes (make-array (1+ len) :initial-element nil))
	(wdc   (tagger-wdc tagger))
	(unk   (tagger-unk tagger))
	(mtx   (tagger-mtx tagger)))
    (setf (aref nodes 0) +BOS-NODES+)

    (loop FOR i FROM 0 BELOW len 
	  FOR prevs = (aref nodes i) DO
      (setf (code-stream:position cs) i)
      (when prevs
	(dolist (vn (unk:search cs unk wdc (dic:search cs '() wdc)))
	  (if (vn:space? vn)
	      (nconcf (aref nodes (vn:end vn)) prevs)
	    (push (set-mincost-node vn prevs mtx wdc) (aref nodes (vn:end vn)))))))
    
    (vn:prev (set-mincost-node (vn:make-bos/eos) (aref nodes len) mtx wdc))))

(defmacro parse-then-map-result ((viterbi-node charseq tagger) &body body)
  (let ((result (gensym))
	(code-stream (gensym)))
    `(let ((,result '())
	   (,code-stream (code-stream:make ,charseq)))
       (declare (dynamic-extent ,code-stream))
       (do ((,viterbi-node (parse-impl ,tagger ,code-stream (charseq:length ,charseq))
			   (vn:prev ,viterbi-node)))
	   ((null (vn:prev ,viterbi-node)) ,result)
         (push (progn ,@body) ,result)))))

;;;;;;;;;;;;;;;;;;;;;
;;; external function
(defun load-tagger (data-dir &key (feature-parser #'identity) (bind-special t))
  (if (not bind-special)
      #1=(make-tagger :wdc (dic:load data-dir feature-parser)
		      :unk (unk:load data-dir)
		      :mtx (mtx:load data-dir))
    (setf *tagger* #1#)))

(defun parse (text &key (tagger *tagger*) 
		        (start 0)
			(end (length text))
		   &aux (wdc (tagger-wdc tagger)))
  (declare (optimize (speed 3) (debug 1) (safety 1))
	   (string text)
	   (tagger tagger)
	   (charseq:index start end))
  (charseq:with-dynamic-extent (charseq text :start start :end end)
    (declare #.*optimize-fastest*)
    (parse-then-map-result (vn charseq tagger)
      (list (charseq:to-string charseq (vn:start vn) (vn:end vn))
	    (dic:word-data (vn:word-id vn) wdc)
	    (vn:start vn)))))

(defun wakati (text &key (tagger *tagger*) (start 0) (end (length text)))
  (declare (optimize (speed 3) (debug 1) (safety 1))
	   (string text)
	   (tagger tagger)
	   (charseq:index start end))
  (charseq:with-dynamic-extent (charseq text :start start :end end)
    (declare #.*optimize-fastest*)
    (parse-then-map-result (vn charseq tagger)
      (charseq:to-string charseq (vn:start vn) (vn:end vn)))))