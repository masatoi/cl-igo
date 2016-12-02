(defpackage igo.char-category
  (:use :common-lisp)
  (:shadow load)
  (:export load
	   category
	   category-set
	   compatible?
	   category-trie-id
	   category-length
	   category-invoke?
	   category-group?))
(in-package :igo.char-category)

;;;;;;;;;;;
;;; declaim
(declaim (inline category compatible?))

;;;;;;;;;;
;;; struct
(defstruct category
  (trie-id 0   :type fixnum :read-only t)
  (length  0   :type fixnum :read-only t)
  (invoke? nil :type boolean :read-only t)
  (group?  nil :type boolean :read-only t))
  
(defstruct (category-set (:conc-name ""))
  (categorys #() :type (simple-array category) :read-only t)
  (char->id  #() :type (simple-array (signed-byte 32)) :read-only t)
  (eql-masks #() :type (simple-array (signed-byte 32)) :read-only t))

;;;;;;;;;;;;;;;;;;;;;
;;; internal function
(defun load-categorys (root-dir)
  (vbs:with-input-file (in (merge-pathnames "char.category" root-dir))
    (let ((data (vbs:read-sequence in 4 (/ (vbs:file-size in) 4))))
      (coerce
       (loop FOR i FROM 0 BELOW (length data) BY 4 COLLECT
         (make-category :trie-id (aref data (+ i 0))
			:length  (aref data (+ i 1))
			:invoke? (= 1 (aref data (+ i 2)))
			:group?  (= 1 (aref data (+ i 3)))))
       'vector))))

;;;;;;;;;;;;;;;;;;;;;
;;; external-function
(defun load (root-dir)
  (vbs:with-input-file (in (merge-pathnames "code2category" root-dir))
    (make-category-set 
     :categorys (load-categorys root-dir)
     :char->id  (vbs:read-sequence in 4 (/ (vbs:file-size in) 4 2))
     :eql-masks (vbs:read-sequence in 4 (/ (vbs:file-size in) 4 2)))))

(defun category (code cset)
  (aref (categorys cset) (aref (char->id cset) code)))

(defun compatible? (code1 code2 cset)
  (let ((eqls (eql-masks cset)))
    (logtest (aref eqls code1) (aref eqls code2))))