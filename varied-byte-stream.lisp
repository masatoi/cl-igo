(defpackage igo.varied-byte-stream
  (:use :common-lisp :igo.type)
  (:nicknames :vbs)
  (:shadow read-byte
	   read-sequence)
  (:export with-input-file
	   read-byte
	   read-sequence
	   file-size))
(in-package :igo.varied-byte-stream)

;;;;;;;;;;
;;; struct
(defstruct varied-byte-stream 
  (source nil :type file-stream)
  (offset 0   :type fixnum))

;;;;;;;;;;;;;;;;;;;;;
;;; external function
(defmacro with-input-file ((stream filespec) &body body)
  `(with-open-file (,stream ,filespec)
     (let ((,stream (make-varied-byte-stream :source ,stream)))
       ,@body)))

(defun file-size (vbs)
  (file-length (varied-byte-stream-source vbs)))

(defun read-byte (varied-byte-stream byte-size &key (signed t))
  (with-slots (source offset) varied-byte-stream
    (with-open-file (in source :element-type `(n-byte ,byte-size ,signed))
      (file-position in (/ offset byte-size))
      (prog1 (common-lisp:read-byte in)
        (incf offset byte-size)))))

(defun read-sequence (varied-byte-stream byte-size count &key (signed t))
  (with-slots (source offset) varied-byte-stream
    (with-open-file (in source :element-type `(n-byte ,byte-size ,signed))
      (file-position in (/ offset byte-size))
      (let ((buf (make-array count :element-type `(n-byte ,byte-size ,signed))))
        (common-lisp:read-sequence buf in)
        (incf offset (* byte-size count))
        buf))))