(defpackage igo.type
  (:use :common-lisp)
  (:export array-index
	   character-code
	   utf16-code
	   negative-fixnum
	   n-byte
	   simple-characters))
(in-package :igo.type)
  
(deftype array-index ()    `(integer 0 ,array-total-size-limit))
(deftype character-code () `(integer 0 ,char-code-limit))
(deftype utf16-code ()     `(integer 0 #xFFFF))
(deftype negative-fixnum ()`(integer ,most-negative-fixnum -1))
(deftype n-byte (byte-size signed?) 
  `(,(if signed? 'signed-byte 'unsigned-byte) ,(* byte-size 8)))
(deftype simple-characters () '(simple-array character (*)))