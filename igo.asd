(in-package :asdf)

(defsystem igo
  :name    "igo"
  :version "0.3.1"
  :author  "Takeru Ohta"
  :description "A Common Lisp morpheme analyzer"
  :depends-on (:charseq)

  :serial t
  :components ((:file "package")
	       (:file "type")
	       (:file "util")
	       (:file "varied-byte-stream")
	       (:file "code-stream")
	       (:file "trie")
	       (:file "matrix")
	       (:file "char-category")
	       (:file "viterbi-node")
	       (:file "word-dic")
	       (:file "unknown")
	       (:file "tagger")
	       (:file "delete-nicknames")))
