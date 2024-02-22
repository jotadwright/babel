
(in-package :moral-foundations)

(defparameter *moral-foundations* nil "Will contain the moral foundations that we wish to detect.")
(defparameter *mf-dictionaries* nil "Will contain word lists for extracting moral foundations.")
(defparameter *mf-vocabulary-list* nil "Hash-table that maps string onto its moral foundation.")
(defparameter *ressources-pathname* nil "Path to the ressources.")

(setf *ressources-pathname* (babel-pathname :directory '("grammars" "moral-foundations" "ressources"))
      *mf-dictionaries* '((english "mf-dictionary-english.txt"))
      *mf-vocabulary-list* (make-hash-table :test #'equal))
