(in-package :cl-user)

(defpackage :cl-propbank
  (:use :common-lisp :cl-user)
  (:export *PB-DATA* LOAD-PB-DATA rolesets lemma aliases framenet id)
  (:documentation "A Common Lisp interface to PropBank"))

(pushnew :cl-propbank *features*)
