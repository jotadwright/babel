(in-package :cl-user)

(defpackage :visual-dialog
  (:documentation "Visual Dialog")
  (:use :common-lisp :utils :web-interface :irl :fcg
    :clevr-dialog-grammar :nao-interface :robot-interface)
  (:import-from :monitors
                :activate-monitor)
  (:import-from :cl-json
   :decode-json-from-source
   :encode-json))

(in-package :visual-dialog)

(export '(*symbolic-primitives* *hybrid-primitives*))

(def-irl-primitives symbolic-primitives
  :primitive-inventory *symbolic-primitives*)

(def-irl-primitives hybrid-primitives
  :primitive-inventory *hybrid-primitives*)