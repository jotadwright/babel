(in-package :cl-user)

(defpackage :visual-dialog
  (:documentation "Visual Dialog")
  (:use :common-lisp :utils :web-interface :irl :fcg
    :clevr-dialog-grammar :monitors)
  (:import-from :cl-json
   :decode-json-from-source
   :encode-json)
  (:shadowing-import-from :jonathan
   :to-json :parse)
  (:import-from :drakma
   :http-request))

(in-package :visual-dialog)

(export '(*symbolic-primitives* *hybrid-primitives*))

(def-irl-primitives symbolic-primitives
  :primitive-inventory *symbolic-primitives*)

(def-irl-primitives hybrid-primitives
  :primitive-inventory *subsymbolic-primitives*)