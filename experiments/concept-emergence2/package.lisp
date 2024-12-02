(in-package :cl-user)

(defpackage :cle
  (:documentation "Emergent concept learning")
  (:use :common-lisp
        :utils
        :web-interface
        :monitors
        :plot-raw-data
        :experiment-framework
        :test-framework
        :irl
        :fcg
        :cl-jonathan)
  (:import-from :cl-mop
                :slot-names
                :map-slots)
  (:import-from :cl-json
   :decode-json-from-string
   :encode-json-to-string
   :encode-json-alist-to-string)
  (:import-from :alexandria #:hash-table-alist #:alist-hash-table)
  (:local-nicknames (:jzon :com.inuoe.jzon))
  (:shadowing-import-from :fcg :size :attributes))
