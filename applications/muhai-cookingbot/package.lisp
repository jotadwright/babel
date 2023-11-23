(in-package :cl-user)

(defpackage :muhai-cookingbot
  (:use :cl-user
        :common-lisp
        :utils
        :monitors
        :web-interface
        :irl
        :fcg)
  (:export #:evaluate)
  (:import-from :alexandria #:hash-table-alist #:alist-hash-table #:make-keyword)
  (:import-from :assoc-utils :alistp)
  (:shadow "PROTOTYPE" "PP")
  (:local-nicknames
   (:jzon :com.inuoe.jzon))
  (:documentation ""))
