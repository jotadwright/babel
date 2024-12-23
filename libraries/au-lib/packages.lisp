(in-package :cl-user)

(defpackage :au-lib
  (:use :common-lisp :cl-user :monitors)
  (:import-from :alexandria
                #:shuffle
                #:random-elt
                #:mappend
                #:compose
                #:hash-table-keys
                #:hash-table-alist)
  (:export #:anti-unify-predicate-networks))
