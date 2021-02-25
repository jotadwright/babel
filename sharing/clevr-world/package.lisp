(in-package :cl-user)

(defpackage :clevr-world
  (:use :common-lisp
        :utils
        #+:hunchentoot-available-on-this-platform :web-interface
        :irl)
  (:import-from :cl-jonathan
   :decode-json-as-plist
   :decode-json-as-alist
   :decode-json-as-alist-from-source
   :decode-json-as-plist-from-source)
  (:documentation "A package that provides an interface to the CLEVR dataset"))
