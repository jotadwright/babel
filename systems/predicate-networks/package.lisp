(in-package :cl-user)

(defpackage :predicate-networks
  (:documentation "Package for manipulating predicate networks")
  (:use :common-lisp :test-framework :utils :monitors
        #+:hunchentoot-available-on-this-platform :web-interface)
  (:import-from :irl :equivalent-irl-programs? :bind)
  (:nicknames :pn))