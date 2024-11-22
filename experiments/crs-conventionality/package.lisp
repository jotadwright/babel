(in-package :cl-user)

(defpackage :crs-conventionality
  (:documentation "Package for the crs-conventionality experiments.")
  (:use :common-lisp :utils :test-framework :fcg :experiment-framework :monitors :plot-raw-data #+:hunchentoot-available-on-this-platform :web-interface))
