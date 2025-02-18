(in-package :cl-user)

(defpackage :crs-conventionality
  (:documentation "Package for the crs-conventionality experiments.")
  (:use
   :common-lisp
   :utils
   :test-framework
   :fcg
   :irl
   :experiment-framework
   :monitors
   :plot-raw-data
   :concept-representations
   #+:hunchentoot-available-on-this-platform :web-interface))
