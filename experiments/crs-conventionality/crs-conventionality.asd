(in-package :asdf)

(defsystem :crs-conventionality
  :description ("System for loading crs-conventionality architecture.")
  :depends-on (:utils :test-framework :experiment-framework :monitors :plot-raw-data :fcg  #+:hunchentoot-available-on-this-platform :web-interface)
  :serial t
  :components 
  ((:file "package")))
