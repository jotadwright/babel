(in-package :asdf)

(defsystem :crs-conventionality
  :description ("System for loading crs-conventionality architecture.")
  :depends-on (:utils :test-framework :experiment-framework :monitors :plot-raw-data :fcg :irl  #+:hunchentoot-available-on-this-platform :web-interface)
  :serial t
  :components 
  ((:file "package")
   (:file "class-definitions")
   (:file "irl")
   (:file "grammar")
   (:file "interaction")
   (:file "conceptualisation-and-production")
   (:file "comprehension-and-interpretation")
   (:file "invention")
   (:file "alignment")
   (:file "diagnostics-and-repairs")
   (:file "html")
   (:file "web-monitors")))
