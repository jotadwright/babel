(in-package :asdf)

(defsystem :crs-conventionality
  :description "System for loading crs-conventionality architecture."
  :author "Hermes I Research Team"
  :maintainer "Lara Verheyen <lara.verheyen@ai.vub.ac.be>"
  :depends-on (:utils 
               :test-framework 
               :experiment-framework 
               :monitors 
               :plot-raw-data 
               :fcg 
               :irl 
               #+:hunchentoot-available-on-this-platform :web-interface)
  :serial t
  :components ((:file "package")
               (:file "class-definitions")
               (:file "utils")
               (:module "monitors"
                :serial t
                :components ((:file "html")
                             (:file "monitors")
                             (:file "web-monitors")))
               (:file "irl")
               (:file "grammar")
               (:file "interaction")
               (:file "conceptualisation-and-production")
               (:file "comprehension-and-interpretation")
               (:file "invention")
               (:file "alignment")
               (:file "diagnostics-and-repairs")
               (:file "social-network")))
