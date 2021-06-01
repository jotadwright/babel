(defpackage grammar-learning
  (:nicknames :gl)
  (:use :cl :utils :monitors :plot-raw-data :web-interface :fcg :meta-layer-learning :type-hierarchies)
  (:export :args))

;; fix loading of consolidate-repairs after type-hierarchy package has been loaded;;
(load (babel-pathname
       :directory '("systems" "fcg" "construction-inventory-processor")
       :name "construction-inventory-processor"
       :type "lisp"))
