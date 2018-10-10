(in-package :asdf)

(defsystem :scene-generator
  :description "Generate a web page with random SVG objects, used as scenes for robots"
  :license "Apache 2.0 License"
  :author "Jens Nevens <jnevens@ai.vub.ac.be>"
  :depends-on (:utils
               :hunchentoot
               :cl-who
               :ht-simple-ajax
               :parenscript)
  :serial t
  :components
  ((:file "package")
   (:file "utils")
   (:file "svg-objects")
   (:file "server")
   (:file "scene-generator")))