(in-package :asdf)

(defsystem :nao-interface
  :author "Jens Nevens <jens@ai.vub.ac.be>"
  :maintainer "Jens Nevens <jens@ai.vub.ac.be>"
  :license "Apache 2.0"
  :homepage "https://gitlab.ai.vub.ac.be/ehai/nao-interface"
  :version "2.0"
  :description "A system implementing an interface between Babel2 and the Nao Robot."
  :depends-on (:test-framework
               :utils
               :cl-json
               :drakma
               :assoc-utils)
  :serial t
  :components 
  ((:file "package")
   (:file "nao")
   (:file "nao-movement")
   (:file "nao-headtouch")
   (:file "nao-speak")
   (:file "nao-vision")))
