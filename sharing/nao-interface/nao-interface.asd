(in-package :asdf)

(defsystem :nao-interface
  :author "EHAI <ehai@ai.vub.ac.be>"
  :maintainer "Jens Nevens <jens@ai.vub.ac.be>"
  :license "GPL 3.0"
  :homepage "https://gitlab.ai.vub.ac.be/ehai/nao-interface"
  :version "3.0"
  :description "Implementing the Babel robot interface for the Nao platform"
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
   (:file "vision-server")
   (:file "nao-vision")))
