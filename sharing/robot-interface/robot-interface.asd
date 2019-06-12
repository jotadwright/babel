(in-package :asdf)

(defsystem :robot-interface
  :author "EHAI <ehai@ai.vub.ac.be>"
  :maintainer "Jens Nevens <jens@ai.vub.ac.be>"
  :license "GPL 3.0"
  :homepage "https://gitlab.ai.vub.ac.be/ehai/robot-interface"
  :version "3.0"
  :description "A high-level interface for using robots in the Babel multi-agent experiment framework"
  :depends-on (:test-framework
               :utils
               :cl-json
               :drakma
               :nao-interface)
  :serial t
  :components 
  ((:file "package")
   (:file "robot-connection")
   (:file "movement")
   (:file "speech")
   (:file "vision")
   (:file "headtouch")))
