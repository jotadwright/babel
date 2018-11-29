(in-package :asdf)

(defsystem :robot-interface
  :author "Jens Nevens <jens@ai.vub.ac.be>"
  :maintainer "Jens Nevens <jens@ai.vub.ac.be>"
  :license "Apache 2.0"
  :homepage "https://gitlab.ai.vub.ac.be/ehai/robot-interface"
  :version "2.0"
  :description "A system implementing an interface between Babel2 and physical robots."
  :depends-on (:test-framework
               :utils
               :cl-json
               :drakma
               :nao-interface)
  :serial t
  :components 
  ((:file "package")
   (:file "setup")
   (:file "robot-connection")
   (:file "movement")
   (:file "speech")
   (:file "vision")
   (:file "headtouch")))
