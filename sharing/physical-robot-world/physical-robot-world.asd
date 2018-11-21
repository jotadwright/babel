
(in-package #:asdf)

(defsystem :physical-robot-world
  :author "Multiple authors at VUB AI Lab and Sony CSL"
  :maintainer "EHAI <ehai@ai.vub.ac.be>"
  :homepage "https://gitlab.ai.vub.ac.be/ehai/physical-robot-world"
  :depends-on (:utils
               :monitors
               #+:hunchentoot-available-on-this-platform :web-interface)
  :serial t
  :components ((:file "package")
               (:file "robot-world")
               (:file "physical-robot-world-object")
               (:file "physical-robot-world")
               (:file "dynamic-physical-robot-world")
	       #+:hunchentoot-available-on-this-platform 
	       (:file "html")
	       (:file "monitors"))
  :description "An interface to the robot data repository.
                Only used for educational purposes.")

