(in-package :asdf)

(defsystem :grounded-colour-naming-game-experiment
  :description "Grounded Colour Naming Game with robots"
  :author "EHAI <ehai@ai.vub.ac.be>"
  :maintainer "Jens Nevens <jens@ai.vub.ac.be>"
  :depends-on (:utils
               :experiment-framework
               :test-framework
               :monitors
               :plot-raw-data
               :irl
               :fcg
               #+:hunchentoot-available-on-this-platform :web-interface
               :nao-interface
               :robot-interface
               :scene-generator)
  :serial t
  :components ((:file "package")
               (:file "color-conversions")
               (:file "classes")
               (:file "sensori-motor-level")
               (:file "conceptual-level")
               (:file "language-level")
               (:file "interaction-script")
               (:file "alignment")
               (:file "monitors")
               (:file "web-monitors")))
               