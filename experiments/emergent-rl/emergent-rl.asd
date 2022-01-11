(in-package :asdf)

(defsystem :emergent-rl
  :description "Language games x Reinforcement Learning experiment"
  :author "EHAI <ehai@ai.vub.ac.be>"
  :maintainer "Jérôme Botoko Ekila <jerome@ai.vub.ac.be>"
  :license "Babel Research License"
  :depends-on (:utils
               :plot-raw-data
               :monitors
               :experiment-framework)
  :serial t
  :components (
               (:file "package")
               (:file "plot-py")
               )
  )
