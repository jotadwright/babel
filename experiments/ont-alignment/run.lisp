(ql:quickload :ont-alignment)
(in-package :ont-alignment)

(defparameter *experiment*
  (make-instance 'ont-alignment-experiment))

(run-interaction *experiment*)


;(postmodern::query "SELECT year FROM actorsfilms WHERE film = 'American Loser'")

;(postmodern::query "SELECT film FROM actorsfilms WHERE actor = 'Nelson Franklin'")