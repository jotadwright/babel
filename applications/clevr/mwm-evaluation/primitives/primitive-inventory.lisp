
(in-package :mwm-evaluation)
 
(export '(*mwm-primitives*))

(def-irl-primitives mwm-primitives
  :irl-configurations ((:node-tests :check-duplicate :no-duplicate-solutions))
  :primitive-inventory *mwm-primitives*)