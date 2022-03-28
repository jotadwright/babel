
(in-package :mwm-evaluation)
 
(export '(*mwm-primitives*))

(def-irl-primitives mwm-primitives
  :irl-configurations ((:node-tests :no-duplicate-solutions :single-path))
  :primitive-inventory *mwm-primitives*)