(in-package :au-lib)

(defgeneric anti-unify-predicate-networks (pattern source mode &key cost-mode allow-generalisation-over-constants &allow-other-keys)
  (:documentation "Anti-unify the predicate networks pattern and source according to mode"))