(in-package :cle)

;; -------------
;; + Alignment +
;; -------------

(defgeneric after-interaction 
    (experiment)
  (:documentation "Finalize the interaction."))

(defmethod after-interaction ((experiment cle-experiment))
  (alignment (speaker experiment))
  (alignment (hearer experiment)))
