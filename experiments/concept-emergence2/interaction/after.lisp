(in-package :cle)

;; -------------
;; + Alignment +
;; -------------

(defgeneric after-interaction 
    (experiment)
  (:documentation "Finalize the interaction."))

(defmethod after-interaction ((experiment cle-experiment))
  (when (align-p experiment)
    (align (speaker experiment))
    (align (hearer experiment))))

(defun align-p (experiment)
  (get-configuration experiment :align))
