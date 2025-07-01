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
    (align (hearer experiment))

    ;; update social-network q-values
    (let ((reward (if (communicated-successfully (speaker experiment)) 1 0)))
      (partner-selection::update-neighbour-q-value (speaker experiment) (hearer experiment) reward (get-configuration experiment :boltzmann-lr))
      (partner-selection::update-neighbour-q-value (hearer experiment) (speaker experiment) reward (get-configuration experiment :boltzmann-lr)))))


(defun align-p (experiment)
  (get-configuration experiment :align))
