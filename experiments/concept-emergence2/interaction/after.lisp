(in-package :cle)

;; -------------
;; + Alignment +
;; -------------

(defgeneric after-interaction 
    (experiment)
  (:documentation "Finalize the interaction."))

(defmethod after-interaction ((experiment cle-experiment))
  ;; update lexicons
  (when (align-p experiment)
    ;; align lexicons
    (align (speaker experiment))
    (align (hearer experiment))
  
    ;; update preferences
    (let ((reward (if (communicated-successfully (speaker experiment)) 1 0)))
      (update-neighbor-q-value (speaker experiment) (hearer experiment) reward (get-configuration experiment :boltzmann-lr))
      (update-neighbor-q-value (hearer experiment) (speaker experiment) reward (get-configuration experiment :boltzmann-lr)))))

(defun align-p (experiment)
  (get-configuration experiment :align))
