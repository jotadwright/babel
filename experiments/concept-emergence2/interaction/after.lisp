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
    (align (speaker experiment))
    (align (hearer experiment)))
  
  ;; update partner preferences
  (let ((reward (if (communicated-successfully (speaker experiment)) 1 0)))
    (update-agent-preference (speaker experiment) (hearer experiment) reward (get-configuration experiment :boltzmann-lr))
    (update-agent-preference (hearer experiment) (speaker experiment) reward (get-configuration experiment :boltzmann-lr))))

  

(defun align-p (experiment)
  (get-configuration experiment :align))
