(in-package :cle)

;; --------------
;; + Experiment +
;; --------------

(defclass cle-experiment (experiment)
  ()
  (:documentation "The experiment class."))

(defmethod initialize-instance :after ((experiment cle-experiment) &key)
  "Create the population and load the scenes from file."
  ;; 1. population
  (setf (population experiment)
        (loop for i from 1 to (get-configuration experiment :population-size)
              collect (make-instance 'cle-agent :experiment experiment)))
  ;; 2. load clevr scenes
  (setf (world experiment) (make-instance 'clevr-world :data-sets (list "t-val"))))
