(in-package :crs-conventionality)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                           ;;
;; Utils for different experimental settings ;;
;;                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;                Learnability               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun throw-in-new-agents (experiment &key (number-of-agents 1))
  "Push a number of new agents to the population of an experiment."
  (loop for i from 1 to number-of-agents
        for new-agent = (make-instance 'naming-game-agent
                                       :id (intern (format nil "AGENT-~a" (+ i (length (agents (population experiment))))))
                                       :experiment experiment
                                       :population (population experiment)
                                       :introduced-in-game (length (interactions experiment)))
        do (push new-agent (agents (population experiment)))))