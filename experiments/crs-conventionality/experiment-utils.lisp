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



;;                Robustness                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod add-noise ((utterance string) &optional (noise-level 0.2))
  "Add noise to the utterance proportionate to the noise level."
  (let ((characters '("a" "e" "i" "o" "u" "b" "c" "d" "f" "g" "h" "j" "k" "l" "m" "n" "p" "q" "r" "s" "t" "v" "w" "x" "y" "z"))
        (characters-to-replace (floor (* noise-level (length utterance)))))
    (loop for i from 1 to characters-to-replace
          do (setf (elt utterance (random (length utterance))) (char (random-elt characters) 0)))
    utterance))

