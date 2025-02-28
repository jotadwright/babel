(in-package :crs-conventionality)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                           ;;
;; Utils for different experimental settings ;;
;;                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;                Learnability               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun introduce-new-agents (experiment &key (number-of-agents 1))
  "Push a number of new agents to the population of an experiment."
  (loop for i from 1 to number-of-agents
        for new-agent = (make-instance 'naming-game-agent
                                       :id (make-id "AGENT")
                                       :experiment experiment
                                       :population (population experiment)
                                       :introduced-in-game (length (interactions experiment)))
        do (progn
             ; Add new-agent to the social-network
             (setf (social-network new-agent) (agents (population experiment)))
             (initialise-neighbor-q-values new-agent)
             (loop for neighbor in (social-network new-agent)
                   do (progn (setf (social-network neighbor) (push new-agent (social-network neighbor)))
                        (insert-neighbor-q-value neighbor new-agent)))
             ; Add new-agent to the population
             (push new-agent (agents (population experiment))))))

        do (push new-agent (agents (population experiment)))))



;;                Robustness                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun add-noise (utterance  &optional (noise-level 0.2))
  "Add noise to the utterance proportionate to the noise level."
  (let* ((strings '("a" "e" "i" "o" "u" "b" "c" "d" "f" "g" "h" "j" "k" "l" "m" "n" "p" "q" "r" "s" "t" "v" "w" "x" "y" "z"))
(characters (loop for string in strings collect (coerce string 'character)))
        (characters-to-replace (floor (* noise-level (length utterance)))))
    (loop for i from 1 to characters-to-replace
          for index-to-replace = (random (length utterance))
          for char-to-replace = (elt utterance index-to-replace)
          for character-list = (remove char-to-replace characters)
          do (setf (elt utterance index-to-replace) (random-elt character-list)))
    utterance))

