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
  (let ((new-agents '()))
    (loop for i from 1 to number-of-agents
          for new-agent = (make-instance 'naming-game-agent
                                         :id (make-id "AGENT")
                                         :experiment experiment
                                         :population (population experiment)
                                         :introduced-in-game (length (interactions experiment)))
          do (progn
               (setf new-agents (cons new-agent new-agents))
               
               ; Add new-agent to the social-network
               (setf (social-network new-agent) (agents (population experiment)))
               (initialise-neighbor-q-values new-agent)
               (loop for neighbor in (social-network new-agent)
                     do (progn (setf (social-network neighbor) (push new-agent (social-network neighbor)))
                          (insert-neighbor-q-value neighbor new-agent)))
               ; Add new-agent to the population
               (push new-agent (agents (population experiment)))))
    (notify introduce-new-agents-finished (population experiment) (reverse new-agents))))



(defun replace-agents (experiment proportion-of-population)
  "Randomly replaces a given proportion of agents in the population by a new agent."
  (let* ((nr-of-replacements (round (* proportion-of-population (length (agents (population experiment))))))
         (agents-to-replace (random-elts (agents (population experiment)) nr-of-replacements))
         (new-agents '()))
    (loop for old-agent in agents-to-replace
          do (let ((new-agent (make-instance 'naming-game-agent
                                             :id (make-id "AGENT")
                                             :experiment experiment
                                             :population (population experiment)
                                             :introduced-in-game (length (interactions experiment)))))
               (setf new-agents (cons new-agent new-agents))
               
               ; Replace old-agent by new-agent in the population
               (nsubst new-agent old-agent (agents (population experiment)))
               
               ; Every neighbor of old-agent now keeps new-agent in its social-network instead 
               (loop for neighbor in (social-network old-agent)
                     do (progn
                          (nsubst new-agent old-agent (social-network neighbor))
                          (remove-neighbor-q-value neighbor old-agent)
                          (insert-neighbor-q-value neighbor new-agent))) ; default initial q-value
               
               ; new-agent takes over the social-network of old-agent
               (setf (social-network new-agent) (social-network old-agent))
               (initialise-neighbor-q-values new-agent)))
    (notify replace-agents-finished (population experiment) agents-to-replace (reverse new-agents))))


;;                Robustness                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-noise (utterance  &optional (noise-level 0.2))
  "Add noise to the utterance proportionate to the noise level."
  (let* ((strings '("a" "e" "i" "o" "u" "b" "c" "d" "f" "g" "h" "j" "k" "l" "m" "n" "p" "q" "r" "s" "t" "v" "w" "x" "y" "z"))
         (characters (loop for string in strings collect (coerce string 'character)))
         (characters-to-replace (floor (* noise-level (length utterance))))
         (index-list (loop for i from 0 to (- (length utterance) 1) collect i)))
    
    (loop for i from 1 to characters-to-replace
          for index-to-replace = (random-elt index-list)
          for char-to-replace = (elt utterance index-to-replace)
          for character-list = (remove char-to-replace characters)
          do (setf index-list (remove index-to-replace index-list))
          do (setf (elt utterance index-to-replace) (random-elt character-list)))
    utterance))


(defun levenshtein-distance-strings (s1 s2)
  "Levenshtein distance function between two strings."
  (let* ((width (1+ (length s1)))
	 (height (1+ (length s2)))
	 (d (make-array (list height width))))
    (dotimes (x width)
      (setf (aref d 0 x) x))
    (dotimes (y height)
      (setf (aref d y 0) y))
    (dotimes (x (length s1))
      (dotimes (y (length s2))
	(setf (aref d (1+ y) (1+ x))
	      (min (1+ (aref d y (1+ x)))
		   (1+ (aref d (1+ y) x))
		   (+ (aref d y x)
		      (if (char= (aref s1 x) (aref s2 y))
			  0
			  1))))))
    (aref d (1- height) (1- width))))
