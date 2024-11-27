;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                     ;;
;; All functionality for running the crs-conventionality experiment    ;;
;;                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load the system and set *package*
(ql:quickload :crs-conventionality)
(in-package :crs-conventionality)

;;;;;;;;;;;;;;;;;;;;;;;
;; Canonical setting ;; 
;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *configuration-canonical* (make-configuration
                                         :entries '(;; Initialising the experiment
                                                    (:nr-of-entities-in-world . 5)
                                                    (:nr-of-agents-in-population . 10)
                                                    (:nr-of-entities-in-scene . 3)
                                                    ;; Initialising an interaction
                                                    (:determine-interacting-agents-mode . :random-from-population)
                                                    (:determine-scene-entities-mode . :random-subset-of-world)
                                                    (:determine-topic-mode . :random-entity-from-scene))))

(defparameter *naming-game-canonical* (make-instance 'naming-game-experiment
                                                     :configuration *configuration-canonical*))


(run-interaction *naming-game-canonical*)



(add-element (make-html (world *naming-game-canonical*)))
(add-element (make-html (population *naming-game-canonical*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Learnability setting ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

; ...




