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
                                         :entries '((:nr-of-entities . 5)
                                                    (:nr-of-agents . 10)
                                                    (:determine-interacting-agents-mode . :random-from-population))))

(defparameter *naming-game-canonical* (make-instance 'naming-game-experiment
                                                     :configuration *configuration-canonical*))



;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Learnability setting ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

; ...