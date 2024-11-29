(in-package :crs-conventionality)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                        ;;
;; Code implementing grammars             ;;
;;                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric make-initial-grammar (agent)
  (:documentation "Initialises the grammar of an agent."))


;; Naming game ;;
;;;;;;;;;;;;;;;;;

(defmethod make-initial-grammar ((agent naming-game-agent))
  "Initialises the grammar of a naming game agent."
  (let* ((grammar-name (make-const "grammar")))
    (setf (grammar agent) (eval `(def-fcg-constructions ,grammar-name
                                   :hashed t
                                   :cxn-inventory ,grammar-name
                                   :feature-types ((meaning set)
                                                   (form set))
                                   :fcg-configurations ((:de-render-mode . :de-render-raw)
                                                        (:render-mode . :render-raw)))))))