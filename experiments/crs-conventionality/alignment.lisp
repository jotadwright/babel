(in-package :crs-conventionality)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                        ;;
;;          Success and alignment         ;;
;;                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod determine-success ((speaker naming-game-agent) (hearer naming-game-agent))
  "Determines and sets success. There is success if the computed-topic of the hearer is the same as the intended topic of the speaker."
  (if (eq (computed-topic hearer) (first (entities (topic speaker))))
    (setf (communicated-successfully speaker) t
          (communicated-successfully hearer) t)
    (setf (communicated-successfully speaker) nil
          (communicated-successfully hearer) nil)))

(defgeneric align (topic speaker hearer)
  (:documentation "Align both agents"))

(defmethod align ((topic crs-conventionality-entity-set) (speaker naming-game-agent) (hearer naming-game-agent))
  "Align both agents."
  (if (communicated-successfully speaker)
    (progn
      (incf (cdr (assoc :score (attributes (first (applied-constructions speaker))))) 0.1)
      ;; todo punish competing cxns, do we need to evaluate irl-program and see which have the same topic?
      (incf (cdr (assoc :score (attributes (first (applied-constructions hearer))))) 0.1)
      )
    (progn
      (when (applied-constructions speaker)
        (decf (cdr (assoc :score (attributes (first (applied-constructions speaker))))) 0.1))
      (adopt topic hearer)
  )))


(defmethod adopt ((topic crs-conventionality-entity-set) (hearer naming-game-agent))
  (let* ((cxn-inventory (grammar hearer))
         (scene (scene (first (interactions (experiment (population hearer))))))
         (primitive-inventory (get-data (blackboard cxn-inventory) :primitive-inventory))
         (topic-entity (first (crs-conventionality::entities topic)))
         (partial-program `((bind ,(type-of scene) ?scene ,scene)))
         (composition-result (crs-conventionality::compose-program topic-entity partial-program primitive-inventory))
         (irl-program (irl::irl-program (irl::chunk (first composition-result))))
         (bind-statements (irl::bind-statements (first composition-result)))
         (meaning (append irl-program bind-statements))
         (form (first (utterance hearer)))
         (unit-name (intern (upcase (format nil "?~a-unit" form))))
         (cxn-name (intern (upcase (format nil "~a-cxn" form)))))
    (multiple-value-bind (cxn-set cxn)
        (eval `(def-fcg-cxn ,cxn-name
                            ((,unit-name
                              (meaning ,meaning))
                             <-
                             (root
                              (topic ,topic)
                              --
                              )
                             (,unit-name
                              --
                              (HASH form (,form))))
                            :cxn-inventory ,cxn-inventory))
      (add-cxn cxn cxn-inventory))))
