(in-package :crs-conventionality)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                        ;;
;;          Success and alignment         ;;
;;                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-event alignment-finished (speaker crs-conventionality::crs-conventionality-agent) (hearer crs-conventionality::crs-conventionality-agent))

(define-event adoption-finished (cxn fcg::fcg-construction))

(define-event-handler (trace-interaction alignment-finished)
  (add-element `((h2 :style "background-color: LightGray; padding: 5px;") ,(format nil "Alignment")))
  ; TODO: add punished and rewarded cxns to interface
  )

(define-event-handler (trace-interaction adoption-finished)
  (add-element `((a) ,(format nil "Hearer adopted: ")))
  (add-element (make-html cxn)))



(defmethod determine-success ((speaker naming-game-agent) (hearer naming-game-agent) (interaction crs-conventionality-interaction))
  "Determines and sets success. There is success if the computed-topic of the hearer is the same as the intended topic of the speaker."
  (if (eq (computed-topic hearer) (first (entities (topic speaker)))) ;; pointing
    (setf (communicated-successfully interaction) t)
    (setf (communicated-successfully interaction) nil)))



;;lateral inhibition: reward and punish all competitors --> comprehend-all
;; just reward, no punishment 

(defmethod align ((speaker naming-game-agent) (hearer naming-game-agent) (interaction crs-conventionality-interaction) (mode (eql :lateral-inhibition)))
  "Align grammar of speaker and hearer based on interaction."
  (if (communicated-successfully interaction)
    (let* ((speaker-score (cdr (assoc :score (attributes (first (applied-constructions speaker))))))
           (hearer-score (cdr (assoc :score (attributes (first (applied-constructions hearer))))))
           (meaning-competitors-speaker (find-competitors speaker)) ;; all words that refer to that object --> comprehend-all!!!!! 
           (meaning-competitors-hearer (find-competitors hearer))) ;; all 
      (setf speaker-score (increase-score (learning-rate speaker) speaker-score))
      ;; todo punish competing cxns, do we need to evaluate irl-program and see which have the same topic?
      
      (setf hearer-score (increase-score (learning-rate hearer) hearer-score)))
    (progn
      (when (applied-constructions speaker)
        (let ((speaker-score (cdr (assoc :score (attributes (first (applied-constructions speaker)))))))
          (setf speaker-score (decrease-score (learning-rate speaker) speaker-score))))
      (adopt (topic interaction) hearer)
      (notify alignment-finished speaker hearer))))


(defun increase-score (learning-rate score)
  (+ learning-rate (* score (- 1 learning-rate))))

(defun decrease-score (learning-rate score)
  (* score (- 1 learning-rate)))

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
      ;(add-cxn cxn cxn-inventory)
      (notify adoption-finished cxn))))
