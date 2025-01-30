(in-package :crs-conventionality)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                        ;;
;;          Success and alignment         ;;
;;                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; even handlers
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


(defmethod determine-coherence ((speaker naming-game-agent) (hearer naming-game-agent))
  "Determines and sets the coherences. Tests whether the hearer would have used the same word as the speaker, should the hearer have been the speaker."
  (let* ((interaction (current-interaction (experiment speaker)))
         (scene (scene interaction))
         (topic (topic interaction)))
    (conceptualise-and-produce hearer scene topic)
    (if (equalp (conceptualised-utterance speaker) (conceptualised-utterance hearer))
      (setf (coherence interaction) t)
      (setf (coherence interaction) nil))))

;;lateral inhibition: reward and punish all competitors --> comprehend-all
;; just reward, no punishment 

(defmethod align ((speaker naming-game-agent) (hearer naming-game-agent) (interaction crs-conventionality-interaction) (mode (eql :lateral-inhibition)))
  "Align grammar of speaker and hearer based on interaction."
  (if (communicated-successfully interaction)
    ;; Communication succeeded
    (let ((applied-cxn-speaker (first (applied-constructions speaker)))
          (applied-cxn-hearer (first (applied-constructions hearer)))
          (meaning-competitors-speaker (find-competitors speaker)))

      ;; Speaker and hearer increase the score of the constructions they used:
      (setf (attr-val applied-cxn-speaker :score)
            (increase-score (learning-rate speaker) (attr-val applied-cxn-speaker :score)))
      (setf (attr-val applied-cxn-hearer :score)
            (increase-score (learning-rate hearer) (attr-val applied-cxn-hearer :score)))
      
      ;; Speaker punishes competing constructions:
      (loop for cxn in meaning-competitors-speaker
              do (setf (attr-val cxn :score) (decrease-score (learning-rate speaker) (attr-val cxn :score)))))
    
    ;; Communication failed 
    (progn
      (when (applied-constructions speaker)
        (let ((speaker-score (cdr (assoc :score (attributes (first (applied-constructions speaker)))))))
          (setf speaker-score (decrease-score (learning-rate speaker) speaker-score))))
      (adopt (topic interaction) hearer)
      (notify alignment-finished speaker hearer))))

(defun find-competitors (agent)
  "Finds competitors in the cip"
  (set-difference (remove-duplicates (mappend #'fcg::applied-constructions (succeeded-nodes (cip (solution-node agent)))))
                  (applied-constructions agent)))

(defun increase-score (learning-rate score)
  "Increase the score using the interpolation rule and the learning rate."
  (+ learning-rate (* score (- 1 learning-rate))))

(defun decrease-score (learning-rate score)
  "Decrease the score using the interpolation rule and the learning rate."
  (* score (- 1 learning-rate)))

(defmethod adopt ((topic crs-conventionality-entity-set) (hearer naming-game-agent))
  "Adoption of the construction through composition."
  (let* ((cxn-inventory (grammar hearer))
         (scene (scene (first (interactions (experiment (population hearer))))))
         (primitive-inventory (get-data (blackboard cxn-inventory) :primitive-inventory))
         (topic-entity (first (crs-conventionality::entities topic)))
         (partial-program `((bind ,(type-of scene) ?scene ,scene)))
         (composition-result (crs-conventionality::compose-program topic-entity partial-program primitive-inventory)))

    ;;remove later
    (assert composition-result)
    
    (let* ((irl-program (irl::irl-program (irl::chunk (first composition-result))))
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
                              :cxn-inventory ,cxn-inventory
                              :attributes (:score 0.5 :topic ,(id (first (crs-conventionality::entities topic))))))
        ;(add-cxn cxn cxn-inventory)
        (notify adoption-finished cxn)))))

