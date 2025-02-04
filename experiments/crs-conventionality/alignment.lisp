(in-package :crs-conventionality)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       ;;
;;  Alignment of agents  ;;
;;                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;lateral inhibition: reward and punish all competitors --> comprehend-all
;; just reward, no punishment 

(defmethod align ((speaker naming-game-agent) (hearer naming-game-agent) (interaction crs-conventionality-interaction) (mode (eql :lateral-inhibition)))
  "Align grammar of speaker and hearer based on interaction."
  (let ((applied-cxn-speaker (first (applied-constructions speaker)))
        (applied-cxn-hearer (first (applied-constructions hearer))))
    (if (communicated-successfully interaction)
      ;; Communication succeeded
      ;; Speaker and hearer increase the score of the constructions they used:
      (progn 
        (setf (attr-val applied-cxn-speaker :score)
              (increase-score (learning-rate speaker) (attr-val applied-cxn-speaker :score)))
        (setf (attr-val applied-cxn-hearer :score)
              (increase-score (learning-rate hearer) (attr-val applied-cxn-hearer :score)))
      
        ;; Speaker punishes competing constructions:
        (loop for cxn in (find-competitors speaker)
              do (setf (attr-val cxn :score) (decrease-score (learning-rate speaker) (attr-val cxn :score))))
        (loop for cxn in (find-competitors hearer)
              do (setf (attr-val cxn :score) (decrease-score (learning-rate hearer) (attr-val cxn :score)))))
      
    ;; Communication failed 
    (progn
      (when (applied-constructions speaker)
        (setf (attr-val applied-cxn-speaker :score)
              (decrease-score (learning-rate speaker) (attr-val applied-cxn-speaker :score))))
      (adopt (topic interaction) hearer)
      (notify alignment-finished speaker hearer)))))


(defmethod find-competitors ((agent naming-game-agent))
  "Finds competitors in the cip"
  ;; TODO to keep?
  #|(set-difference (remove-duplicates (mappend #'fcg::applied-constructions (succeeded-nodes (cip (solution-node agent)))))
                  (applied-constructions agent))|#
  (loop for cxn in (constructions-list (grammar agent))
        when (and (eq (attr-val cxn :topic) (id (first (entities (topic agent)))))
                  (not (string= (attr-val cxn :form) (first (utterance agent)))))
          collect cxn))


(defun increase-score (learning-rate score)
  "Increase the score using the interpolation rule and the learning rate."
  (+ learning-rate (* score (- 1 learning-rate))))


(defun decrease-score (learning-rate score)
  "Decrease the score using the interpolation rule and the learning rate."
  (* score (- 1 learning-rate)))


(defmethod adopt ((topic crs-conventionality-entity-set) (hearer naming-game-agent))
  "Adoption of the construction through composition."
  (let* (;; get cxn-inventory and primitive-inventory
         (cxn-inventory (grammar hearer))
         (primitive-inventory (get-data (blackboard cxn-inventory) :primitive-inventory))

         ;; get scene and topic
         (scene (scene (first (interactions (experiment (population hearer))))))
         (topic-entity (first (crs-conventionality::entities topic)))

         ;; start from a partial program that has the scene
         (partial-program `((bind ,(type-of scene) ?scene ,scene)))

         ;; compose a program that leads to the topic, starting from the partial program with the scene
         (composition-result (crs-conventionality::compose-program topic-entity partial-program primitive-inventory)))

    ;; make the construction 
    (let* (;; get meaning based on the irl-program and the bind-statements that are in the composition result
           (irl-program (irl::irl-program (irl::chunk (first composition-result))))
           (bind-statements (irl::bind-statements (first composition-result)))
           (meaning (append irl-program bind-statements))

           ;; form is stored in the utterance slot
           (form (first (utterance hearer)))

           ;; make the construction based on the form, meaning and topic
           (cxn (make-naming-game-cxn topic meaning cxn-inventory form)))

      ;; add cxn to the cxn-inventory
      (add-cxn cxn cxn-inventory)
      (notify adoption-finished cxn))))
