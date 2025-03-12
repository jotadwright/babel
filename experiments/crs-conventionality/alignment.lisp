(in-package :crs-conventionality)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       ;;
;;  Alignment of agents  ;;
;;                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Alignment Strategies ;;
;; Lateral inhibition: reward used utterance and punish all competitors in success, punish used utterance and adopt in failure. 

(defmethod align ((speaker crs-conventionality-agent) (hearer crs-conventionality-agent) (interaction crs-conventionality-interaction)
                  (mode (eql :lateral-inhibition)))
  "Align grammar of speaker and hearer based on interaction."
  (notify alignment-started speaker hearer)
  (let ((applied-cxn-speaker (first (applied-constructions speaker)))
        (applied-cxn-hearer (first (applied-constructions hearer))))
    (if (communicated-successfully interaction)
      ;; Communication succeeded
      ;; Speaker and hearer increase the score of the constructions they used:
      (progn
        (setf (attr-val applied-cxn-speaker :score)
              (calculate-increased-score (learning-rate speaker) (attr-val applied-cxn-speaker :score)))
        (setf (attr-val applied-cxn-hearer :score)
              (calculate-increased-score (learning-rate hearer) (attr-val applied-cxn-hearer :score)))
      
        ;; Speaker punishes competing constructions:
        (loop for cxn in (find-competitors speaker)
              do (setf (attr-val cxn :score) (calculate-decreased-score (learning-rate speaker) (attr-val cxn :score))))
        (loop for cxn in (find-competitors hearer)
              do (setf (attr-val cxn :score) (calculate-decreased-score (learning-rate hearer) (attr-val cxn :score))))
        (notify alignment-finished speaker hearer interaction))
      
      
     ;; Communication failed 
     (progn
       (when (applied-constructions speaker)
         (setf (attr-val applied-cxn-speaker :score)
               (calculate-decreased-score (learning-rate speaker) (attr-val applied-cxn-speaker :score))))
       (notify alignment-finished speaker hearer interaction)
         (adopt (topic interaction) hearer)))))

(defmethod align ((speaker crs-conventionality-agent) (hearer crs-conventionality-agent) (interaction crs-conventionality-interaction)
                  (mode (eql :concept-alignment)))
  "Align grammar of speaker and hearer based on interaction."
  (notify alignment-started speaker hearer)
  (let ((applied-cxn-speaker (first (applied-constructions speaker)))
        (applied-cxn-hearer (first (applied-constructions hearer))))
    (if (communicated-successfully interaction)
      ;; Communication succeeded
      ;; Speaker and hearer increase the score of the constructions they used:
      (progn
        (shift applied-cxn-speaker (topic interaction) speaker)
        (update-score-cxn applied-cxn-speaker 0.1)
        (update-score-cxn applied-cxn-hearer 0.1)

        ;; Speaker punishes competing constructions:
        (punish-competitors speaker)
        (punish-competitors hearer)

        (notify alignment-finished speaker hearer interaction))
      
      
     ;; Communication failed 
     (progn
       (when (applied-constructions speaker)
         (update-score-cxn applied-cxn-speaker -0.1))
       (notify alignment-finished speaker hearer interaction)
       (if (computed-topic hearer)
         (progn 
           (shift applied-cxn-hearer (topic interaction) hearer) ;; check if shift is on the right cxn
           (update-score-cxn applied-cxn-hearer -0.1))
         (adopt (topic interaction) hearer))))))

;; Don't punish competitors in success. 

(defmethod align ((speaker naming-game-agent) (hearer naming-game-agent) (interaction crs-conventionality-interaction)
                  (mode (eql :dont-punish-competitors)))
  "Align grammar of speaker and hearer based on interaction."
  (let ((applied-cxn-speaker (first (applied-constructions speaker)))
        (applied-cxn-hearer (first (applied-constructions hearer))))
    (if (communicated-successfully interaction)
      ;; Communication succeeded
      ;; Speaker and hearer increase the score of the constructions they used:
      (progn 
        (setf (attr-val applied-cxn-speaker :score)
              (calculate-increased-score (learning-rate speaker) (attr-val applied-cxn-speaker :score)))
        (setf (attr-val applied-cxn-hearer :score)
              (calculate-increased-score (learning-rate hearer) (attr-val applied-cxn-hearer :score))))
            
      ;; Communication failed 
      (progn
        (when (applied-constructions speaker)
          (setf (attr-val applied-cxn-speaker :score)
                (calculate-decreased-score (learning-rate speaker) (attr-val applied-cxn-speaker :score))))
        (notify alignment-finished speaker hearer interaction)))))

;; Don't punish in failure, still punish competitors in success. 

(defmethod align ((speaker naming-game-agent) (hearer naming-game-agent) (interaction crs-conventionality-interaction)
                  (mode (eql :dont-punish-failure)))
  "Align grammar of speaker and hearer based on interaction."
  (let ((applied-cxn-speaker (first (applied-constructions speaker)))
        (applied-cxn-hearer (first (applied-constructions hearer))))
    (if (communicated-successfully interaction)
      ;; Communication succeeded
      ;; Speaker and hearer increase the score of the constructions they used:
      (progn 
        (setf (attr-val applied-cxn-speaker :score)
              (calculate-increased-score (learning-rate speaker) (attr-val applied-cxn-speaker :score)))
        (setf (attr-val applied-cxn-hearer :score)
              (calculate-increased-score (learning-rate hearer) (attr-val applied-cxn-hearer :score)))
      
        ;; Speaker punishes competing constructions:
        (loop for cxn in (find-competitors speaker)
              do (setf (attr-val cxn :score) (calculate-decreased-score (learning-rate speaker) (attr-val cxn :score))))
        (loop for cxn in (find-competitors hearer)
              do (setf (attr-val cxn :score) (calculate-decreased-score (learning-rate hearer) (attr-val cxn :score)))))
      
      ;; Communication failed 
      (notify alignment-finished speaker hearer interaction))))

;; Don't punish in failure, don't punish competitors in success. 

(defmethod align ((speaker naming-game-agent) (hearer naming-game-agent) (interaction crs-conventionality-interaction)
                  (mode (eql :never-punish)))
  "Align grammar of speaker and hearer based on interaction."
  (let ((applied-cxn-speaker (first (applied-constructions speaker)))
        (applied-cxn-hearer (first (applied-constructions hearer))))
    (if (communicated-successfully interaction)
      ;; Communication succeeded
      ;; Speaker and hearer increase the score of the constructions they used:
      (progn 
        (setf (attr-val applied-cxn-speaker :score)
              (calculate-increased-score (learning-rate speaker) (attr-val applied-cxn-speaker :score)))
        (setf (attr-val applied-cxn-hearer :score)
              (calculate-increased-score (learning-rate hearer) (attr-val applied-cxn-hearer :score))))
      
      ;; Communication failed 
      (notify alignment-finished speaker hearer interaction))))

;; Never change scores. 


(defmethod align ((speaker crs-conventionality-agent) (hearer crs-conventionality-agent) (interaction crs-conventionality-interaction)
                  (mode (eql :no-alignment)))
  "No alignment setting - scores not adjusted."
  (notify alignment-finished speaker hearer interaction))


(defmethod find-competitors ((agent naming-game-agent))
  "Finds competitors in the cip"
  (loop for cxn in (constructions-list (grammar agent))
        when (and (eq (attr-val cxn :topic) (id (first (entities (topic agent)))))
                  (not (string= (attr-val cxn :form) (first (utterance agent)))))
          collect cxn))


(defmethod punish-competitors ((agent concept-emergence-game-agent))
  "Finds competitors in the cip in conceptualisation. Competitors are nodes that have a positive discriminative power (just the similarity - best-other-similarity, no entrenchment score). So get all solution-nodes (these are the ones that had a positive discriminative power > checked in goal-test). From these solution-nodes, get rest of solution-nodes (the first is the concept that was uttered, this list is sorted on discriminative-power * entrenchment. Then, from these solution-nodes, get discriminative power that is stored in the node: (cdr (find-data node :discriminative-power)) and check that is it positive (only sanity check because goal test should have done this). These are all the candidates. Then punish based on the concept-similarity and inhibition score. "
  (let* ((solution-nodes (if (speakerp agent) (solution-nodes agent) (solution-nodes-conceptualisation agent))))
    (when (> (length solution-nodes) 1) ; only do this when there are multiple solutions
      (let* ((cxn-of-uttered-word (if (speakerp agent)
                                    (car-applied-cxn (cipn-car (first solution-nodes)))
                                    (first (loop for node in solution-nodes
                                                 for form = (render (car-resulting-cfs (cipn-car node))
                                                                    (get-configuration (grammar agent) :render-mode))
                                                   
                                                 if (equal form (utterance agent))
                                                   collect (car-applied-cxn (cipn-car node))))))
             (concept-of-uttered-word (first (extract-concept cxn-of-uttered-word)))
             (candidate-cxns (if (speakerp agent)
                               (loop for node in (rest solution-nodes)
                                     when (> (cdr (find-data node :discriminative-power)) 0) ;; sanity check
                                       collect (car-applied-cxn (cipn-car node)))
                               (loop for node in solution-nodes
                                     when (not (equal (name (car-applied-cxn (cipn-car node)))
                                                      (name cxn-of-uttered-word)))
                                       collect (car-applied-cxn (cipn-car node))))))
        (loop for cxn in candidate-cxns
              for concept-of-candidate-cxn = (first (extract-concept cxn))
              for original-score = (attr-val cxn :score)
              for concept-concept-similarity = (concept-representations::concept-similarity concept-of-uttered-word concept-of-candidate-cxn)
              do (update-score-cxn cxn (- original-score (* -0.02 concept-concept-similarity))))))))



(defun calculate-increased-score (learning-rate score)
  "Increase the score using the interpolation rule and the learning rate."
  (+ learning-rate (* score (- 1 learning-rate))))


(defun calculate-decreased-score (learning-rate score)
  "Decrease the score using the interpolation rule and the learning rate."
  (* score (- 1 learning-rate)))


(defmethod adopt ((topic crs-conventionality-entity-set) (hearer naming-game-agent))
  "Adoption of the construction through composition."
  (fcg::add-repair (get-data (blackboard (grammar hearer)) :cipn) 'fcg::repair-through-adoption)
  ;; Notify learning
  (set-data (blackboard (grammar hearer)) :agent hearer)
  (let* ((fix (first (second (multiple-value-list (fcg::notify-learning (get-data (blackboard (grammar hearer)) :cipn) :trigger 'fcg::feedback-received))))))
    (when fix
      (let* ((cxn (meta-layer-learning:restart-data fix))
             (best-solution nil)
             (consolidated-cxns nil)
             (current-node (get-data (blackboard (grammar hearer)) :cipn))
             (fixed-car (first (get-data fix 'fcg::fixed-cars))) 
             (child (fcg::cip-add-child current-node fixed-car)))
      
        (setf current-node child)
        (push (type-of (fcg::issued-by fix)) (fcg::statuses child))
        (setf (fcg::fully-expanded? child) t)
        (fcg::cip-run-goal-tests child (cip (get-data (blackboard (grammar hearer)) :cipn))) ;; to include succeeded status in node statuses
        (push 'added-by-repair (fcg::statuses child))

        (fcg::add-cxn cxn (grammar hearer))
        (push cxn consolidated-cxns)
    
        (values cxn fix)))))


(defmethod adopt ((topic crs-conventionality-entity-set) (hearer concept-emergence-game-agent))
  "Adoption of the construction through composition."
    (fcg::add-repair (get-data (blackboard (grammar hearer)) :cipn) 'fcg::repair-through-concept-adoption)
    ;; Notify learning
    
    (set-data (blackboard (grammar hearer)) :agent hearer) ;; you need to have access to the agent once you are deeper into the metalayer and only have the construction-inventory
    (let* ((fix (first (second (multiple-value-list (fcg::notify-learning (get-data (blackboard (grammar hearer)) :cipn) :trigger 'fcg::feedback-received)))))
           (cxn (meta-layer-learning:restart-data fix))
           (best-solution nil)
           (consolidated-cxns nil)
           (current-node (get-data (blackboard (grammar hearer)) :cipn))
           (fixed-car (first (get-data fix 'fcg::fixed-cars))) 
           (child (fcg::cip-add-child current-node fixed-car)))
      
      (setf current-node child)
      (push (type-of (fcg::issued-by fix)) (fcg::statuses child))
      (setf (fcg::fully-expanded? child) t)
      (fcg::cip-run-goal-tests child (cip (get-data (blackboard (grammar hearer)) :cipn))) ;; to include succeeded status in node statuses
      (push 'added-by-repair (fcg::statuses child))

      (fcg::add-cxn cxn (grammar hearer))
      (push cxn consolidated-cxns)
    
      (values cxn fix))
  )

(defmethod shift ((construction fcg::construction) (entity-set crs-conventionality-entity-set) (agent concept-emergence-game-agent))
  "Shifting of the concept."
  (let ((concept-representation (first (extract-concept construction))) ;; you only can have 1 concept in a cxn, so take the first
        (context (entities (scene (current-interaction (experiment agent)))))
        (topic (first (entities entity-set))))
  (concept-representations::update-concept concept-representation topic context)
  ))

(defun extract-concept (cxn)
  (let ((structure (pole-structure (left-pole cxn))))
    (loop for unit in structure collect (extract-concept-from-unit unit 'topic))))

(defun extract-concept-from-unit (unit value)
  (unit-feature-value unit value))


(defmethod update-score-cxn (cxn delta &key (upper-bound 1.0) (lower-bound 0.0))
  "Updates the entrenchment score of a cxn."
  ;; update the score
  (setf (attr-val cxn :score) (+ (attr-val cxn :score) delta))
  ;; check the upper boundary
  (when (> (attr-val cxn :score) upper-bound)
    (setf (attr-val cxn :score) upper-bound))
  (when (< (attr-val cxn :score) lower-bound)
    (setf (attr-val cxn :score) lower-bound))
  ;(update-lexicon-inventory (lexicon agent) cxn)
  )
