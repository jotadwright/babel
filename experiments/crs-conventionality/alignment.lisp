(in-package :crs-conventionality)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       ;;
;;  Alignment of agents  ;;
;;                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Alignment Strategies ;;
;; Lateral inhibition: reward used utterance and punish all competitors in success, punish used utterance and adopt in failure. 

(defmethod align ((speaker naming-game-agent) (hearer naming-game-agent) (interaction crs-conventionality-interaction)
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
      (notify alignment-finished speaker hearer interaction)))))

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


(defmethod align ((speaker naming-game-agent) (hearer naming-game-agent) (interaction crs-conventionality-interaction)
                  (mode (eql :no-alignment)))
  "No alignment setting - scores not adjusted."
  (notify alignment-finished speaker hearer interaction))


(defmethod find-competitors ((agent naming-game-agent))
  "Finds competitors in the cip"
  ;; TODO to keep?
  #|(set-difference (remove-duplicates (mappend #'fcg::applied-constructions (succeeded-nodes (cip (solution-node agent)))))
                  (applied-constructions agent))|#
  (loop for cxn in (constructions-list (grammar agent))
        when (and (eq (attr-val cxn :topic) (id (first (entities (topic agent)))))
                  (not (string= (attr-val cxn :form) (first (utterance agent)))))
          collect cxn))


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

          (notify adoption-finished cxn (invention (current-interaction (experiment hearer))))
          
          (values cxn fix)))))
