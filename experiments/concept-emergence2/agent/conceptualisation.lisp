(in-package :cle)

;; ---------------------
;; + Conceptualisation +
;; ---------------------

;; events - test
(define-event conceptualisation-start (agent cle-agent))

(define-event conceptualisation-finished
  (agent cle-agent)
  (discriminating-concepts list)
  (duplicate-sets list)
  (unique-concepts list)
  (applied-concept list))

(define-event conceptualisation-finished-times
  (agent cle-agent)
  (discriminating-concepts list)
  (applied-concept list))

(define-event coherence-event
  (experiment cle-experiment)
  (coherence symbol)
  (speaker-concept t)
  (hearer-concept t))

;; -----------------
;; + Conceptualise +
;; -----------------

(defgeneric conceptualise (agent)
  (:documentation "Run conceptualisation"))

(defmethod conceptualise (agent)
  ;; notify
  (notify conceptualisation-start agent)
  ;; conceptualise
  (case (discourse-role agent)
    (speaker (speaker-conceptualise agent (get-configuration agent :conceptualisation-strategy)))
    (hearer (hearer-conceptualise agent (get-configuration agent :conceptualisation-strategy)))))

;; -----------------------------
;; + General conceptualisation +
;; -----------------------------
(defmethod speaker-conceptualise ((agent cle-agent) (mode (eql :standard)))
  "Conceptualise the topic of the interaction."
  (if (length= (lexicon agent) 0)
    nil
    (let* ((discriminating-concepts (search-concepts agent))
           (duplicate-sets (find-duplicate-concept-sets discriminating-concepts 
                                                        :activation (get-configuration agent :concept-similarity-activation)))
           (unique-concepts (loop for set in duplicate-sets
                                  collect (select-best-entrenched-concept set)))
           (applied-concept (select-best-concept unique-concepts
                                                 (get-configuration agent :concept-selection-strategy)
                                                 :weights (get-configuration agent :concept-selection-weights))))
      ;; decides which concepts are considered during alignment
      (decide-competitors agent
                          discriminating-concepts ;; phase 1
                          duplicate-sets unique-concepts ;; phase 2a and 2b
                          applied-concept ;; chosen concept
                          (get-configuration agent :competitor-strategy))
      (set-data agent 'applied-concept applied-concept)
      (notify conceptualisation-finished agent discriminating-concepts duplicate-sets unique-concepts (list applied-concept)) ;; notify
      applied-concept)))

(defmethod hearer-conceptualise ((agent cle-agent) (mode (eql :standard)))
  (if (length= (lexicon agent) 0)
    nil
    (let* ((discriminating-concepts (search-concepts agent))
           (duplicate-sets (find-duplicate-concept-sets discriminating-concepts
                                                        :activation (get-configuration agent :concept-similarity-activation)))
           (unique-concepts (loop for set in duplicate-sets
                                  collect (select-best-entrenched-concept set)))
           (applied-concept (select-best-concept unique-concepts
                                                 (get-configuration agent :concept-selection-strategy)
                                                 :weights (get-configuration agent :concept-selection-weights))))
      applied-concept)))


;; ----------------------------------
;; + Search discriminative concepts +
;; ----------------------------------
(defmethod search-concepts ((agent cle-agent))
  "Discriminately conceptualise the topic relative to the context."
  (let ((topic (get-data agent 'topic))
        (context (objects (get-data agent 'context)))
        (discriminating-concepts '()))
    (loop for concept in (lexicon agent)
          for topic-similarity = (weighted-similarity topic concept)
          for best-other-similarity = (loop for object in (remove topic context)
                                            maximize (weighted-similarity object concept))
          when (> topic-similarity best-other-similarity)
            do (setf discriminating-concepts (cons (list (cons :concept concept)
                                                         (cons :topic-sim topic-similarity)
                                                         (cons :best-other-sim best-other-similarity))
                                                   discriminating-concepts)))
    discriminating-concepts))

;; ------------------------------------------
;; + Deciding priority of selected concepts +
;; ------------------------------------------
(defgeneric select-best-concept (concepts mode &key &allow-other-keys)
  (:documentation "Strategy to select the final conceptualised concept from a set of concepts."))

(defmethod select-best-concept (concepts (mode (eql :waterfall)) &key (weights nil))
  "Waterfall strategy: first highest entrenched, them highest topic similarity, then random."
  (let* ((highest-entrenched-concepts (all-biggest
                                       (lambda (x) (score (assqv :concept x)))
                                       concepts))
         (highest-topic-sim (all-biggest
                             (lambda (x) (assqv :topic-sim x))
                             highest-entrenched-concepts))
         (best-concept (random-elt highest-topic-sim)))
    (if best-concept
      (assqv :concept best-concept)
      nil)))

(defmethod select-best-concept (concepts (mode (eql :weighted-average)) &key
                                         (weights (list (cons :entrenchment-weight  1/3)
                                                        (cons :topic-similarity-weight 1/3)
                                                        (cons :similarity-distance-weight 1/3)))) ;; discriminative-power
  "Weighted average of
        1. the entrenchment score, [-1, 1]
        2. absolute topic-similarity [-inf, 1]
        3. discriminative-power [0, inf]." 
  (let ((best-score -1)
        (best-concept nil))
    (loop with w1 = (assqv :entrenchment-weight weights)
          with w2 = (assqv :topic-similarity-weight weights)
          with w3 = (assqv :similarity-distance-weight weights)
          for tuple in concepts
          for entrenchment = (score (assqv :concept tuple))
          for topic-sim = (sigmoid (assqv :topic-sim tuple))
          for best-other-sim = (sigmoid (assqv :best-other-sim tuple))
          for score = (+ (* entrenchment w1)
                         (* topic-sim w2)
                         (* (abs (- topic-sim best-other-sim)) w3))
          when (> score best-score)
            do (progn
                 (setf best-score score)
                 (setf best-concept (assqv :concept tuple))))
    best-concept))

(defmethod select-best-concept (concepts (mode (eql :times)) &key (weights nil)) ;; discriminative-power
  "Choose concept that maximises power * entrenchment"
  (let ((best-score -1)
        (best-concept nil))
    (loop for tuple in concepts
          for form = (form (assqv :concept tuple))
          for entrenchment = (score (assqv :concept tuple))
          for topic-sim = (sigmoid (assqv :topic-sim tuple))
          for best-other-sim = (sigmoid (assqv :best-other-sim tuple))
          for discriminative-power = (abs (- topic-sim best-other-sim))
          for score = (* entrenchment discriminative-power)
          when (> score best-score)
            do (progn
                 (setf best-score score)
                 (setf best-concept (assqv :concept tuple))))
    best-concept))

;; ---------------------
;; + Lexicon coherence +
;; ---------------------

(defun lexicon-coherence-p (experiment speaker hearer)
  "Records how coherent the lexicons of the interactings agents are for the topic.

   Coherence is measured by inspecting whether the hearer would produce
   the same utterance for the given topic inside the context (must be measured before alignment!)."
  (let* ((speaker-concept (find-data speaker 'applied-concept))
         (hearer-concept (conceptualise hearer))
         (coherence (if (and speaker-concept hearer-concept)
                      (string= (form speaker-concept) (form hearer-concept))
                      nil)))
    (notify coherence-event experiment coherence speaker-concept hearer-concept)
    coherence))
