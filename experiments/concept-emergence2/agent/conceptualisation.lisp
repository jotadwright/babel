(in-package :cle)

;; ---------------------
;; + Conceptualisation +
;; ---------------------

;; events - test
(define-event conceptualisation-start (agent cle-agent))

(define-event conceptualisation-finished
  (agent cle-agent)
  (discriminating-concepts list)
  (similar-sets list)
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

(defmethod conceptualise ((agent cle-agent))
  ;; notify
  (notify conceptualisation-start agent)
  ;; conceptualise
  (case (discourse-role agent)
    (speaker (speaker-conceptualise agent))
    (hearer (hearer-conceptualise agent))))

;; -----------------------------
;; + General conceptualisation +
;; -----------------------------
(defmethod speaker-conceptualise ((agent cle-agent))
  "Conceptualise the topic of the interaction."
  (if (length= (lexicon agent) 0)
    nil
    (let* (;; step 1 - find the discriminating concepts
           (discriminating-concepts (search-concepts agent))
           ;; step 2 - find similar concepts and sort them into sets where concepts are similar
           (similar-sets (find-similar-concepts-into-sets discriminating-concepts 
                                                          :activation (get-configuration agent :concept-similarity-activation)))
           ;; step 3 - find the best entrenched concept in each set
           (unique-concepts (loop for set in similar-sets
                                  collect (select-best-entrenched-concept set)))
           ;; step 4 - find the concept with the most discriminative power
           (applied-concept (select-most-discriminating-concept unique-concepts)))
      ;; decides which concepts are considered during alignment
      (decide-competitors-speaker agent
                                  discriminating-concepts ;; phase 1
                                  similar-sets ;; phase 2a
                                  unique-concepts ;; phase 2b
                                  applied-concept ;; phase 3: most-discriminating -> applied
                                  )
      (set-data agent 'applied-concept applied-concept)
      (notify conceptualisation-finished agent discriminating-concepts similar-sets unique-concepts (list applied-concept)) ;; notify
      applied-concept)))

(defmethod hearer-conceptualise ((agent cle-agent))
  (if (length= (lexicon agent) 0)
    nil
    (let* ((discriminating-concepts (search-concepts agent))
           (similar-sets (find-similar-concepts-into-sets discriminating-concepts
                                                          :activation (get-configuration agent :concept-similarity-activation)))
           (unique-concepts (loop for set in similar-sets
                                  collect (select-best-entrenched-concept set)))
           (applied-concept (select-most-discriminating-concept unique-concepts)))
      applied-concept)))

;; ----------------------------------
;; + Search discriminative concepts +
;; ----------------------------------
(defmethod search-concepts ((agent cle-agent))
  "Discriminately conceptualise the topic relative to the context."
  (let ((topic (get-data agent 'topic))
        (context (objects (get-data agent 'context)))
        (discriminating-concepts '()))
    (loop for construction in (lexicon agent)
          for concept = (meaning construction)
          for topic-similarity = (weighted-similarity topic concept)
          for best-other-similarity = (loop for object in (remove topic context)
                                            maximize (weighted-similarity object concept))
          when (> topic-similarity best-other-similarity)
            do (setf discriminating-concepts (cons (list (cons :construction construction)
                                                         (cons :topic-sim topic-similarity)
                                                         (cons :best-other-sim best-other-similarity))
                                                   discriminating-concepts)))
    discriminating-concepts))

;; ------------------------------------------
;; + Deciding priority of selected concepts +
;; ------------------------------------------
(defmethod select-most-discriminating-concept (concepts)
   "Selects the concept with the most discriminative-power [0, inf]." 
  (let ((best-score -1)
        (best-concept nil))
    (loop for tuple in concepts
          for topic-sim = (sigmoid (assqv :topic-sim tuple)) ;; sigmoid-ed!
          for best-other-sim = (sigmoid (assqv :best-other-sim tuple)) ;; sigmoid-ed!
          for discriminative-power = (abs (- topic-sim best-other-sim))
          when (> discriminative-power best-score)
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
