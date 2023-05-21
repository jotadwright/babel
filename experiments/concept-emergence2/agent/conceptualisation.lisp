(in-package :cle)

;; -----------------
;; + Conceptualise +
;; -----------------

(defmethod conceptualise ((agent cle-agent))
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
           (discriminating-concepts (search-discriminative-concepts agent))
           ;; step 2 - find similar concepts and sort them into sets where concepts are similar
           (similar-sets (find-similar-concepts-into-sets discriminating-concepts 
                                                          :activation (get-configuration agent :concept-similarity-activation)))
           ;; step 3 - find the best entrenched concept in each set
           (unique-concepts (loop for set in similar-sets
                                  collect (select-best-entrenched-concept set)))
           ;; step 4 - find the concept with the most discriminative power
           (applied-cxn (select-most-discriminating-concept unique-concepts)))
      ;; decides which concepts are considered during alignment
      (decide-competitors-speaker agent
                                  discriminating-concepts ;; phase 1
                                  similar-sets ;; phase 2a
                                  unique-concepts ;; phase 2b
                                  applied-cxn ;; phase 3: most-discriminating -> applied
                                  )
      (set-data agent 'applied-cxn applied-cxn)
      applied-cxn)))

(defmethod hearer-conceptualise ((agent cle-agent))
  (if (length= (lexicon agent) 0)
    nil
    (let* (;; step 1 - find the discriminating concepts
           (discriminating-concepts (search-discriminative-concepts agent))
           ;; step 2 - find similar concepts and sort them into sets where concepts are similar
           (similar-sets (find-similar-concepts-into-sets discriminating-concepts
                                                          :activation (get-configuration agent :concept-similarity-activation)))
           
           ;; step 3 - find the best entrenched concept in each set
           (unique-concepts (loop for set in similar-sets
                                  collect (select-best-entrenched-concept set)))
           ;; step 4 - find the concept with the most discriminative power
           (applied-cxn (select-most-discriminating-concept unique-concepts)))
      applied-cxn)))

;; ----------------------------------
;; + Search discriminative concepts +
;; ----------------------------------
(defmethod search-discriminative-concepts ((agent cle-agent))
  "Discriminately conceptualise the topic relative to the context."
  (let ((topic (get-data agent 'topic))
        (context (objects (get-data agent 'context)))
        (discriminating-cxns '()))
    (loop for cxn in (lexicon agent)
          for concept = (meaning cxn)
          for topic-similarity = (weighted-similarity topic concept)
          for best-other-similarity = (loop for object in (remove topic context)
                                            maximize (weighted-similarity object concept))
          when (> topic-similarity best-other-similarity)
            do (setf discriminating-cxns (cons (list (cons :cxn cxn)
                                                     (cons :topic-sim topic-similarity)
                                                     (cons :best-other-sim best-other-similarity))
                                               discriminating-concepts)))
    discriminating-cxns))

;; ------------------------------------------
;; + Deciding priority of selected concepts +
;; ------------------------------------------
(defmethod select-most-discriminating-concept (cxns)
   "Selects the concept with the most discriminative-power [0, inf]." 
  (let ((best-score -1)
        (best-cxn nil))
    (loop for tuple in cxns
          for topic-sim = (sigmoid (assqv :topic-sim tuple)) ;; sigmoid-ed!
          for best-other-sim = (sigmoid (assqv :best-other-sim tuple)) ;; sigmoid-ed!
          for discriminative-power = (abs (- topic-sim best-other-sim))
          when (> discriminative-power best-score)
            do (progn
                 (setf best-score score)
                 (setf best-cxn (assqv :cxn tuple))))
    best-cxn))

;; ---------------------
;; + Lexicon coherence +
;; ---------------------

(defun lexicon-coherence-p (experiment speaker hearer)
  "Records how coherent the lexicons of the interactings agents are for the topic.

   Coherence is measured by inspecting whether the hearer would produce
   the same utterance for the given topic inside the context (must be measured before alignment!)."
  (let* ((speaker-cxn (find-data speaker 'applied-cxn))
         (hearer-cxn (conceptualise hearer))
         (coherence (if (and speaker-cxn hearer-cxn)
                      (string= (form speaker-cxn) (form hearer-cxn))
                      nil)))
    coherence))
