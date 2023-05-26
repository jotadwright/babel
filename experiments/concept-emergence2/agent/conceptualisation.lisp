(in-package :cle)

;; -----------------
;; + Conceptualise +
;; -----------------

;; events - test
(define-event event-conceptualisation-start (agent cle-agent))
(define-event event-conceptualisation-end
  (agent cle-agent)
  (discriminating-cxns list)
  (similar-sets list)
  (best-entrenched-cxns list)
  (applied-cxn list))
(define-event event-coherence-p
  (experiment cle-experiment)
  (coherence symbol)
  (speaker-cxn t)
  (hearer-cxn t))


(defmethod conceptualise ((agent cle-agent))
  ;; notify
  (notify event-conceptualisation-start agent)
  ;; conceptualise ifo the role
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
           (discriminating-cxns (search-discriminative-concepts agent))
           ;; step 2 - find similar concepts and sort them into sets in which concepts are similar
           (similar-sets (find-similar-concepts-into-sets
                          discriminating-cxns
                          :activation (get-configuration agent :concept-similarity-activation)))
           ;; step 3 - find the best entrenched concept in each set
           (best-entrenched-cxns (loop for set in similar-sets
                                  collect (select-best-entrenched-concept set)))
           ;; step 4 - find the concept with the most discriminative power
           (applied-cxn (select-most-discriminating-concept best-entrenched-cxns)))
      ;; decides which concepts are considered during alignment
      (decide-competitors-speaker agent
                                  applied-cxn ;; phase 3: most-discriminating -> applied
                                  similar-sets ;; phase 2a
                                  )
      ;; set the applied-cxn slot
      (set-data agent 'applied-cxn applied-cxn)
      ;; notify
      (notify event-conceptualisation-end
              agent
              discriminating-cxns
              similar-sets
              best-entrenched-cxns
              (list applied-cxn))
      applied-cxn)))

(defmethod hearer-conceptualise ((agent cle-agent))
  (if (length= (lexicon agent) 0)
    nil
    (let* (;; step 1 - find the discriminating concepts
           (discriminating-cxns (search-discriminative-concepts agent))
           ;; step 2 - find similar concepts and sort them into sets where concepts are similar
           (similar-sets (find-similar-concepts-into-sets discriminating-cxns
                                                          :activation (get-configuration agent :concept-similarity-activation)))
           
           ;; step 3 - find the best entrenched concept in each set
           (best-entrenched-cxns (loop for set in similar-sets
                                  collect (select-best-entrenched-concept set)))
           ;; step 4 - find the concept with the most discriminative power
           (applied-cxn (select-most-discriminating-concept best-entrenched-cxns)))
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
                                               discriminating-cxns)))
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
                 (setf best-score discriminative-power)
                 (setf best-cxn (assqv :cxn tuple))))
    best-cxn))

(defun sigmoid (x)
  (/ 1 (+ 1 (exp (* -1 x)))))

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
    (notify event-coherence-p experiment coherence speaker-cxn hearer-cxn)
    coherence))
