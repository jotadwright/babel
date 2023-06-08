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
    (speaker (speaker-conceptualise agent (get-configuration agent :strategy)))
    (hearer (hearer-conceptualise agent (get-configuration agent :strategy)))))

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

(defun sigmoid (x &key (c -1/2))
  "Sigmoid function where c changes the slope of the function. 
  
    When c is a fraction the slope is less steep, when c is a larger the slope is steeper."
  (/ 1 (+ 1 (exp (* c x)))))

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

;; -------------
;; + Algorithm +
;; -------------
(define-event event-conceptualisation-end
  (agent cle-agent)
  (discriminating-cxns list)
  (applied-cxn list))

(defmethod speaker-conceptualise ((agent cle-agent) (mode (eql :times)))
  "Conceptualise the topic of the interaction."
  (if (length= (lexicon agent) 0)
    nil
    (let* (;; step 1 - find the discriminating concepts
           (discriminating-cxns (search-discriminative-concepts agent))
           ;; step 2 - find the concept that maximises entrenchment * discriminative power
           (applied-cxn (select-most-discriminating-concept discriminating-cxns mode)))
      ;; decides which concepts are considered during alignment
      (decide-competitors-speaker agent
                                  applied-cxn
                                  discriminating-cxns
                                  mode)
      ;; set the applied-cxn slot
      (set-data agent 'applied-cxn applied-cxn)
      ;; notify
      (notify event-conceptualisation-end
              agent
              discriminating-cxns
              (list applied-cxn))
      applied-cxn)))

(defmethod hearer-conceptualise ((agent cle-agent) (mode (eql :times)))
  (if (length= (lexicon agent) 0)
    nil
    (let* (;; step 1 - find the discriminating concepts
           (discriminating-cxns (search-discriminative-concepts agent))
           ;; step 2 - find the concept that maximises entrenchment * discriminative power
           (applied-cxn (select-most-discriminating-concept discriminating-cxns mode)))
      applied-cxn)))

(defmethod select-most-discriminating-concept (cxns (mode (eql :times)))
  "Selets the concept that maximises power * entrenchment."
  (let* ((best-score -1)
         (best-cxn nil)
         (ledger (loop for tuple in cxns
                       ;; discriminative-power
                       for topic-sim = (sigmoid (assqv :topic-sim tuple)) ;; sigmoid-ed!
                       for best-other-sim = (sigmoid (assqv :best-other-sim tuple)) ;; sigmoid-ed!
                       sum (abs (- topic-sim best-other-sim)))))
    (loop for tuple in cxns
          ;; discriminative-power
          for topic-sim = (sigmoid (assqv :topic-sim tuple)) ;; sigmoid-ed!
          for best-other-sim = (sigmoid (assqv :best-other-sim tuple)) ;; sigmoid-ed!
          for discriminative-power = (/ (abs (- topic-sim best-other-sim)) (if (not (eq ledger 0.0)) ledger 1e-4))
          ;; entrenchment
          for entrenchment = (score (assqv :cxn tuple))
          ;; combine both
          for score = (* discriminative-power entrenchment)
          when (> score best-score)
            do (progn
                 (setf best-score score)
                 (setf best-cxn (assqv :cxn tuple))))
    best-cxn))


