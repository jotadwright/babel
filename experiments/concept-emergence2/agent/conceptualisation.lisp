(in-package :cle)

;; -----------------
;; + Conceptualise +
;; -----------------

;; events - test
(define-event event-conceptualisation-start (agent cle-agent))
(define-event event-conceptualisation-end
  (agent cle-agent)
  (discriminating-cxns list)
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

;; -------------
;; + Algorithm +
;; -------------
(defmethod speaker-conceptualise ((agent cle-agent))
  "Conceptualise the topic of the interaction."
  (if (empty-lexicon-p agent)
    nil
    (destructuring-bind (applied-cxn . competitors) (find-best-concept agent 
                                                                       (get-configuration (experiment agent) :conceptualisation-heuristics)
                                                                       :all-competitors-p (get-configuration (experiment agent) :speaker-competitors))
      ;; set competitors
      (set-data agent 'meaning-competitors competitors)
      ;; set the applied-cxn slot
      (set-data agent 'applied-cxn applied-cxn)
      applied-cxn)))

(defmethod hearer-conceptualise ((agent cle-agent))
  "Conceptualise the topic as the hearer"
  (if (empty-lexicon-p agent)
    nil
    (destructuring-bind (hypothetical-cxn . competitors) (find-best-concept agent 
                                                                       (get-configuration (experiment agent) :conceptualisation-heuristics)
                                                                       :all-competitors-p (get-configuration (experiment agent) :hearer-competitors))
      (let ((all-competitors (remove (find-data agent 'applied-cxn)
                                 (if hypothetical-cxn
                                   (cons hypothetical-cxn competitors)
                                   competitors))))
        (set-data agent 'meaning-competitors all-competitors))
      (set-data agent 'hypothetical-cxn hypothetical-cxn)
      hypothetical-cxn)))

(defmethod find-best-concept ((agent cle-agent) (mode (eql :heuristic-1)) &key all-competitors-p)
  "Finds the best concept (and its direct competitors) for a given scene and topic.

   The best concept corresponds to the concept that maximises
   the multiplication of its entrenchment score and its discriminative power."
  (loop with all-competitors = nil
        for inventory-name in (list :fast :slow :trash)
        for (best-score best-cxn competitors) = (search-inventory agent inventory-name :all-competitors-p all-competitors-p)
        if best-cxn
          do (return (cons best-cxn
                           ;; trash competitors do not need to be punished
                           (if (eq inventory-name :trash)
                             all-competitors
                             (append all-competitors competitors))))
        else
          ;; compound competitors
          do (setf all-competitors (append all-competitors competitors))
        finally
          ;; if nothing is found, return nil nil
          (return (cons nil nil))))

;; --------------------------------------------
;; + Conceptualisation through discrimination +
;; --------------------------------------------

(defun search-inventory (agent inventory-name &key all-competitors-p)
  """Searches an inventory for a concept.

   The best concept corresponds to the concept that maximises
   the multiplication of its entrenchment score and its discriminative power."""
  (loop with similarity-threshold = (get-configuration (experiment agent) :similarity-threshold)
        with topic = (get-data agent 'topic)
        with context = (remove topic (objects (get-data agent 'context)))
        with best-score = -1
        with best-cxn = nil
        with competitors = '()
        ;; iterate
        for cxn being the hash-values of (get-inventory (lexicon agent) inventory-name)
        for concept = (meaning cxn)
        for topic-sim = (weighted-similarity agent topic concept)
        for best-other-sim = (loop named bos-loop
                                   for object in context
                                   for other-sim = (weighted-similarity agent object concept)
                                   when (<= topic-sim other-sim)
                                     ;; lazy stopping
                                     do (return-from bos-loop other-sim)
                                   maximize other-sim)
        for discriminative-power = (abs (- topic-sim best-other-sim))
        ;; new candidate cxn
        if (and (> topic-sim (+ best-other-sim similarity-threshold))
                (> (* discriminative-power (score cxn)) best-score))
          do (progn
               (when best-cxn
                 (setf competitors (cons best-cxn competitors)))
               (setf best-score (* discriminative-power (score cxn)))
               (setf best-cxn cxn))
        ;; save as competitor TODO
        else
          do (when all-competitors-p
               (setf competitors (cons cxn competitors)))
        finally (return (list best-score best-cxn competitors))))

;; ---------------------
;; + Lexicon coherence +
;; ---------------------
(defun lexicon-coherence-p (experiment speaker hearer)
  "Checks coherence of the lexicon of the interacting-agents for the current scene.

   Coherence is measured by inspecting whether the hearer would produce
   the same utterance for the given topic inside the context
   (must be measured before alignment!)."
  (let* ((speaker-cxn (find-data speaker 'applied-cxn))
         (hearer-cxn (if (conceptualised-p hearer)
                       (find-data hearer 'hypothetical-cxn)
                       (conceptualise hearer)))
         (coherence (if (and speaker-cxn hearer-cxn)
                      (string= (form speaker-cxn) (form hearer-cxn))
                      nil)))
    ;; notify
    (notify event-coherence-p experiment coherence speaker-cxn hearer-cxn)
    ;; return
    coherence))

;; helper function
(defun conceptualised-p (agent)
  (case (discourse-role agent)
    (speaker (find-data agent 'applied-cxn))
    (hearer (find-data agent 'hypothetical-cxn))))
