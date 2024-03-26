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
    (destructuring-bind (applied-cxn . competitors) (find-best-concept agent (get-configuration (experiment agent) :conceptualisation-heuristics))
      ;; set competitors
      (set-data agent 'meaning-competitors competitors)
      ;; set the applied-cxn slot
      (set-data agent 'applied-cxn applied-cxn)
      applied-cxn)))

(defmethod hearer-conceptualise ((agent cle-agent))
  "Conceptualise the topic as the hearer"
  (if (empty-lexicon-p agent)
    nil
    (destructuring-bind (hypothetical-cxn . competitors) (find-best-concept agent (get-configuration (experiment agent) :conceptualisation-heuristics))
      ;; competitors: ALL discriminative concepts (thus both the hypothetical and competitors but NOT the interpreted cxn!)
      (let ((competitors (remove (find-data agent 'applied-cxn)
                                 (cons hypothetical-cxn competitors)
                                 :test #'(lambda (x y) (equal x y))))) ;; TODO: test nodig? 
        (set-data agent 'meaning-competitors competitors))
      ;; set the hypothetical-cxn slot
      (set-data agent 'hypothetical-cxn hypothetical-cxn)
      hypothetical-cxn)))

(defgeneric find-best-concept (agent mode)
  (:documentation "Finds the best concept (and its direct competitors) for a given scene and topic.")
  )

(defmethod find-best-concept ((agent cle-agent) (mode (eql :heuristic-1)))
  """Waterfall lazy stopping - with trash"""
  (destructuring-bind (fast-score fast-cxn fast-comps) (search-inventory agent :fast)
    (if fast-cxn
      ;; level 1: FAST memory
      (cons fast-cxn fast-comps)
      (destructuring-bind (slow-score slow-cxn slow-comps) (search-inventory agent :slow)
        (if slow-cxn
          ;; level 2: SLOW memory
          (cons slow-cxn slow-comps)
          (destructuring-bind (trash-score trash-cxn trash-comps) (search-inventory agent :trash)
            ;; level 3: TRASH (trash competitors should not be punished as they already have 0 entrenchment)
            (cons trash-cxn nil)))))))

(defmethod find-best-concept ((agent cle-agent) (mode (eql :heuristic-2)))
  """Waterfall lazy stopping - no trash"""
  (destructuring-bind (fast-score fast-cxn fast-comps) (search-inventory agent :fast)
    (if fast-cxn
      ;; level 1: FAST memory
      (cons fast-cxn fast-comps)
      (destructuring-bind (slow-score slow-cxn slow-comps) (search-inventory agent :slow)
        (if slow-cxn
          ;; level 2: SLOW memory
          (cons slow-cxn slow-comps)
          (cons nil nil))))))

;; --------------------------------------------
;; + Conceptualisation through discrimination +
;; --------------------------------------------

(defun search-inventory (agent inventory-name)
  """Searches an inventory for a concept.

   The best concept corresponds to the concept that maximises
   the multiplication of its entrenchment score and its discriminative power."""
  (loop with similarity-threshold = (get-configuration (experiment agent) :similarity-threshold)
        with topic = (get-data agent 'topic)
        with context = (remove topic (objects (get-data agent 'context)))
        with best-score = -1
        with best-cxn = nil
        with competitors = '()
        for cxn in (get-inventory (lexicon agent) inventory-name)
        for concept = (meaning cxn)
        for topic-sim = (weighted-similarity agent topic concept)
        for best-other-sim = (loop for object in context
                                   maximize (weighted-similarity agent object concept))
        for discriminative-power = (abs (- topic-sim best-other-sim))
        if (and (> topic-sim (+ best-other-sim similarity-threshold))
                (> (* discriminative-power (score cxn)) best-score))
          do (progn
               (when best-cxn
                 (setf competitors (cons best-cxn competitors)))
               (setf best-score (* discriminative-power (score cxn)))
               (setf best-cxn cxn))
        else
          do (setf competitors (cons cxn competitors))
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
