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
    (destructuring-bind (applied-cxn . competitors) (find-best-concept agent)
      ;; set competitors
      (set-data agent 'meaning-competitors competitors)
      ;; set the applied-cxn slot
      (set-data agent 'applied-cxn applied-cxn)
      applied-cxn)))

(defmethod hearer-conceptualise ((agent cle-agent))
  "Conceptualise the topic as the hearer"
  (if (empty-lexicon-p agent)
    nil
    (if (conceptualised-p agent)
      ;; if already conceptualised, just return the result
      (find-data agent 'hypothetical-cxn)
      (destructuring-bind (hypothetical-cxn . competitors) (find-best-concept agent)
        ;; hypothetical-cxn corresponds to the concept that hearer would have produces as a speaker
        (if (get-configuration (experiment agent) :hearer-competition-alignment)
          (let* ((applied-cxn (find-data agent 'applied-cxn))
                (competitors (if hypothetical-cxn
                                (cons hypothetical-cxn competitors)
                                competitors))
                (all-competitors (remove applied-cxn competitors :test #'(lambda (x y) (equal x y)))))
            (set-data agent 'meaning-competitors all-competitors))
          (set-data agent 'meaning-competitors nil))
        ;; set the hypothetical-cxn slot
        (set-data agent 'hypothetical-cxn hypothetical-cxn)
        hypothetical-cxn))))

;; --------------------------------------------
;; + Conceptualisation through discrimination +
;; --------------------------------------------

(defun calculate-max-similarity-in-context (agent concept context topic-sim)
  """Calculates the maximim similarity between the given concept and all objects in the context."
  (loop named lazy-loop
        for object in context
        for other-sim = (weighted-similarity agent object concept)
        when (<= topic-sim other-sim)
          ;; lazy stopping
          do (return-from lazy-loop other-sim)
        maximize other-sim))

(defun find-best-concept (agent)
  "Finds the best concept (and its direct competitors) for a given scene and topic.

   The best concept corresponds to the concept that maximises
   the multiplication of its entrenchment score and its discriminative power."
  (loop with all-competitors = nil
        for inventory-name in (list :fast :trash)
        for (best-score best-cxn competitors) = (search-inventory agent inventory-name)
        ;; if a cxn is found, return it
        if best-cxn
          do (return (cons best-cxn
                          (if (eq inventory-name :trash)
                            all-competitors
                            (append all-competitors competitors))))
        else
          ;; add competitors
          do (setf all-competitors (append all-competitors competitors))
        finally
          ;; if nothing is found, return nil nil
          (return (cons nil all-competitors))))

(defmethod search-inventory (agent inventory-name)
  "Searches an inventory for the best concept.

  The best concept corresponds to the concept that maximises
  the multiplication of its entrenchment score and its discriminative power."
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
        for best-other-sim = (calculate-max-similarity-in-context agent concept context topic-sim)
        for discriminative-power = (abs (- topic-sim best-other-sim))
        ;; tricky hack: if the inventory is the trash, we do not have to check the score
        for score = (if (eq inventory-name :trash) 1 (score cxn))
        ;; new candidate cxn
        if (> topic-sim (+ best-other-sim similarity-threshold))
          do (if (> (* discriminative-power score) best-score)
                (progn
                  (when best-cxn
                    (push best-cxn competitors))
                  (setf best-score (* discriminative-power score))
                  (setf best-cxn cxn))
                (push cxn competitors))
        else 
          do (if (get-configuration (experiment agent) :punish-non-candidates)
                (push cxn competitors))
        finally
          (return (list best-score best-cxn competitors))))

;; ---------------------
;; + Lexicon coherence +
;; ---------------------
(defun lexicon-coherence-p (experiment speaker hearer)
  "Checks coherence of the lexicon of the interacting-agents for the current scene.

   Coherence is measured by inspecting whether the hearer would produce
   the same utterance for the given topic inside the context
   (must be measured before alignment!)."
  (let* ((speaker-cxn (find-data speaker 'applied-cxn))
         (hearer-cxn (conceptualise hearer))
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
