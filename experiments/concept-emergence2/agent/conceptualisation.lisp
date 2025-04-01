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

(defmethod conceptualise ((agent cle-agent))
  ;; conceptualise ifo the role
  (case (discourse-role agent)
    (speaker (speaker-conceptualise agent))
    (hearer (hearer-conceptualise agent))))

;; -------------
;; + Algorithm +
;; -------------
(defmethod speaker-conceptualise ((agent cle-agent))
  "Conceptualise the topic of the interaction."
  (notify event-conceptualisation-start agent)
  (if (empty-lexicon-p agent)
    nil
    (let* ((topic  (get-data agent 'topic))
           (context (remove topic (objects (get-data agent 'context)))))
      (destructuring-bind (applied-cxn . competitors) (find-best-concept agent topic context)
        ;; set competitors
        (set-data agent 'meaning-competitors competitors)
        ;; set the applied-cxn slot
        (set-data agent 'applied-cxn applied-cxn)
        applied-cxn))))

(defmethod hearer-conceptualise ((agent cle-agent))
  "Conceptualise the topic as the hearer"
  (if (empty-lexicon-p agent)
    nil
    (if (conceptualised-p agent)
      ;; if already conceptualised, just return the result
      (find-data agent 'hypothetical-cxn)
      ;; get the topic and and context from the speaker's perspective
      (let* ((experiment (experiment agent))
             (speaker (speaker experiment))
             (hearer (hearer experiment))
             (topic (case (get-configuration experiment :coherence-perspective)
                      (:speaker (get-data speaker 'topic))
                      (:hearer (get-data hearer 'topic))))
             (context (case (get-configuration experiment :coherence-perspective)
                        (:speaker (remove topic (objects (get-data speaker 'context))))
                        (:hearer (remove topic (objects (get-data hearer 'context)))))))
        (destructuring-bind (hypothetical-cxn . competitors) (find-best-concept agent topic context)
          ;; hypothetical-cxn corresponds to the concept that hearer would have produces as a speaker
          (let* ((applied-cxn (find-data agent 'applied-cxn))
                 (competitors (if hypothetical-cxn
                                (cons hypothetical-cxn competitors)
                                competitors))
                 (all-competitors (remove applied-cxn competitors :test #'(lambda (x y) (equal x y)))))
            (set-data agent 'meaning-competitors all-competitors))
          ;; set the hypothetical-cxn slot
          (set-data agent 'hypothetical-cxn hypothetical-cxn)
          hypothetical-cxn)))))

;; --------------------------------------------
;; + Conceptualisation through discrimination +
;; --------------------------------------------

(defun find-best-concept (agent topic context)
  "Searches the lexicon for the best concept for the given topic and context.

  The agent first searches its fast inventory,
  if no cxn is found, it searches the trash inventory."
  (loop with all-competitors = nil
        for inventory-name in (list :fast :trash)
        for (best-score best-candidate competitors) = (search-inventory agent inventory-name topic context)
        ;; if a cxn is found, return it
        if best-candidate
          do (return (cons best-candidate
                           (if (eq inventory-name :trash)
                             all-competitors
                             (append all-competitors competitors))))
        else
          ;; add competitors
          do (setf all-competitors (append all-competitors competitors))
        finally
          ;; if nothing is found, return nil nil
          (return (cons nil all-competitors))))

(defmethod search-inventory (agent inventory-name topic context)
  "Searches an inventory for the best concept.

  The best concept corresponds to the concept that maximises the entrenchment score."
  (loop with best-score = -1
        with best-candidate = nil
        with competitors = '()
        for cxn being the hash-values of (get-inventory (lexicon agent) inventory-name)
        for concept = (meaning cxn)
        for score = (if (eq inventory-name :trash) 1 (score cxn))
        if (eq (get-topic-id topic) (obj-id concept))
          do (if (> score best-score)
               (progn
                 (when best-candidate
                   (push best-candidate competitors))
                 ;; update the best candidate
                 (setf best-score score)
                 (setf best-candidate cxn))
               ;; candidate is not better, push to competitors
               (push cxn competitors))
        finally
          (return (list best-score best-candidate competitors))))

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
    ;; return
    coherence))

;; helper function
(defun conceptualised-p (agent)
  (case (discourse-role agent)
    (speaker (find-data agent 'applied-cxn))
    (hearer (find-data agent 'hypothetical-cxn))))
