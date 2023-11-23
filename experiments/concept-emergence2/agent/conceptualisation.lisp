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
  (if (length= (lexicon agent) 0)
    nil
    (destructuring-bind (applied-cxn . competitors) (find-best-concept agent)
      ;; set competitors
      (set-data agent 'meaning-competitors competitors)
      ;; set the applied-cxn slot
      (set-data agent 'applied-cxn applied-cxn)
      applied-cxn)))

(defmethod hearer-conceptualise ((agent cle-agent))
  "Conceptualise the topic as the hearer"
  (if (length= (lexicon agent) 0)
    nil
    (destructuring-bind (applied-cxn . competitors) (find-best-concept agent)
      applied-cxn)))

(defun find-best-concept (agent)
  "Finds the best concept (and its direct competitors) for a given scene and topic.

   The best concept corresponds to the concept that maximises
   the multiplication of its entrenchment score and its discriminative power."
  (let* ((threshold (get-configuration (experiment agent) :similarity-threshold))
         (topic (get-data agent 'topic))
         (context (remove topic (objects (get-data agent 'context))))
         (best-score -1)
         (best-cxn nil)
         (competitors '()))
    ;; case 1: look only at entrenched concepts first
    ;; this heuristic is possible as the score is based on the multiplication
    (loop for cxn in (lexicon agent)
          for concept = (meaning cxn)
          for topic-sim = (weighted-similarity agent topic concept)
          for best-other-sim = (loop for object in context
                                     maximize (weighted-similarity agent object concept))
          for discriminative-power = (abs (- topic-sim best-other-sim))
          if (and (> topic-sim (+ best-other-sim threshold))
                  (> (* discriminative-power (score cxn)) best-score))
            do (progn
                 (when best-cxn
                   (setf competitors (cons best-cxn competitors)))
                 (setf best-score (* discriminative-power (score cxn)))
                 (setf best-cxn cxn))
          else
            do (setf competitors (cons cxn competitors)))
    (if best-cxn
      (cons best-cxn competitors)
      ;; case 2: if no cxn is found -> look in trash
      (let* ((best-score -1)
             (best-cxn nil))
        (loop for cxn in (trash agent)
              for concept = (meaning cxn)
              for topic-sim = (weighted-similarity agent topic concept)
              for best-other-sim = (loop for object in context maximize (weighted-similarity agent object concept))
              for discriminative-power = (abs (- topic-sim best-other-sim))
              if (and (> topic-sim (+ best-other-sim threshold))
                      (> discriminative-power best-score))
                do (progn
                     (setf best-score discriminative-power)
                     (setf best-cxn cxn)))
        (cons best-cxn competitors)))))

;; ----------------------------------
;; + Search discriminative concepts +
;; ----------------------------------
(defmethod search-discriminative-concepts ((agent cle-agent))
  "Function to only determine the competitors of a cxn.

   Only used by the hearer when it punishes in the case of success
   the competitors of the applied cxn.
   Therefore, this function will only look into the lexicon for
   competitors as all competitors in the trash already have an
   entrenchment score of zero."
  (let ((threshold (get-configuration (experiment agent) :similarity-threshold))
        (topic (get-data agent 'topic))
        (context (objects (get-data agent 'context)))
        (discriminating-cxns '()))
    (loop for cxn in (lexicon agent)
          for concept = (meaning cxn)
          for topic-sim = (weighted-similarity agent topic concept)
          for best-other-sim = (loop for object in context
                                     maximize (weighted-similarity agent object concept))
          when (> topic-sim (+ best-other-sim threshold))
            do (setf discriminating-cxns (cons (list (cons :cxn cxn)
                                                     (cons :topic-sim topic-sim)
                                                     (cons :best-other-sim best-other-sim))
                                               discriminating-cxns)))
    discriminating-cxns))

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
  (find-data agent 'applied-cxn))
