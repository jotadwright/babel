(in-package :cle)

;; -------------
;; + Alignment +
;; -------------

;; events
(define-event event-align-start (agent cle-agent))
(define-event event-align-cxn
              (reason string)
              (cxn cxn)
              (previous-copy cxn))

(defmethod align ((agent cle-agent))
  (notify event-align-start agent)
  (case (discourse-role agent)
    (speaker (speaker-alignment agent))
    (hearer (hearer-alignment agent))))

;; ---------------------
;; + Speaker Alignment +
;; ---------------------
(defmethod speaker-alignment ((agent cle-agent))
  "Speaker alignment."
  (let* ((topic (find-data agent 'topic))
         (context (get-data agent 'context))
         (applied-cxn (find-data agent 'applied-cxn))
         (previous-copy (copy-object applied-cxn)))
    (when (not (invented-or-adopted agent))
      ;; when the speaker has not invented
      (if (communicated-successfully agent)
        ;; if success,
        (progn
          ;;  1. entrench applied-cxn
          (update-score-cxn agent applied-cxn (get-configuration (experiment agent) :entrenchment-incf))
          ;;  2. shift concept of applied-cxn to topic
          (concept-representations::update-concept (meaning applied-cxn)
                                                   topic
                                                   (entities context)
                                                   :weight-incf (get-configuration (experiment agent) :weight-incf)
                                                   :weight-decf (get-configuration (experiment agent) :weight-decf))
          ;;  3. punish competing similar cxns TODO: punishment is based on shifted concept
          (loop for other-cxn in (find-data agent 'meaning-competitors)
                for similarity = (concept-representations::concept-similarity (meaning applied-cxn)
                                                                              (meaning other-cxn)
                                                                              (get-configuration (experiment agent) :f-divergence))
                for delta = (* similarity (get-configuration (experiment agent) :entrenchment-li))
                do (update-score-cxn agent other-cxn delta))
          ;; notify
          (notify event-align-cxn "Entrench and shift" applied-cxn previous-copy))
        ;; otherwise, entrench used cxn negatively
        (progn
          (update-score-cxn agent applied-cxn (get-configuration (experiment agent) :entrenchment-decf))
          (notify event-align-cxn "Punish (due to failure)" applied-cxn previous-copy))))))

;; --------------------
;; + Hearer Alignment +
;; --------------------
(defmethod hearer-alignment ((agent cle-agent))
  "Hearer alignment."
  (let* ((topic (get-data agent 'topic))
         (context (get-data agent 'context))
         (applied-cxn (find-data agent 'applied-cxn))
         (previous-copy (copy-object applied-cxn)))
    (case applied-cxn
      ;; CASE A: hearer did not recognize the word
      ((nil)
       (adopt agent topic (utterance agent)))
      (otherwise 
      ;; CASE B: hearer did recognize the word
       (cond (;; CASE A: recognized + communication was successful!
              (communicated-successfully agent)
              (progn
                ;; 1. entrench applied-cxn
                (update-score-cxn agent applied-cxn (get-configuration (experiment agent) :entrenchment-incf))
                ;; 2. shift concept of applied-cxn to topic
                (concept-representations::update-concept (meaning applied-cxn)
                                                         topic
                                                         (entities context)
                                                         :weight-incf (get-configuration (experiment agent) :weight-incf)
                                                         :weight-decf (get-configuration (experiment agent) :weight-decf))
                ;; 3. find and punish meaning competitors TODO: punishment is based on shifted concept
                (conceptualise agent)
                (loop for other-cxn in (find-data agent 'meaning-competitors)
                      for similarity = (concept-representations::concept-similarity (meaning applied-cxn)
                                                                                    (meaning other-cxn)
                                                                                    (get-configuration (experiment agent) :f-divergence))
                      for delta = (* similarity (get-configuration (experiment agent) :entrenchment-li))
                      do (update-score-cxn agent other-cxn delta))
                ;; notify
                (notify event-align-cxn "Entrench and shift" applied-cxn previous-copy)))
             ;; CASE B: recognized but did not point to correct word
             ((or (get-data agent 'interpreted-topic)
                  (eq (get-data agent 'interpreted-topic-reason) 'more-candidates))
              (progn
                ;; 1. entrench applied-cxn negatively
                (update-score-cxn agent applied-cxn (get-configuration (experiment agent) :entrenchment-decf))
                ;; 2. shift concept of applied-cxn to topic
                (concept-representations::update-concept (meaning applied-cxn)
                                                         topic
                                                         (entities context)
                                                         :weight-incf (get-configuration (experiment agent) :weight-incf)
                                                         :weight-decf (get-configuration (experiment agent) :weight-decf))
                (notify event-align-cxn "Punish (due to failure) and shift" applied-cxn previous-copy)))
             ;; CASE C: recognized but used concept is useless due to defects
             ((eq (get-data agent 'interpreted-topic-reason) 'no-match)
              (progn
                (reset-adopt agent applied-cxn topic)
                (notify event-align-cxn "Concept is useless, shifting" applied-cxn previous-copy))))))))
