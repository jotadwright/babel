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
          (shift-concept agent topic (meaning applied-cxn))
          ;;  3. punish competing similar cxns
          (loop for other-cxn in (find-data agent 'meaning-competitors)
                for similarity = (similar-concepts agent (meaning applied-cxn) (meaning other-cxn))
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
         (applied-cxn (find-data agent 'applied-cxn))
         (previous-copy (copy-object applied-cxn)))
    (case applied-cxn
      ;; CASE A: hearer did not recognize the word
      ((nil)
       (adopt agent topic (utterance agent)))
      ;; CASE B: hearer did recognize the word
      (otherwise 
       (cond ((communicated-successfully agent)
              ;; CASE A: recognized + communication was successful!
              (progn
                ;; 1. entrench applied-cxn
                (update-score-cxn agent applied-cxn (get-configuration (experiment agent) :entrenchment-incf))
                ;; 2. shift concept of applied-cxn to topic
                (shift-concept agent topic (meaning applied-cxn))
                ;; 3. find and punish meaning competitors
                (decide-competitors-hearer agent applied-cxn)
                (loop for other-cxn in (find-data agent 'meaning-competitors)
                      for similarity = (similar-concepts agent (meaning applied-cxn) (meaning other-cxn))
                      for delta = (* similarity (get-configuration (experiment agent) :entrenchment-li))
                      do (update-score-cxn agent other-cxn delta))
                ;; notify
                (notify event-align-cxn "Entrench and shift" applied-cxn previous-copy)))
             ((or (get-data agent 'interpreted-topic)
                  (eq (get-data agent 'interpreted-topic-reason) 'more-candidates))
              ;; CASE B: recognized but did not point to correct word
              (progn
                ;; 1. entrench applied-cxn negatively
                (update-score-cxn agent applied-cxn (get-configuration (experiment agent) :entrenchment-decf))
                ;; 2. shift concept of applied-cxn to topic
                (shift-concept agent topic (meaning applied-cxn))
                (notify event-align-cxn "Punish (due to failure) and shift" applied-cxn previous-copy)))
             ((eq (get-data agent 'interpreted-topic-reason) 'no-match)
              ;; CASE C: recognized but used concept is useless due to defects
              (progn
                (reset-adopt agent applied-cxn topic)
                (notify event-align-cxn "Concept is useless, shifting" applied-cxn previous-copy))))))))
