(in-package :cle)

;; -------------
;; + Alignment +
;; -------------

;; event
(define-event event-align-start (agent cle-agent))
(define-event event-align-cxn
  (reason string)
  (cxn cxn)
  (previous-copy cxn))

(defmethod align ((agent cle-agent))
  ;; event
  (notify event-align-start agent)
  (case (discourse-role agent)
    (speaker (speaker-alignment agent :times))
    (hearer (hearer-alignment agent :times))))

;; ----------------
;; + Alternatives +
;; ----------------

(defmethod speaker-alignment ((agent cle-agent) (mode (eql :times)))
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
          (update-score-cxn agent applied-cxn (get-configuration agent :entrenchment-incf))
          ;;  2. shift concept of applied-cxn to topic
          (shift-concept agent topic (meaning applied-cxn))
          ;;  3. punish competing similar cxns
          (loop for other-cxn in (find-data agent 'meaning-competitors)
                for similarity = (similar-concepts (meaning applied-cxn) (meaning other-cxn) mode)
                for delta = (* similarity (get-configuration agent :entrenchment-li))
                do (update-score-cxn agent other-cxn delta))
          ;; notify
          (notify event-align-cxn "Entrench and shift" applied-cxn previous-copy))
        ;; otherwise, entrench used cxn negatively
        (progn
          (update-score-cxn agent applied-cxn (get-configuration agent :entrenchment-decf))
          (notify event-align-cxn "Punish (due to failure)" applied-cxn previous-copy))))))

(defmethod hearer-alignment ((agent cle-agent) (mode (eql :times)))
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
       (if (communicated-successfully agent)
         ;; CASE A: recognized + communication was successful!
         (progn
           ;; 1. entrench applied-cxn
           (update-score-cxn agent applied-cxn (get-configuration agent :entrenchment-incf))
           ;; 2. shift concept of applied-cxn to topic
           (shift-concept agent topic (meaning applied-cxn))
           ;; 3. find and punish meaning competitors
           (decide-competitors-hearer agent applied-cxn mode)
           (loop for other-cxn in (find-data agent 'meaning-competitors)
                 for similarity = (similar-concepts (meaning applied-cxn) (meaning other-cxn) mode)
                 for delta = (* similarity (get-configuration agent :entrenchment-li))
                 do (update-score-cxn agent other-cxn delta))
           ;; notify
           (notify event-align-cxn "Entrench and shift" applied-cxn previous-copy))
         ;; CASE B: recognized but did not point to correct word
         (progn
           ;; 1. entrench applied-cxn negatively
           (update-score-cxn agent applied-cxn (get-configuration agent :entrenchment-decf))
           ;; 2. shift concept of applied-cxn to topic
           (shift-concept agent topic (meaning applied-cxn))
           (notify event-align-cxn "Punish (due to failure) and shift" applied-cxn previous-copy)))))))
