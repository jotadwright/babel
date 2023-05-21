(in-package :cle)

;; -------------
;; + Alignment +
;; -------------

;; events
(define-event alignment-started (agent cle-agent))
(define-event adoption-started (form string))
(define-event entrenchment-update (cxn cxn) (rewarded-attrs list) (punished-attrs list))

(defmethod align ((agent cle-agent))
  (case (discourse-role agent)
    (speaker (speaker-alignment agent))
    (hearer (hearer-alignment agent))))

;; -----------------
;; + Align speaker +
;; -----------------
(defmethod speaker-alignment ((agent cle-agent))
  "Speaker alignment."
  (let ((topic (find-data agent 'topic))
        (applied-cxn (find-data agent 'applied-cxn)))
    (when (not (invented-or-adopted agent))
      ;; when the speaker has not invented
      (if (communicated-successfully agent)
        ;; if success,
        (progn
          ;;  1. entrench applied-cxn
          (update-score-cxn agent applied-cxn)
          ;;  2. shift concept of applied-cxn to topic
          (shift-concept agent topic (meaning applied-cxn))
          ;;  3. punish competing similar cxns
          (loop for other-cxn in (find-data agent 'meaning-competitors)
                 do (update-score-cxn agent other-cxn (get-configuration agent :entrenchment-li))))
        ;; otherwise, entrench used cxn negatively
        (update-score-cxn agent applied-cxn)))))

;; ----------------
;; + Align hearer +
;; ----------------
(defmethod hearer-alignment ((agent cle-agent))
  "Hearer alignment."
  (let ((topic (get-data agent 'topic))
        (applied-cxn (find-data agent 'applied-cxn)))
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
           (decide-competitors-hearer agent applied-cxn)
           (loop for other-cxn in (find-data agent 'meaning-competitors)
                 do (update-score-cxn agent other-cxn (get-configuration agent :entrenchment-li)))
           ;; CASE B: recognized but did not point to correct word
           (progn
             ;; 1. entrench applied-cxn
             (update-score-cxn agent applied-cxn)
             ;; 2. shift concept of applied-cxn to topic
             (shift-concept agent topic (meaning applied-cxn)))))))))
