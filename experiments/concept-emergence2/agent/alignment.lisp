(in-package :cle)

;; -------------
;; + Alignment +
;; -------------

;; events
(define-event alignment-started (agent cle-agent))
(define-event adopt-cxn-started (form string))
(define-event scores-updated (cxn cxn) (rewarded-attrs list) (punished-attrs list))

(defmethod align ((agent cle-agent))
  ;; notify alignment
  (notify alignment-started agent)
  (case (discourse-role agent)
    (speaker (speaker-alignment agent))
    (hearer (hearer-alignment agent (get-configuration agent :hearer-alignment)))))

;; -----------------
;; + Align speaker +
;; -----------------
(defmethod speaker-alignment ((agent cle-agent))
  "Speaker alignment.

  If communication was successful,
       1. entrench and shift concept to topic
       2. punish competing duplicate concepts.
  Otherwise,
       1. entrench the concept negatively."
  (let ((topic (find-data agent 'topic))
        (applied-cxn (find-data agent 'applied-cxn)))
    (when (not (invented-or-adopted agent))
      ;; when the speaker has not invented
      (if (communicated-successfully agent)
        ;; if success,
        (progn
          ;;  1. entrench applied-cxn
          (update-score-concept agent applied-cxn)
          ;;  2. shift concept of applied-cxn to topic
          (shift-concept agent topic applied-cxn)
          ;;  3. punish competing duplicate cxns
          (punish-competitors agent topic applied-cxn))
        ;; otherwise, entrench used cxn negatively
        (update-score-concept agent applied-cxn)))))

;; ----------------
;; + Align hearer +
;; ----------------
(defgeneric hearer-alignment (agent mode)
  (:documentation "Hearer alignment."))

(defmethod hearer-alignment ((agent cle-agent) (mode (eql :shift-when-successful)))
  "Speaker alignment.

  If communication was successful,
       1. entrench and shift concept to topic
       2. punish competing duplicate concepts.
  Otherwise,
       1. entrench concept negatively, but do shift it!"
  (let ((topic (get-data agent 'topic))
        (concept (find-data agent 'applied-concept)))
    (when (utterance agent)
      ;; if speaker has uttered something
      (case concept
        ;; did not recognize the word
        ((nil) 
         (progn
           ;(notify adopt-concept-started (utterance agent))
           (adopt-concept agent topic (utterance agent))))
        ;; recognized the word
        (otherwise 
         (if (communicated-successfully agent)
           ;; recognized the word and successful usage!
           (progn
             ;; 1. entrench applied-cxn
             (update-score-cxn agent applied-cxn)
             ;; 2. shift concept of applied-cxn to topic
             (shift-concept agent topic applied-cxn)
             ;; 3. punish competitors
             (decide-competitors-hearer agent concept)
             (punish-competitors agent topic concept (get-configuration agent :punish-strategy)))
           ;; recognized but did not point to correct word
           ;; then, entrench cxn and shift concept to topic
           (progn
             ;; 1. entrench applied-cxn
             (update-score-cxn agent applied-cxn)
             ;; 2. shift concept of applied-cxn to topic
             (shift-concept agent topic applied-cxn))))))))
