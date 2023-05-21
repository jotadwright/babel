(in-package :cle)

;; -----------
;; + Parsing +
;; -----------

(define-event parsing-finished (agent cle-agent))

(defmethod parsing ((agent cle-agent))
  (let* ((utterance (utterance agent))
         (cxn (find-in-lexicon agent utterance)))
    ;; either nil or something
    (set-data agent 'applied-cxn cxn))
  (notify parsing-finished agent)
  (find-data agent 'applied-cxn))
