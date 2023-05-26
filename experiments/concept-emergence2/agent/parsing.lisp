(in-package :cle)

;; -----------
;; + Parsing +
;; -----------

;; events
(define-event event-parsing-end (agent cle-agent))

(defmethod parsing ((agent cle-agent))
  (let* ((utterance (utterance agent))
         (cxn (find-in-lexicon agent utterance)))
    ;; either nil or something
    (set-data agent 'applied-cxn cxn))
  ;; notify
  (notify event-parsing-end agent)
  ;; return nil or the applied cxn
  (find-data agent 'applied-cxn))
