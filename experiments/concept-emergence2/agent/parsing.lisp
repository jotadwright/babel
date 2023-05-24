(in-package :cle)

;; -----------
;; + Parsing +
;; -----------

(defmethod parsing ((agent cle-agent))
  (let* ((utterance (utterance agent))
         (cxn (find-in-lexicon agent utterance)))
    ;; either nil or something
    (set-data agent 'applied-cxn cxn))
  (find-data agent 'applied-cxn))
