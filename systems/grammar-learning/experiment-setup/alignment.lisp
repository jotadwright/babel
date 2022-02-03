;;;; alignment.lisp

(in-package :grammar-learning)

(define-event alignment-started)
(define-event cxns-rewarded (cxns list))
(define-event cxns-punished (cxns list))



(defmethod run-alignment ((agent clevr-learning-learner)
                          cipn (strategy (eql :lateral-inhibition)))
  (let ((applied-cxns (original-applied-constructions cipn))
        (utterance (utterance agent)))
    (notify alignment-started)
    ;; reward the applied cxns and punish competitors
    (loop with cxn-delta = (get-configuration agent :cxn-incf-score)
          for cxn in applied-cxns
          do (inc-cxn-score cxn :delta cxn-delta)
          finally (notify cxns-rewarded applied-cxns))
    (loop with cxn-delta = (get-configuration agent :cxn-decf-score)
          for competitor in (get-meaning-competitors agent applied-cxns utterance)
          do (dec-cxn-score agent competitor :delta cxn-delta)
          collect competitor into punished-cxns
          finally (notify cxns-punished punished-cxns))))
      
