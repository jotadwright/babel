;;;; alignment.lisp

(in-package :grammar-learning)

(define-event alignment-started)
(define-event cxns-rewarded (cxns list))
(define-event cxns-punished (cxns list))

(defun alter-ego-cxn (original-cxn cxn-inventory)
  (when (attr-val original-cxn :bare-cxn-name)
    (loop for cxn in (constructions cxn-inventory)
          when (and (attr-val cxn :bare-cxn-name)
                    (eq (attr-val cxn :bare-cxn-name) (attr-val original-cxn :bare-cxn-name))
                    (not (eql (name cxn) (name original-cxn))))
          do (return cxn))))
  
(defmethod run-alignment ((agent clevr-learning-learner)
                          cipn (strategy (eql :lateral-inhibition)))
  (let ((applied-cxns (original-applied-constructions cipn))
        (utterance (utterance agent)))
    (notify alignment-started)
    ;; reward the applied cxns and punish competitors
    (loop with cxn-delta = (get-configuration agent :cxn-incf-score)
          for cxn in applied-cxns
          for alter-ego-cxn = (alter-ego-cxn cxn (grammar agent))
          do (inc-cxn-score cxn :delta cxn-delta)
            (when alter-ego-cxn
              (inc-cxn-score alter-ego-cxn :delta cxn-delta))
          finally (notify cxns-rewarded applied-cxns))
    (loop with cxn-delta = (get-configuration agent :cxn-decf-score)
          for competitor in (get-meaning-competitors agent applied-cxns utterance)
          for alter-ego-competitor = (alter-ego-cxn cxn (grammar agent))
          do (dec-cxn-score agent competitor :delta cxn-delta)
            (when alter-ego-competitor
              (dec-cxn-score agent alter-ego-competitor :delta cxn-delta))
          collect competitor into punished-cxns
          finally (notify cxns-punished punished-cxns))))
      
