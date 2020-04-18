(in-package :grammar-learning)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repair Add holophrase construction ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass add-holophrase-cxn (repair) 
  ((trigger :initform 'fcg::new-node)))
  
(defmethod repair ((repair add-holophrase-cxn)
                   (problem non-gold-standard-meaning)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new holophrase construction."
  (make-instance 'fcg::cxn-fix
                     :repair repair
                     :problem problem
                     :restart-data (create-holophrase-cxn problem node)))
  
(defmethod repair ((repair add-holophrase-cxn)
                   (problem non-gold-standard-utterance)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new holophrase construction."
  (make-instance 'fcg::cxn-fix
                     :repair repair
                     :problem problem
                     :restart-data (create-holophrase-cxn problem node)))

(defun create-holophrase-cxn (problem node)
  "Creates a holophrase-cxn."
  (let* ((processing-cxn-inventory (construction-inventory node))
         (cxn-inventory (original-cxn-set processing-cxn-inventory))
         (utterance (random-elt (get-data problem :utterances)))
         (form-constraints (form-constraints-with-variables utterance (get-configuration cxn-inventory :de-render-mode)))
         (meaning-predicates (meaning-predicates-with-variables (random-elt (get-data problem :meanings))))
         (cxn-name (make-cxn-name utterance cxn-inventory))
         (holophrase-cxn (second (multiple-value-list  (eval
                                                        `(def-fcg-cxn ,cxn-name
                                                                      ((?holophrase-unit
                                                                        (syn-cat (phrase-type holophrase)))
                                                                       <-
                                                                       (?holophrase-unit
                                                                        (HASH meaning ,meaning-predicates)
                                                                        --
                                                                        (HASH form ,form-constraints)))
                                                                      :cxn-inventory ,(copy-object cxn-inventory)))))))
    holophrase-cxn))
