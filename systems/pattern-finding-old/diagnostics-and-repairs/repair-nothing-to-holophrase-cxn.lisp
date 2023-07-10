(in-package :pattern-finding-old)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repair nothing to holistic ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass nothing->holistic (add-cxns-and-categorial-links) 
  ((trigger :initform 'fcg::new-node)))


(defmethod repair ((repair nothing->holistic)
                   (problem non-gold-standard-meaning)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new holophrase construction."
  (when (and (initial-node-p node)
             (get-data problem :utterance))
    (let ((cxns-and-categorial-links
           (do-repair
            (get-data problem :utterance)
            (get-data problem :meaning)
            (make-blackboard)
            (construction-inventory node)
            node
            'nothing->holistic)))
      (make-instance 'fcg::cxn-fix
                     :repair repair
                     :problem problem
                     :restart-data cxns-and-categorial-links))))

(defmethod do-repair (observation-form observation-meaning (args blackboard) (cxn-inventory construction-inventory) node (repair-type (eql 'nothing->holistic)))
  (let* ((cxn-inventory (original-cxn-set cxn-inventory))
         ;; make the cxn name
         (cxn-name
          (make-cxn-name observation-form cxn-inventory
                         :holistic-suffix t
                         :numeric-suffix t))
         (cxn-name-apply-last
          (intern (upcase (format nil "~a-apply-last" cxn-name))))
         (cxn-name-apply-first
          (intern (upcase (format nil "~a-apply-first" cxn-name))))
         ;; top lvl args
         (cxn-form-args (find-data args :top-lvl-form-args))
         (cxn-meaning-args (find-data args :top-lvl-meaning-args))
         ;; find an identical existing holistic cxn
         (existing-routine-holistic-cxn
          (find-identical-holistic-cxn observation-form observation-meaning cxn-form-args cxn-meaning-args cxn-inventory))
         (existing-meta-holistic-cxn
          (when existing-routine-holistic-cxn
            (alter-ego-cxn existing-routine-holistic-cxn cxn-inventory)))
         ;; lex class
         (lex-class
          (if existing-routine-holistic-cxn
            (extract-lex-class-holistic-cxn existing-routine-holistic-cxn)
            (make-lex-class cxn-name :trim-cxn-suffix t :numeric-suffix t)))
         ;; cxn inventory
         (cxn-inventory-copy
          (unless existing-routine-holistic-cxn
            (copy-object cxn-inventory)))
         ;; new cxns
         (holistic-cxn-apply-first
          (or existing-routine-holistic-cxn
              (holistic-cxn-apply-first-skeleton cxn-name cxn-name-apply-first lex-class
                                                 observation-form observation-meaning
                                                 cxn-form-args cxn-meaning-args
                                                 (get-configuration cxn-inventory :initial-cxn-score)
                                                 (and node (get-configuration cxn-inventory :mark-holophrases))
                                                 cxn-inventory-copy)))
         (holistic-cxn-apply-last
          (or existing-meta-holistic-cxn
              (holistic-cxn-apply-last-skeleton cxn-name cxn-name-apply-last lex-class
                                                observation-form observation-meaning
                                                cxn-form-args cxn-meaning-args
                                                (get-configuration cxn-inventory :initial-cxn-score)
                                                (and node (get-configuration cxn-inventory :mark-holophrases))
                                                cxn-inventory-copy)))
         ;; build results
         (cxns-to-apply (list holistic-cxn-apply-first))
         (cxns-to-consolidate (list holistic-cxn-apply-last))
         (cats-to-add (list lex-class)))

    (apply-fix
     ;; form
     observation-form
     ;; cxns to apply
     cxns-to-apply
     ;; categorial links
     nil
     ;; original cxns to consolidate
     cxns-to-consolidate
     ;; categories to add
     cats-to-add
     ;; top level category
     lex-class
     ;; gold standard consulted p
     t
     ;; node
     node
     ;; repair name
     repair-type)))