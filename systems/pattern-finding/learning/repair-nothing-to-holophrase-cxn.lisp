(in-package :pf)

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

(defgeneric holistic-form-top-args (form mode)
  (:documentation "Extract the top args from the form"))

(defmethod holistic-form-top-args (form mode)
  (get-boundaries form))

(defgeneric holistic-meaning-top-args (meaning mode)
  (:documentation "Extract the top args from the meaning"))

(defmethod holistic-meaning-top-args (meaning mode)
  (let ((target-var (get-target-var meaning)))
    (if target-var
      (list target-var)
      (let* ((all-vars (find-all-anywhere-if #'variable-p meaning))
             (singleton-vars (find-all-if #'(lambda (var) (= (count-anywhere var meaning) 1)) all-vars)))
        singleton-vars))))

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
         (meaning-representation (get-configuration cxn-inventory :meaning-representation-formalism))
         (form-representation (get-configuration cxn-inventory :form-representation-formalism))
         (cxn-form-args (or (find-data args :top-lvl-form-args)
                            (holistic-form-top-args observation-form form-representation)))
         (cxn-meaning-args (or (find-data args :top-lvl-meaning-args)
                               (holistic-meaning-top-args observation-meaning meaning-representation)))
         ;; find an identical existing holistic cxn
         (existing-routine-holistic-cxn
          (find-identical-holistic-cxn observation-form observation-meaning cxn-form-args cxn-meaning-args cxn-inventory))
         (existing-meta-holistic-cxn
          (when existing-routine-holistic-cxn
            (alter-ego-cxn existing-routine-holistic-cxn cxn-inventory)))
         ;; grammatical categories
         (category
          (if existing-routine-holistic-cxn
            (extract-top-category-holistic-cxn existing-routine-holistic-cxn)
            (make-grammatical-category cxn-name :trim-cxn-suffix t :numeric-suffix t)))
         ;; cxn inventory
         (cxn-inventory-copy
          (unless existing-routine-holistic-cxn
            (copy-object cxn-inventory)))
         ;; new cxns
         (holistic-cxn-apply-first
          (or existing-routine-holistic-cxn
              (holistic-cxn-apply-first-skeleton cxn-name cxn-name-apply-first category
                                                 observation-form observation-meaning
                                                 cxn-form-args cxn-meaning-args
                                                 (get-configuration cxn-inventory :initial-cxn-score)
                                                 (and node (get-configuration cxn-inventory :mark-holophrases))
                                                 cxn-inventory-copy)))
         (holistic-cxn-apply-last
          (or existing-meta-holistic-cxn
              (holistic-cxn-apply-last-skeleton cxn-name cxn-name-apply-last category
                                                observation-form observation-meaning
                                                cxn-form-args cxn-meaning-args
                                                (get-configuration cxn-inventory :initial-cxn-score)
                                                (and node (get-configuration cxn-inventory :mark-holophrases))
                                                cxn-inventory-copy)))
         ;; build results
         (cxns-to-apply (list holistic-cxn-apply-first))
         (cxns-to-consolidate (list holistic-cxn-apply-last))
         (cats-to-add (list category)))

    (apply-fix
     ;; form
     observation-form
     ;; cxns to apply
     cxns-to-apply
     ;; cxns to consolidate
     cxns-to-consolidate
     ;; categories to add
     cats-to-add
     ;; categorial links
     nil
     ;; top level category
     category
     ;; gold standard consulted p
     t
     ;; node
     node
     ;; repair name
     repair-type)))