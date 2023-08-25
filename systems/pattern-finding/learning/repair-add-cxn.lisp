(in-package :pf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repair nothing to holistic ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass add-cxn (add-cxns-and-categorial-links) 
  ((trigger :initform 'fcg::new-node)))


(defmethod repair ((repair add-cxn)
                   (problem non-gold-standard-meaning)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new construction."
  (when (and (initial-node-p node)
             (get-data problem :utterance))
    (let ((cxns-and-categorial-links
           (do-repair
            (get-data problem :utterance)
            (get-data problem :meaning)
            (make-blackboard)
            (construction-inventory node)
            node
            'add-cxn)))
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


(defmethod do-repair (observation-form observation-meaning (args blackboard) (cxn-inventory construction-inventory) node (repair-type (eql 'add-cxn)))
  (let* ((cxn-inventory (original-cxn-set cxn-inventory))
         (new-cxns-and-links-afr
          (cond (; when the args blackboard contains both top-lvl-args and slot-args
                 ; build an item-based cxn!
                 (and (field? args :slot-form-args)
                      (field? args :slot-meaning-args)
                      (field? args :top-lvl-form-args)
                      (field? args :top-lvl-meaning-args))
                 (make-item-based-cxn
                  observation-form
                  observation-meaning
                  (find-data args :top-lvl-form-args)
                  (find-data args :top-lvl-meaning-args)
                  (find-data args :slot-form-args)
                  (find-data args :slot-meaning-args)
                  cxn-inventory))
                (; all other cases (only top-lvl-args or no args at all)
                 ; build a holistic cxn
                 t
                 (let ((meaning-representation (get-configuration cxn-inventory :meaning-representation-formalism))
                       (form-representation (get-configuration cxn-inventory :form-representation-formalism)))
                   (make-holistic-cxn
                    observation-form
                    observation-meaning
                    (or (find-data args :top-lvl-form-args)
                        (holistic-form-top-args observation-form form-representation))
                    (or (find-data args :top-lvl-meaning-args)
                        (holistic-meaning-top-args observation-meaning meaning-representation))
                    cxn-inventory)))))
         (new-cxns-slot-categories
          (remove (afr-top-lvl-category new-cxns-and-links-afr)
                  (afr-categories-to-add new-cxns-and-links-afr)))
         (end-of-recursion-links
          (append
           (afr-categorial-links new-cxns-and-links-afr)
           ; + top lvl category of new cxn <-> slots that are filled by top lvl category of original cxn
           (loop for original-slot in (find-data args :original-slot-cats)
                 collect (cons (afr-top-lvl-category new-cxns-and-links-afr) original-slot))
           ; + fillers of slots of original cxn <-> slots of new cxn
           (loop for filler in (find-data args :slot-filler-cats)
                 for slot in new-cxns-slot-categories
                 collect (cons filler slot)))))
    ;; done!
    (apply-fix :form-constraints observation-form
               :cxns-to-apply (afr-cxns-to-apply new-cxns-and-links-afr)
               :cxns-to-consolidate (afr-cxns-to-consolidate new-cxns-and-links-afr)
               :categories-to-add (afr-categories-to-add new-cxns-and-links-afr)
               :categorial-links end-of-recursion-links
               :top-level-category (afr-top-lvl-category new-cxns-and-links-afr)
               :node node
               :repair-name repair-type)))



#|
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
|#