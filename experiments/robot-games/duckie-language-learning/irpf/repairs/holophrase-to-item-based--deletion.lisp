(in-package :duckie-language-learning)

;; -------------------------------------------------
;; + Repair:  HOLOPHRASE -> ITEM-BASED W/ DELETION +
;; -------------------------------------------------

(defclass holophrase->item-based--deletion (duckie-learning-repair)
  ((trigger :initform 'fcg::new-node)))

(defmethod repair ((repair holophrase->item-based--deletion)
                   (problem unknown-utterance-problem)
                   (node cip-node) &key &allow-other-keys)
  "Repair by making a new item-based construction and lexical cxn"
  (when (initial-node-p node)
    (let* ((constructions-and-categorial-links (create-item-based-cxn-deletion problem node)))
      (when constructions-and-categorial-links
        (make-instance 'fcg::cxn-fix
                       :repair repair
                       :problem problem
                       :restart-data constructions-and-categorial-links)))))

(defun create-item-based-cxn-deletion (problem node)
  (let* (;; intention reading
         (agent (find-data problem :owner))
         (meaning (find-data problem :intention))
         ;; pattern finding
         (cxn-inventory (original-cxn-set (construction-inventory node)))
         (initial-transient-structure (initial-transient-structure node))
         (utterance (cipn-utterance node)))
    (multiple-value-bind (superset-holophrase-cxn
                          non-overlapping-form
                          non-overlapping-meaning)
        (find-superset-holophrase-cxn initial-transient-structure
                                      cxn-inventory
                                      meaning
                                      utterance)
      (when superset-holophrase-cxn
        (let* ((overlapping-form (set-difference (extract-form-predicates superset-holophrase-cxn)
                                                 non-overlapping-form
                                                 :test #'equal))
               (overlapping-meaning (set-difference (extract-meaning-predicates superset-holophrase-cxn)
                                                    non-overlapping-meaning
                                                    :test #'equal))
               (existing-lex-cxn (find-cxn-by-type-form-and-meaning 'lexical
                                                                    non-overlapping-form
                                                                    non-overlapping-meaning
                                                                    cxn-inventory))
               (existing-item-based-cxn (find-cxn-by-type-form-and-meaning 'item-based
                                                                           overlapping-form
                                                                           overlapping-meaning
                                                                           cxn-inventory)))
          (unless (and existing-lex-cxn existing-item-based-cxn)
            (let* ((lex-cxn-name (make-const (make-cxn-name non-overlapping-form
                                                            cxn-inventory)))
                   (cxn-name-item-based-cxn (make-cxn-name overlapping-form
                                                           cxn-inventory
                                                           :add-cxn-suffix nil))
                   (unit-name-lex-cxn (second (find 'string
                                                    non-overlapping-form
                                                    :key #'first)))
                   ;; lex-class
                   (lex-class-lex-cxn (if existing-lex-cxn
                                        (lex-class-cxn existing-lex-cxn)
                                        (intern (symbol-name (make-const unit-name-lex-cxn)) :fcg)))
                   (lex-class-item-based-cxn (if existing-item-based-cxn
                                               (loop for unit in (contributing-part existing-item-based-cxn)
                                                     for lex-class = (lex-class-item-based unit)
                                                     when lex-class return lex-class)
                                               (intern (symbol-name (make-const cxn-name-item-based-cxn)) :fcg)))
                   ;; type hierachy links
                   (categorial-link-1 (cons lex-class-lex-cxn lex-class-item-based-cxn))
                   ;; args: 
                   (args-lex-cxn (third (first non-overlapping-meaning))) ;; third if bind
                   (holophrase-cxn-name (make-const (make-cxn-name
                                                     (remove-spurious-spaces
                                                      (remove-punctuation utterance))
                                                     cxn-inventory)))
                   (form-constraints (form-constraints-with-variables utterance
                                                                      (get-configuration cxn-inventory :de-render-mode)))
                   ;; cxns
                   (initial-cxn-score 0.5)
                   (holophrase-cxn (second
                                    (multiple-value-list
                                     (eval
                                      `(def-fcg-cxn ,holophrase-cxn-name
                                                    ((?holophrase-unit
                                                      (syn-cat (phrase-type holophrase)))
                                                     <-
                                                     (?holophrase-unit
                                                      (HASH meaning ,meaning)
                                                      --
                                                      (HASH form ,form-constraints)))
                                                    :attributes (:cxn-type holophrase
                                                                 :repair holo->item
                                                                 :score ,initial-cxn-score
                                                                 :string ,(form-predicates->hash-string form-constraints)
                                                                 :meaning ,(meaning-predicates->hash-meaning meaning))
                                                    :cxn-inventory ,(copy-object cxn-inventory)
                                                    :cxn-set holophrase)))))
                   (lex-cxn (if existing-lex-cxn
                              existing-lex-cxn
                              (second
                               (multiple-value-list
                                (eval
                                 `(def-fcg-cxn
                                   ,lex-cxn-name
                                   ((,unit-name-lex-cxn
                                     (args (,args-lex-cxn))
                                     (syn-cat (phrase-type lexical)
                                              (lex-class ,lex-class-lex-cxn)))
                                    <-
                                    (,unit-name-lex-cxn
                                     (HASH meaning ,non-overlapping-meaning)
                                     --
                                     (HASH form ,non-overlapping-form)))
                                   :attributes (:cxn-type lexical
                                                :repair holo->item
                                                :score ,initial-cxn-score
                                                :string ,(form-predicates->hash-string non-overlapping-form)
                                                :meaning ,(meaning-predicates->hash-meaning non-overlapping-meaning))
                                   :cxn-inventory ,(copy-object cxn-inventory)
                                   :cxn-set non-holophrase))))))
                   (item-based-cxn (or existing-item-based-cxn
                                       (second
                                        (multiple-value-list
                                         (eval
                                          `(def-fcg-cxn
                                            ,(make-const (add-cxn-suffix cxn-name-item-based-cxn))
                                            ((?item-based-unit
                                              (syn-cat (phrase-type item-based))
                                              (subunits (,unit-name-lex-cxn)))
                                             (,unit-name-lex-cxn 
                                              (syn-cat (lex-class ,lex-class-item-based-cxn)))
                                             <-
                                             (?item-based-unit
                                              (HASH meaning ,overlapping-meaning)
                                              --
                                              (HASH form ,overlapping-form))
                                             (,unit-name-lex-cxn
                                              (args (,args-lex-cxn))
                                              --))
                                            :attributes (:cxn-type item-based
                                                         :repair holo->item
                                                         :score ,initial-cxn-score
                                                         :string ,(form-predicates->hash-string overlapping-form)
                                                         :meaning ,(meaning-predicates->hash-meaning overlapping-meaning))
                                            :cxn-inventory ,(copy-object cxn-inventory)
                                            :cxn-set non-holophrase)))))))
              ;(add-composer-chunk agent overlapping-meaning)
              ;(set-data interaction :applied-repair 'holophrase->item-based)
              ;; returns 1. existing cxns to apply
              ;; 2. new cxns to apply
              ;; 3. other new cxns
              ;; 4. categorial links
              (list nil (list holophrase-cxn)
                    (list lex-cxn item-based-cxn)
                    (list categorial-link-1)))))))))
