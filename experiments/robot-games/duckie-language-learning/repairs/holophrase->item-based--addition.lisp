(in-package :duckie-language-learning)

;; ------------------------------------------------
;; + Repair: HOLOPHRASE -> ITEM-BASED W/ ADDITION +
;; ------------------------------------------------

(defclass holophrase->item-based--addition (duckie-learning-repair)
  ((trigger :initform 'fcg::new-node)))

(defmethod repair ((repair holophrase->item-based--addition)
                   (problem unknown-utterance-problem)
                   (node cip-node) &key &allow-other-keys)
  "Repair by making a new item-based construction and lexical cxn"
  (when (gl::initial-node-p node)
    (let* ((reconstructed-intention (find-data problem :intention))
           (constructions-and-categorial-links (create-item-based-cxn-addition problem node     reconstructed-intention)))
      (when constructions-and-categorial-links
        (make-instance 'fcg::cxn-fix
                       :repair repair
                       :problem problem
                       :restart-data constructions-and-categorial-links)))))

(defun create-item-based-cxn-addition (problem node intention)
  (let* ((agent (find-data problem :owner))
         (cxn-inventory (original-cxn-set (construction-inventory node)))
         (utterance (cipn-utterance node))
         (initial-transient-structure (gl::initial-transient-structure node))
         (meaning intention))
    (multiple-value-bind (subset-holophrase-cxn
                          superset-form
                          non-overlapping-form
                          non-overlapping-meaning)
        (gl::find-subset-holophrase-cxn initial-transient-structure
                                        cxn-inventory meaning
                                        utterance)
      (when subset-holophrase-cxn
        (let* ((overlapping-form (set-difference superset-form
                                                 non-overlapping-form
                                                 :test #'irl:unify-irl-programs))
               (overlapping-meaning (set-difference meaning
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
            (let* ((lex-cxn-name (make-const (gl::make-cxn-name non-overlapping-form cxn-inventory)))
                   (cxn-name-item-based-cxn (gl::make-cxn-name overlapping-form cxn-inventory :add-cxn-suffix nil))
                   (unit-name-lex-cxn (second (find 'string non-overlapping-form :key #'first)))
                   ;; lex-class
                   (lex-class-lex-cxn (if existing-lex-cxn
                                        (gl::lex-class-cxn existing-lex-cxn)
                                        (intern (symbol-name (make-const unit-name-lex-cxn)) :fcg)))
                   (lex-class-item-based-cxn (if existing-item-based-cxn
                                               (loop for unit in (contributing-part existing-item-based-cxn)
                                                     for lex-class = (gl::lex-class-item-based unit)
                                                     when lex-class return lex-class)
                                               (intern (symbol-name (make-const cxn-name-item-based-cxn)) :fcg)))
                   ;; type hierachy links
                   (categorial-link-1 (cons lex-class-lex-cxn lex-class-item-based-cxn))
                   ;; args: 
                   (args-lex-cxn (third (first non-overlapping-meaning))) ;; third if bind
                   ;; cxns
                   (initial-cxn-score 0.5)
                   (lex-cxn (or existing-lex-cxn
                                (second
                                 (multiple-value-list
                                  (eval
                                   `(def-fcg-cxn
                                     ,lex-cxn-name
                                     ((,unit-name-lex-cxn
                                       (args (,args-lex-cxn))
                                       (syn-cat (gl::phrase-type lexical)
                                                (gl::lex-class ,lex-class-lex-cxn)))
                                      <-
                                      (,unit-name-lex-cxn
                                       (HASH meaning ,non-overlapping-meaning)
                                       --
                                       (HASH form ,non-overlapping-form)))
                                     :attributes (:cxn-type gl::lexical
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
                                            ,(make-const (gl::add-cxn-suffix cxn-name-item-based-cxn))
                                            ((?item-based-unit
                                              (syn-cat (gl::phrase-type item-based))
                                              (subunits (,unit-name-lex-cxn)))
                                             (,unit-name-lex-cxn
                                              (syn-cat (gl::lex-class ,lex-class-item-based-cxn)))
                                             <-
                                             (?item-based-unit
                                              (HASH meaning ,overlapping-meaning)
                                              --
                                              (HASH form ,overlapping-form))
                                             (,unit-name-lex-cxn
                                              (args (,args-lex-cxn))
                                              --))
                                            :attributes (:cxn-type gl::item-based
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
              (list nil (list lex-cxn item-based-cxn) nil (list categorial-link-1)))))))))
