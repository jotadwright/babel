(in-package :duckie-language-learning)

;; -----------------------------------------------------
;; + Repair:  HOLOPHRASE -> ITEM-BASED W/ SUBSTITUTION +
;; -----------------------------------------------------

(defclass holophrase->item-based--substitution (duckie-learning-repair)
  ((trigger :initform 'fcg::new-node)))

(defmethod repair ((repair holophrase->item-based--substitution)
                   (problem unknown-utterance-problem)
                   (node cip-node) &key
                   &allow-other-keys)
  "Repair the unknown utterance problem by making
   an item-based construction."
  (when (initial-node-p node)
    (let* ((constructions-and-categorial-links (create-item-based-cxn-substitution problem node)))
      (when constructions-and-categorial-links
        (make-instance 'fcg::cxn-fix
                       :repair repair
                       :problem problem
                       :restart-data constructions-and-categorial-links)))))

(defun create-item-based-cxn-substitution (problem node)
  "Create an item-based construction and two lexical constructions
   based on an existing holophrase construction with sufficient overlap."
  (let* (;; intention reading
         (agent (find-data problem :owner))
         ;(answer (find-data problem :answer))
         ;(meaning (compose-program agent answer))
         (meaning (find-data problem :intention))
         ;; pattern finding
         (cxn-inventory (original-cxn-set (construction-inventory node)))
         (utterance (cipn-utterance node)))
    (multiple-value-bind (non-overlapping-meaning-observation
                          non-overlapping-meaning-cxn
                          non-overlapping-form-observation
                          non-overlapping-form-cxn
                          overlapping-meaning-cxn
                          overlapping-form-cxn
                          cxn)
        (select-cxn-for-making-item-based-cxn cxn-inventory utterance meaning)
      (when cxn
        (let ((lex-cxn-1 (find-cxn-by-type-form-and-meaning 'lexical
                                                            non-overlapping-form-cxn
                                                            non-overlapping-meaning-cxn
                                                            cxn-inventory))
              (lex-cxn-2 (find-cxn-by-type-form-and-meaning 'lexical
                                                            non-overlapping-form-observation
                                                            non-overlapping-meaning-observation
                                                            cxn-inventory))
              (existing-item-based-cxn (find-cxn-by-type-form-and-meaning 'item-based
                                                                          overlapping-form-cxn
                                                                          overlapping-meaning-cxn
                                                                          cxn-inventory)))
          (unless (and lex-cxn-1 lex-cxn-2 existing-item-based-cxn)
            (let* ((cxn-name-item-based-cxn (make-cxn-name overlapping-form-cxn cxn-inventory :add-cxn-suffix nil))
                   ;; unit names
                   (unit-name-lex-cxn-1 (second (find 'string non-overlapping-form-cxn :key #'first)))
                   (unit-name-lex-cxn-2 (second (find 'string non-overlapping-form-observation :key #'first)))
                   ;; args and syn-cat
                   (lex-class-lex-cxn-1 (if lex-cxn-1
                                          (lex-class-cxn lex-cxn-1)
                                          (intern (symbol-name (make-const unit-name-lex-cxn-1)) :fcg)))
                   (lex-class-lex-cxn-2 (if lex-cxn-2
                                          (lex-class-cxn lex-cxn-2)
                                          (intern (symbol-name (make-const unit-name-lex-cxn-2)) :fcg)))
                   (lex-class-item-based-cxn (if existing-item-based-cxn
                                               (loop for unit in (contributing-part existing-item-based-cxn)
                                                     for lex-class = (lex-class-item-based unit)
                                                     when lex-class return lex-class)
                                               (intern (symbol-name (make-const cxn-name-item-based-cxn)) :fcg)))
                   ;; Type hierachy links
                   (categorial-link-1 (cons lex-class-lex-cxn-1 lex-class-item-based-cxn))
                   (categorial-link-2 (cons lex-class-lex-cxn-2 lex-class-item-based-cxn))
                   ;; Args
                   (args-lex-cxn-1 (third (first non-overlapping-meaning-cxn))) ;; third if bind
                   (args-lex-cxn-2 (third (first non-overlapping-meaning-observation))) ;; third if bind
                   ;; CXNs
                   (initial-cxn-score 0.5)
                   (new-lex-cxn-1 (or lex-cxn-1
                                      (second
                                       (multiple-value-list
                                        (eval
                                         `(def-fcg-cxn
                                           ,(make-const (make-cxn-name non-overlapping-form-cxn cxn-inventory))
                                           ((,unit-name-lex-cxn-1
                                             (args (,args-lex-cxn-1))
                                             (syn-cat (phrase-type lexical)
                                                      (fcg::lex-class ,lex-class-lex-cxn-1)))
                                            <-
                                            (,unit-name-lex-cxn-1
                                             (HASH meaning ,non-overlapping-meaning-cxn)
                                             --
                                             (HASH form ,non-overlapping-form-cxn)))
                                           :attributes (:cxn-type lexical
                                                        :repair holo->item
                                                        :score ,initial-cxn-score
                                                        :string ,(form-predicates->hash-string non-overlapping-form-cxn)
                                                        :meaning ,(meaning-predicates->hash-meaning non-overlapping-meaning-cxn))
                                           :cxn-inventory ,(copy-object cxn-inventory)
                                           :cxn-set non-holophrase))))))
                   (new-lex-cxn-2 (or lex-cxn-2 
                                      (second
                                       (multiple-value-list
                                        (eval
                                         `(def-fcg-cxn
                                           ,(make-const (make-cxn-name non-overlapping-form-observation cxn-inventory))
                                           ((,unit-name-lex-cxn-2
                                             (args (,args-lex-cxn-2))
                                             (syn-cat (phrase-type lexical)
                                                      (fcg::lex-class ,lex-class-lex-cxn-2)))
                                            <-
                                            (,unit-name-lex-cxn-2
                                             (HASH meaning ,non-overlapping-meaning-observation)
                                             --
                                             (HASH form ,non-overlapping-form-observation)))
                                           :attributes (:cxn-type lexical
                                                        :repair holo->item
                                                        :score ,initial-cxn-score
                                                        :string ,(form-predicates->hash-string non-overlapping-form-observation)
                                                        :meaning ,(meaning-predicates->hash-meaning non-overlapping-meaning-observation))
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
                                              (subunits (,unit-name-lex-cxn-1)))
                                             (,unit-name-lex-cxn-1
                                              (syn-cat (fcg::lex-class ,lex-class-item-based-cxn)))
                                             <-
                                             (?item-based-unit
                                              (HASH meaning ,overlapping-meaning-cxn)
                                              --
                                              (HASH form ,overlapping-form-cxn))
                                             (,unit-name-lex-cxn-1
                                              (args (,args-lex-cxn-1))
                                              --))
                                            :attributes (:cxn-type item-based
                                                         :repair holo->item
                                                         :score ,initial-cxn-score
                                                         :string ,(form-predicates->hash-string overlapping-form-cxn)
                                                         :meaning ,(meaning-predicates->hash-meaning overlapping-meaning-cxn))
                                            :cxn-inventory ,(copy-object cxn-inventory)
                                            :cxn-set non-holophrase)))))))
                  ;(add-composer-chunk agent overlapping-meaning-cxn)
                  ;(set-data interaction :applied-repair 'holophrase->item-based)
              ;; returns 
              ;; 1. existing cxns to apply
              ;; 2. new cxns to apply
              ;; 3. other new cxns
              ;; 4. categorial links
              (list nil
                    (list new-lex-cxn-2 item-based-cxn)
                    (list new-lex-cxn-1)
                    (list categorial-link-1 categorial-link-2)))))))))
