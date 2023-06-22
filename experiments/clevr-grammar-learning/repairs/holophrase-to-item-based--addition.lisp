(in-package :cgl)

;;  HOLOPHRASE -> ITEM-BASED W/ ADDITION
;; --------------------------------------

(define-event holophrase->item-based-addition-repair-started)
(define-event holophrase->item-based-addition-new-cxn-and-th-links
  (new-cxns list) (th categorial-network) (new-links list))

(defclass holophrase->item-based--addition (clevr-learning-repair)
  ((trigger :initform 'fcg::new-node)))

;; This repair is applied when the utterance is completely unknown
;; or when all repairs using the partial utterance have failed
;; or when interpretation has failed.

(defmethod repair ((repair holophrase->item-based--addition)
                   (problem unknown-utterance-problem)
                   (node cip-node) &key &allow-other-keys)
  "Repair by making a new item-based construction and lexical cxn"
  (when (initial-node-p node)
    (let* ((reconstructed-intention
            (find-data problem :intention))
           (constructions-and-th-links
            (create-item-based-cxn-addition
             problem node reconstructed-intention)))
      (when constructions-and-th-links
        (make-instance 'fcg::cxn-fix
                       :repair repair
                       :problem problem
                       :restart-data constructions-and-th-links)))))

(defmethod repair ((repair holophrase->item-based--addition)
                   (problem failed-interpretation-problem)
                   (node cip-node) &key &allow-other-keys)
  "Repair by making a new item-based construction and lexical cxn"
  (let* ((reconstructed-intention
          (find-data problem :intention))
         (constructions-and-th-links
          (create-item-based-cxn-addition
           problem node reconstructed-intention)))
    (when constructions-and-th-links
      (make-instance 'fcg::cxn-fix
                     :repair repair
                     :problem problem
                     :restart-data constructions-and-th-links))))




(defun create-item-based-cxn-addition (problem node composer-solution)
  (let* ((agent (find-data problem :owner))
         (cxn-inventory (original-cxn-set (construction-inventory node)))
         (utterance (cipn-utterance node))
         (initial-transient-structure (initial-transient-structure node))
         (meaning
          (append (bind-statements composer-solution)
                  (irl-program (chunk composer-solution)))))
    (multiple-value-bind (subset-holophrase-cxn
                          superset-form
                          non-overlapping-form
                          non-overlapping-meaning)
        (find-subset-holophrase-cxn initial-transient-structure
                                        cxn-inventory meaning
                                        utterance)
      (when subset-holophrase-cxn
        (let* ((overlapping-form
                (set-difference superset-form non-overlapping-form
                                :test #'irl:unify-irl-programs))
               (overlapping-meaning
                (set-difference meaning non-overlapping-meaning
                                :test #'equal))
               (existing-lex-cxn
                (find-cxn-by-type-form-and-meaning 'lexical non-overlapping-form
                                                   non-overlapping-meaning cxn-inventory))
               (existing-item-based-cxn
                (find-cxn-by-type-form-and-meaning 'item-based overlapping-form
                                                   overlapping-meaning cxn-inventory)))
          (unless (and existing-lex-cxn existing-item-based-cxn)
            (let* ((lex-cxn-name
                    (make-const (make-cxn-name non-overlapping-form cxn-inventory)))
                   (cxn-name-item-based-cxn
                    (make-cxn-name overlapping-form cxn-inventory :add-cxn-suffix nil))
                   (unit-name-lex-cxn
                    (second (find 'string non-overlapping-form :key #'first)))
                   ;; lex-class
                   (lex-class-lex-cxn
                    (if existing-lex-cxn
                      (lex-class-cxn existing-lex-cxn)
                      (intern (symbol-name (make-const unit-name-lex-cxn)) :fcg)))
                   (lex-class-item-based-cxn
                    (if existing-item-based-cxn
                      (loop for unit in (contributing-part existing-item-based-cxn)
                            for lex-class = (lex-class-item-based unit)
                            when lex-class return lex-class)
                      (intern (symbol-name (make-const cxn-name-item-based-cxn)) :fcg)))
                   ;; type hierachy links
                   (th-link-1
                    (cons lex-class-lex-cxn lex-class-item-based-cxn))
                   ;; args: 
                   (args-lex-cxn
                    (third (first non-overlapping-meaning))) ;; third if bind
                   ;; cxns
                   (initial-cxn-score
                    (get-configuration agent :initial-cxn-score))
                   (interaction
                    (current-interaction (experiment agent)))
                   (interaction-nr
                    (interaction-number interaction))
                   (lex-cxn
                    (or existing-lex-cxn
                        (second
                         (multiple-value-list
                          (eval
                           `(def-fcg-cxn
                             ,lex-cxn-name
                             ((,unit-name-lex-cxn
                               (args (,args-lex-cxn))
                               (syn-cat (phrase-type lexical)
                                        (fcg::lex-class ,lex-class-lex-cxn)))
                              <-
                              (,unit-name-lex-cxn
                               (HASH meaning ,non-overlapping-meaning)
                               --
                               (HASH form ,non-overlapping-form)))
                             :attributes (:cxn-type lexical
                                          :repair holo->item
                                          :score ,initial-cxn-score
                                          :string ,(form-predicates->hash-string non-overlapping-form)
                                          :meaning ,(meaning-predicates->hash-meaning non-overlapping-meaning)
                                          :added-at ,interaction-nr)
                             :cxn-inventory ,(copy-object cxn-inventory)
                             :cxn-set non-holophrase))))))
                   (item-based-cxn
                    (or existing-item-based-cxn
                        (second
                         (multiple-value-list
                          (eval
                           `(def-fcg-cxn
                             ,(make-const (add-cxn-suffix cxn-name-item-based-cxn))
                             ((?item-based-unit
                               (syn-cat (phrase-type item-based))
                               (subunits (,unit-name-lex-cxn)))
                              (,unit-name-lex-cxn
                               (syn-cat (fcg::lex-class ,lex-class-item-based-cxn)))
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
                                          :meaning ,(meaning-predicates->hash-meaning overlapping-meaning)
                                          :added-at ,interaction-nr)
                             :cxn-inventory ,(copy-object cxn-inventory)
                             :cxn-set non-holophrase)))))))
              ;(add-composer-chunk agent overlapping-meaning)
              (set-data interaction :applied-repair 'holophrase->item-based)
              ;; returns 1. existing cxns to apply
              ;; 2. new cxns to apply
              ;; 3. other new cxns
              ;; 4. th links
              (list nil (list lex-cxn item-based-cxn) nil (list th-link-1)))))))))