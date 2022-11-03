;;;; item-based->lexical.lisp

(in-package :intention-reading)

;;  ITEM-BASED -> LEXICAL
;; -----------------------

(define-event item-based->lexical-repair-started)
(define-event item-based->lexical-new-cxn-and-th-links
  (cxn construction) (th categorial-network) (new-links list))

(defclass item-based->lexical (clevr-learning-repair)
  ((trigger :initform 'fcg::new-node)))

;; This repair is applied when a partial utterance was diagnosed.

(defmethod repair ((repair item-based->lexical)
                   (problem partial-utterance-problem)
                   (node cip-node) &key
                   &allow-other-keys)
  (let ((lex-cxn-and-th-link
         (create-lexical-cxn problem node)))
    (when lex-cxn-and-th-link
      (make-instance 'fcg::cxn-fix
                     :repair repair
                     :problem problem
                     :restart-data lex-cxn-and-th-link))))

(defun create-lexical-cxn (problem node)
  ;(notify item-based->lexical-repair-started)
  (let* ((agent (find-data problem :owner))
         (cxn-inventory (original-cxn-set (construction-inventory node)))
         (utterance (cipn-utterance node))
         (applied-cxns (original-applied-constructions node))
         (applied-lex-cxns
          (find-all 'lexical applied-cxns :key #'get-cxn-type))
         (applied-item-based-cxn
          (find 'item-based applied-cxns :key #'get-cxn-type))
         (remaining-strings-in-root
          (get-strings-from-root node)))
    (when (and (not (null applied-item-based-cxn))
               (= (length remaining-strings-in-root) 1)
               (= (item-based-number-of-slots applied-item-based-cxn)
                  (1+ (length applied-lex-cxns))))
      (let* ((meaning-predicates-observed
              (mapcan #'extract-meaning-predicates applied-cxns))
             (composer-strategy
              (get-configuration agent :composer-strategy))
             (composer-solution
              (compose-program agent (topic agent) utterance composer-strategy
                               :partial-program meaning-predicates-observed)))
        (if composer-solution
          (let* ((meaning-predicates-gold
                  (append (bind-statements composer-solution)
                          (irl-program (chunk composer-solution))))
                 (meaning-predicates-lex-cxn
                  (set-difference meaning-predicates-gold meaning-predicates-observed
                                  :test #'unify-irl-programs)))
            ;; we don't know what the composer will return
            ;; so we make sure that the meaning for the new
            ;; lex cxn only contains a single element
            (if (length= meaning-predicates-lex-cxn 1)
              (let* ((form-predicates-lex-cxn
                      remaining-strings-in-root)
                     (existing-lex-cxn
                      (find-cxn-by-type-form-and-meaning 'lexical form-predicates-lex-cxn
                                                         meaning-predicates-lex-cxn cxn-inventory))
                     (cxn-name
                      (make-const
                       (make-cxn-name
                        (third (first form-predicates-lex-cxn)) cxn-inventory)))
                     (unit-name
                      (second (first form-predicates-lex-cxn)))
                     (lex-class
                      (if existing-lex-cxn
                        (lex-class-cxn existing-lex-cxn)
                        (intern (symbol-name (make-const unit-name)) :fcg)))
                     (args
                      (mapcar #'third meaning-predicates-lex-cxn))
                     (initial-cxn-score
                      (get-configuration agent :initial-cxn-score))
                     (interaction
                      (current-interaction (experiment agent)))
                     (interaction-nr
                      (interaction-number interaction))
                     (new-lex-cxn
                      (or existing-lex-cxn
                          (second
                           (multiple-value-list
                            (eval
                             `(def-fcg-cxn
                               ,cxn-name
                               ((,unit-name
                                 (syn-cat (phrase-type lexical)
                                          (fcg::lex-class ,lex-class))
                                 (args ,args))
                                <-
                                (,unit-name
                                 (HASH meaning ,meaning-predicates-lex-cxn)
                                 --
                                 (HASH form ,form-predicates-lex-cxn)))
                               :attributes (:score ,initial-cxn-score
                                            :cxn-type lexical
                                            :repair item->lex
                                            :string ,(form-predicates->hash-string form-predicates-lex-cxn)
                                            :meaning ,(meaning-predicates->hash-meaning meaning-predicates-lex-cxn)
                                            :added-at ,interaction-nr)
                               :cxn-inventory ,(copy-object cxn-inventory)
                               :cxn-set non-holophrase))))))
                     ;; make a list of all cxns, sort them
                     (lex-cxns
                      (sort-cxns-by-form-string
                       (cons new-lex-cxn applied-lex-cxns)
                       (remove-punctuation utterance)))
                     (lex-classes-lex-cxns
                      (mapcar #'lex-class-cxn lex-cxns))
                     (lex-classes-item-based-units
                      (get-all-unit-lex-classes applied-item-based-cxn))
                     ;; assign all th links
                     (type-hierarchy
                      (categorial-network cxn-inventory))
                     (th-links
                      (when (and lex-classes-lex-cxns
                                 lex-classes-item-based-units
                                 (length= lex-classes-lex-cxns lex-classes-item-based-units))
                        (create-new-th-links lex-classes-lex-cxns
                                             lex-classes-item-based-units
                                             type-hierarchy))))
                ;; return
                (if th-links
                  (progn
                    ;(add-composer-chunk agent (irl-program (chunk composer-solution)))
                    ;(notify item-based->lexical-new-cxn-and-th-links new-lex-cxn
                    ;        (categorial-network cxn-inventory) th-links)
                    (set-data interaction :applied-repair 'item-based->lexical)
                    ;; returns 1. existing cxns to apply
                    ;; 2. new cxns to apply
                    ;; 3. other new cxns
                    ;; 4. th links
                    (if existing-lex-cxn
                      (list (cons new-lex-cxn applied-cxns) nil nil th-links)
                      (list applied-cxns (list new-lex-cxn) nil th-links)))
                  (progn (push 'fcg::repair-failed (statuses node)) nil)))
              (progn (push 'fcg::repair-failed (statuses node)) nil)))
          (progn (push 'fcg::repair-failed (statuses node)) nil))))))