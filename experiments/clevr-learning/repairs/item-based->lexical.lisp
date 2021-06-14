;;;; item-based->lexical.lisp

(in-package :clevr-learning)

;;  ITEM-BASED -> LEXICAL
;; -----------------------

;; This repair is applied when
;; a) an item-based cxn has applied
;; b) possibly one or more lexical cxns have applied,
;; c) the root contains a single form.
;; d) the number of slots in the item-based cxn is equal
;;    to the number of applied lex cxns + 1!
;; In this case, an additional lex cxn is required to parse the utterance.
;; In order to find out the meaning for that lex cxn, we
;; need to run the composer with the meaning of the applied
;; cxns as partial program.

(defmethod run-repair ((agent clevr-learning-learner)
                       (applied-cxns list) (node cip-node)
                       (utterance string)
                       (repair-mode (eql :item-based->lexical))
                       &key &allow-other-keys)
  (let ((cxn-inventory (grammar agent))
        (applied-lex-cxns (find-all 'lexical applied-cxns :key #'get-cxn-type))
        (applied-item-based-cxn (find 'item-based applied-cxns :key #'get-cxn-type))
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
                      (find-cxn-by-type-form-and-meaning 'lexical
                                                         form-predicates-lex-cxn
                                                         meaning-predicates-lex-cxn
                                                         cxn-inventory))
                     (cxn-name
                      (make-const
                       (gl::make-cxn-name
                        (third (first form-predicates-lex-cxn)) cxn-inventory)))
                     (unit-name
                      (second (first form-predicates-lex-cxn)))
                     (lex-class
                      (if existing-lex-cxn
                        (gl::lex-class-cxn existing-lex-cxn)
                        (intern (symbol-name (make-const unit-name)) :type-hierarchies)))
                     (args
                      (mapcar #'third meaning-predicates-lex-cxn))
                     (initial-cxn-score
                      (get-configuration agent :initial-cxn-score))
                     (current-interaction-nr
                      (interaction-number (current-interaction (experiment agent))))
                     (new-lex-cxn
                      (or existing-lex-cxn
                          (second
                           (multiple-value-list
                            (eval
                             `(def-fcg-cxn
                               ,cxn-name
                               ((,unit-name
                                 (syn-cat (gl::phrase-type lexical)
                                          (gl::lex-class ,lex-class))
                                 (args ,args))
                                <-
                                (,unit-name
                                 (HASH meaning ,meaning-predicates-lex-cxn)
                                 --
                                 (HASH form ,form-predicates-lex-cxn)))
                               :attributes (:score ,initial-cxn-score
                                            :cxn-type lexical
                                            :repair item->lex
                                            :added-at ,current-interaction-nr
                                            :last-used ,current-interaction-nr
                                            ;:string ,(form-predicates->hash-string form-predicates-lex-cxn)
                                            ;:meaning ,(meaning-predicates->hash-meaning meaning-predicates-lex-cxn)
                                            )
                               :cxn-inventory ,(copy-object cxn-inventory)
                               :cxn-set non-holophrase))))))
                     ;; make a list of all cxns, sort them
                     (lex-cxns
                      (gl::sort-cxns-by-form-string
                       (cons new-lex-cxn applied-lex-cxns)
                       (remove-punctuation utterance)))
                     (lex-classes-lex-cxns
                      (mapcar #'gl::lex-class-cxn lex-cxns))
                     (lex-classes-item-based-units
                      (gl::get-all-unit-lex-classes applied-item-based-cxn))
                     ;; assign all th links
                     (type-hierarchy
                      (get-type-hierarchy cxn-inventory))
                     (th-links
                      (when (and lex-classes-lex-cxns
                                 lex-classes-item-based-units
                                 (length= lex-classes-lex-cxns lex-classes-item-based-units))
                        (gl::create-new-th-links lex-classes-lex-cxns
                                                 lex-classes-item-based-units
                                                 type-hierarchy))))
                ;; return
                (when th-links
                  (add-composer-chunk agent (irl-program (chunk composer-solution)))
                  (values (list new-lex-cxn)
                          th-links)))
              ;; punish
              (loop with delta = (get-configuration agent :cxn-decf-score)
                    for cxn in applied-cxns
                    do (dec-cxn-score agent cxn :delta delta)
                    finally (notify cxns-punished applied-cxns))))
          ;; punish
          (loop with delta = (get-configuration agent :cxn-decf-score)
                for cxn in applied-cxns
                do (dec-cxn-score agent cxn :delta delta)
                finally (notify cxns-punished applied-cxns)))))))