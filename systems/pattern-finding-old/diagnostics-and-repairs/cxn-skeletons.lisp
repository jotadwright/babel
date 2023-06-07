(in-package :pattern-finding-old)

(defun holistic-cxn-apply-first-skeleton (bare-cxn-name cxn-name unit-name lex-class
                                          form meaning form-args meaning-args
                                          initial-cxn-score holophrasep cxn-inventory)
  (second
   (multiple-value-list
    (eval
     `(def-fcg-cxn ,cxn-name
                   ((,unit-name
                     (form-args ,form-args)
                     (meaning-args ,meaning-args)
                     (syn-cat (phrase-type holistic)
                              (lex-class ,lex-class)))
                    <-
                    (,unit-name
                     (HASH meaning ,meaning)
                     --
                     (HASH form ,form)))
                   :attributes (:label fcg::routine
                                :cxn-type holistic
                                :bare-cxn-name ,bare-cxn-name
                                :is-holophrase ,holophrasep
                                :string ,(form-predicates->hash-string form)
                                :meaning ,(meaning-predicates->hash-meaning meaning (get-configuration cxn-inventory :meaning-representation-formalism)))
                   :score ,initial-cxn-score
                   :cxn-inventory ,cxn-inventory)))))

(defun holistic-cxn-apply-last-skeleton (bare-cxn-name cxn-name unit-name lex-class
                                         form meaning form-args meaning-args
                                         initial-cxn-score holophrasep cxn-inventory)
    (second
     (multiple-value-list
      (eval
       `(def-fcg-cxn ,cxn-name
                     (<-
                      (,unit-name
                       (HASH meaning ,meaning)
                       (meaning-args ,meaning-args)
                       (syn-cat (phrase-type holistic)
                                (lex-class ,lex-class))
                       --
                       (HASH form ,form)
                       (form-args ,form-args)
                       (syn-cat (phrase-type holistic)
                                (lex-class ,lex-class))))
                     :attributes (:label fcg::meta-only
                                  :cxn-type holistic
                                  :bare-cxn-name ,bare-cxn-name
                                  :is-holophrase ,holophrasep
                                  :string ,(form-predicates->hash-string form)
                                  :meaning ,(meaning-predicates->hash-meaning meaning (get-configuration cxn-inventory :meaning-representation-formalism)))
                     :score ,initial-cxn-score
                     :cxn-inventory ,cxn-inventory)))))

(defun item-based-cxn-apply-last-skeleton (bare-cxn-name cxn-name lex-class-item-based lex-class-slot
                                           form meaning top-lvl-form-args top-lvl-meaning-args
                                           slot-form-args slot-meaning-args
                                           initial-cxn-score cxn-inventory)
  `(def-fcg-cxn ,cxn-name
                ((?item-based-unit
                  (syn-cat (phrase-type item-based)
                           (lex-class ,lex-class-item-based))
                  (meaning-args ,top-lvl-meaning-args)
                  (form-args ,top-lvl-form-args)
                  (subunits (?slot-unit)))
                 (?slot-unit
                  (footprints (used-as-slot-filler)))
                 <-
                 (?item-based-unit
                  (HASH meaning ,meaning)
                  --
                  (HASH form ,form))
                 (?slot-unit
                  (meaning-args ,slot-meaning-args)
                  (syn-cat (lex-class ,lex-class-slot))
                  (footprints (NOT used-as-slot-filler))
                  --
                  (footprints (NOT used-as-slot-filler))
                  (form-args ,slot-form-args)
                  (syn-cat (lex-class ,lex-class-slot))))
                :attributes (:label fcg::routine
                             :cxn-type item-based
                             :bare-cxn-name ,bare-cxn-name
                             :string ,(form-predicates->hash-string form)
                             :meaning ,(meaning-predicates->hash-meaning meaning :irl))
                :score ,initial-cxn-score 
                :cxn-inventory ,cxn-inventory))
  
(defun item-based-cxn-apply-first-skeleton (bare-cxn-name cxn-name lex-class-item-based lex-class-slot
                                            form meaning top-lvl-form-args top-lvl-meaning-args
                                            slot-form-args slot-meaning-args
                                            initial-cxn-score cxn-inventory)
  `(def-fcg-cxn ,cxn-name
                ((?item-based-unit
                  (syn-cat (phrase-type item-based)
                           (lex-class ,lex-class-item-based))
                  (meaning-args ,top-lvl-meaning-args)
                  (form-args ,top-lvl-form-args)
                  (subunits (?slot-unit)))
                 (?slot-unit
                  (footprints (used-as-slot-filler))
                  (syn-cat (lex-class ,lex-class-slot))
                  (meaning-args ,slot-meaning-args)
                  (form-args ,slot-form-args))
                 <-
                 (?item-based-unit
                  (HASH meaning ,meaning)
                  --
                  (HASH form ,form)))
                :attributes (:label fcg::meta-only
                             :cxn-type item-based
                             :bare-cxn-name ,bare-cxn-name
                             :string ,(form-predicates->hash-string form)
                             :meaning ,(meaning-predicates->hash-meaning meaning :irl))
                :score ,initial-cxn-score 
                :cxn-inventory ,cxn-inventory))
  