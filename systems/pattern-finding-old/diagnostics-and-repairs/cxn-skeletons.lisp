(in-package :pattern-finding-old)

(defun holistic-cxn-apply-first-skeleton (bare-cxn-name cxn-name lex-class
                                          form meaning form-args meaning-args
                                          initial-cxn-score holophrasep cxn-inventory)
  (second
   (multiple-value-list
    (eval
     `(def-fcg-cxn ,cxn-name
                   ((?holistic-unit
                     (form-args ,form-args)
                     (meaning-args ,meaning-args)
                     (syn-cat (phrase-type holistic)
                              (lex-class ,lex-class)))
                    <-
                    (?holistic-unit
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

(defun holistic-cxn-apply-last-skeleton (bare-cxn-name cxn-name lex-class
                                         form meaning form-args meaning-args
                                         initial-cxn-score holophrasep cxn-inventory)
    (second
     (multiple-value-list
      (eval
       `(def-fcg-cxn ,cxn-name
                     (<-
                      (?holistic-unit
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
  (second
   (multiple-value-list
    (eval
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
                                :meaning ,(meaning-predicates->hash-meaning meaning (get-configuration cxn-inventory :meaning-representation-formalism)))
                   :score ,initial-cxn-score 
                   :cxn-inventory ,cxn-inventory)))))
  
(defun item-based-cxn-apply-first-skeleton (bare-cxn-name cxn-name lex-class-item-based lex-class-slot
                                            form meaning top-lvl-form-args top-lvl-meaning-args
                                            slot-form-args slot-meaning-args
                                            initial-cxn-score cxn-inventory)
  (second
   (multiple-value-list
    (eval
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
                                :meaning ,(meaning-predicates->hash-meaning meaning (get-configuration cxn-inventory :meaning-representation-formalism)))
                   :score ,initial-cxn-score 
                   :cxn-inventory ,cxn-inventory)))))

(defun contributing-units-apply-last-skeleton (number-of-units)
  (loop repeat number-of-units
        for unit-serial-number from 1
        for unit-name = (intern (upcase (format nil "?slot-unit-~a" unit-serial-number)))
        collect `(,unit-name
                  (footprints (used-as-slot-filler)))))

(defun conditional-units-apply-last-skeleton (form-arg-groups meaning-arg-groups
                                              lex-classes-slot)
  (loop for lex-class in lex-classes-slot
        for form-arg-group in form-arg-groups
        for unit-serial-number from 1
        for unit-name = (intern (upcase (format nil "?slot-unit-~a" unit-serial-number)))
        for group-id = (first form-arg-group)
        for meaning-arg-group = (find group-id meaning-arg-groups :key #'first)
        collect `(,unit-name
                  (meaning-args ,(rest meaning-arg-group))
                  (syn-cat (lex-class ,lex-class))
                  (footprints (NOT used-as-slot-filler))
                  --
                  (footprints (NOT used-as-slot-filler))
                  (syn-cat (lex-class ,lex-class))
                  (form-args ,(rest form-arg-group)))))

(defun contributing-units-apply-first-skeleton (form-arg-groups meaning-arg-groups
                                                lex-classes-slot)
  (loop for lex-class in lex-classes-slot
        for form-arg-group in form-arg-groups
        for unit-serial-number from 1
        for unit-name = (intern (upcase (format nil "?slot-unit-~a" unit-serial-number)))
        for group-id = (first form-arg-group)
        for meaning-arg-group = (find group-id meaning-arg-groups :key #'first)
        collect `(,unit-name
                  (footprints (used-as-slot-filler))
                  (syn-cat (lex-class ,lex-class))
                  (meaning-args ,(rest meaning-arg-group))
                  (form-args ,(rest form-arg-group)))))

(defun item-based-cxn-apply-last-from-units-skeleton (bare-cxn-name cxn-name lex-class-item-based
                                                      form meaning top-lvl-form-args top-lvl-meaning-args
                                                      initial-cxn-score cxn-inventory
                                                      contributing-slot-units conditional-slot-units slot-unit-names)
  (second
   (multiple-value-list
    (eval
     `(def-fcg-cxn ,cxn-name
                   ((?item-based-unit
                     (syn-cat (phrase-type item-based)
                              (lex-class ,lex-class-item-based))
                     (meaning-args ,top-lvl-meaning-args)
                     (form-args ,top-lvl-form-args)
                     (subunits ,slot-unit-names))
                    ,@contributing-slot-units
                    <-
                    (?item-based-unit
                     (HASH meaning ,meaning)
                     --
                     (HASH form ,form))
                    ,@conditional-slot-units)
                   :attributes (:label fcg::routine
                                :cxn-type item-based
                                :bare-cxn-name ,bare-cxn-name
                                :string ,(form-predicates->hash-string form)
                                :meaning ,(meaning-predicates->hash-meaning meaning (get-configuration cxn-inventory :meaning-representation-formalism)))
                   :score ,initial-cxn-score 
                   :cxn-inventory ,cxn-inventory)))))

(defun item-based-cxn-apply-first-from-units-skeleton (bare-cxn-name cxn-name lex-class-item-based
                                                       form meaning top-lvl-form-args top-lvl-meaning-args
                                                       initial-cxn-score cxn-inventory
                                                       contributing-slot-units slot-unit-names)
  (second
   (multiple-value-list
    (eval
     `(def-fcg-cxn ,cxn-name
                   ((?item-based-unit
                     (syn-cat (phrase-type item-based)
                              (lex-class ,lex-class-item-based))
                     (meaning-args ,top-lvl-meaning-args)
                     (form-args ,top-lvl-form-args)
                     (subunits ,slot-unit-names))
                    ,@contributing-slot-units
                    <-
                    (?item-based-unit
                     (HASH meaning ,meaning)
                     --
                     (HASH form ,form)))
                   :attributes (:label fcg::meta-only
                                :cxn-type item-based
                                :bare-cxn-name ,bare-cxn-name
                                :string ,(form-predicates->hash-string form)
                                :meaning ,(meaning-predicates->hash-meaning meaning (get-configuration cxn-inventory :meaning-representation-formalism)))
                   :score ,initial-cxn-score 
                   :cxn-inventory ,cxn-inventory)))))
        
        
    