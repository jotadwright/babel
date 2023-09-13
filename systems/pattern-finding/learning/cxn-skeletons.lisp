(in-package :pf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; holistic cxn skeletons ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun holistic-cxn-apply-first-skeleton (bare-cxn-name cxn-name top-cat
                                          form meaning form-args meaning-args
                                          initial-cxn-score holophrasep cxn-inventory)
  (second
   (multiple-value-list
    (eval
     `(def-fcg-cxn ,cxn-name
                   ((?holistic-unit
                     (form-args ,form-args)
                     (meaning-args ,meaning-args)
                     (category ,top-cat))
                    <-
                    (?holistic-unit
                     (HASH meaning ,meaning)
                     --
                     (HASH form ,form)))
                   :attributes (:label fcg::routine-apply-first
                                :cxn-type holistic
                                :bare-cxn-name ,bare-cxn-name
                                :is-holophrase ,holophrasep
                                :string ,(form-predicates->hash-string form (get-configuration cxn-inventory :form-representation-formalism))
                                :meaning ,(meaning-predicates->hash-meaning meaning (get-configuration cxn-inventory :meaning-representation-formalism)))
                   :score ,initial-cxn-score
                   :cxn-inventory ,cxn-inventory)))))

(defun holistic-cxn-apply-last-skeleton (bare-cxn-name cxn-name top-cat
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
                       (category ,top-cat)
                       --
                       (HASH form ,form)
                       (form-args ,form-args)
                       (category ,top-cat)))
                     :attributes (:label fcg::meta-apply-last
                                  :cxn-type holistic
                                  :bare-cxn-name ,bare-cxn-name
                                  :is-holophrase ,holophrasep
                                  :string ,(form-predicates->hash-string form (get-configuration cxn-inventory :form-representation-formalism))
                                  :meaning ,(meaning-predicates->hash-meaning meaning (get-configuration cxn-inventory :meaning-representation-formalism)))
                     :score ,initial-cxn-score
                     :cxn-inventory ,cxn-inventory)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; item-based cxn skeletons (apply-last) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun item-based-cxn-apply-last-skeleton (bare-cxn-name cxn-name top-cat slot-cats
                                           form meaning top-lvl-form-args top-lvl-meaning-args
                                           slot-form-args slot-meaning-args
                                           initial-cxn-score cxn-inventory)
  (let* ((contributing-slot-units
          (contributing-slot-units-apply-last-skeleton (length slot-cats)))
         (conditional-slot-units
          (conditional-slot-units-apply-last-skeleton slot-cats slot-form-args slot-meaning-args))
         (slot-unit-names
          (mapcar #'first contributing-slot-units))
         (meaning-representation (get-configuration cxn-inventory :meaning-representation-formalism))
         (form-representation (get-configuration cxn-inventory :form-representation-formalism)))
    (second
     (multiple-value-list
      (eval
       `(def-fcg-cxn ,cxn-name
                     ((?item-based-unit
                       (category ,top-cat)
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
                     :attributes (:label fcg::routine-apply-last
                                  :cxn-type item-based
                                  :bare-cxn-name ,bare-cxn-name
                                  :string ,(form-predicates->hash-string form form-representation)
                                  :meaning ,(meaning-predicates->hash-meaning meaning meaning-representation))
                     :score ,initial-cxn-score 
                     :cxn-inventory ,cxn-inventory))))))


(defun contributing-slot-units-apply-last-skeleton (number-of-units)
  (loop repeat number-of-units
        for unit-serial-number from 1
        for unit-name = (intern (upcase (format nil "?slot-unit-~a" unit-serial-number)))
        collect `(,unit-name
                  (footprints (used-as-slot-filler)))))


(defun conditional-slot-units-apply-last-skeleton (slot-cats slot-form-args slot-meaning-args)
  (loop for slot-cat in slot-cats
        for form-args in slot-form-args
        for meaning-args in slot-meaning-args
        for unit-serial-number from 1
        for unit-name = (intern (upcase (format nil "?slot-unit-~a" unit-serial-number)))
        collect `(,unit-name
                  (meaning-args ,meaning-args)
                  (category ,slot-cat)
                  (footprints (NOT used-as-slot-filler))
                  --
                  (footprints (NOT used-as-slot-filler))
                  (category ,slot-cat)
                  (form-args ,form-args))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; item-based cxn skeletons (apply-first) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun item-based-cxn-apply-first-skeleton (bare-cxn-name cxn-name top-cat slot-cats
                                           form meaning top-lvl-form-args top-lvl-meaning-args
                                           slot-form-args slot-meaning-args
                                           initial-cxn-score cxn-inventory)
  (let* ((contributing-slot-units
          (contributing-slot-units-apply-first-skeleton slot-cats slot-form-args slot-meaning-args))
         (slot-unit-names
          (mapcar #'first contributing-slot-units))
         (meaning-representation (get-configuration cxn-inventory :meaning-representation-formalism))
         (form-representation (get-configuration cxn-inventory :form-representation-formalism)))
    (second
     (multiple-value-list
      (eval
       `(def-fcg-cxn ,cxn-name
                     ((?item-based-unit
                       (category ,top-cat)
                       (meaning-args ,top-lvl-meaning-args)
                       (form-args ,top-lvl-form-args)
                       (subunits ,slot-unit-names))
                      ,@contributing-slot-units
                      <-
                      (?item-based-unit
                       (HASH meaning ,meaning)
                       --
                       (HASH form ,form)))
                     :attributes (:label fcg::meta-apply-first
                                  :cxn-type item-based
                                  :bare-cxn-name ,bare-cxn-name
                                  :string ,(form-predicates->hash-string form form-representation)
                                  :meaning ,(meaning-predicates->hash-meaning meaning meaning-representation))
                     :score ,initial-cxn-score 
                     :cxn-inventory ,cxn-inventory))))))


(defun contributing-slot-units-apply-first-skeleton (slot-cats slot-form-args slot-meaning-args)
  (loop for slot-cat in slot-cats
        for form-args in slot-form-args
        for meaning-args in slot-meaning-args
        for unit-serial-number from 1
        for unit-name = (intern (upcase (format nil "?slot-unit-~a" unit-serial-number)))
        collect `(,unit-name
                  (footprints (used-as-slot-filler))
                  (category ,slot-cat)
                  (meaning-args ,meaning-args)
                  (form-args ,form-args))))