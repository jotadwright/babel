(in-package :pf-for-sql)

;;------------;;
;; ITEM-BASED ;;
;;------------;;

;; Repair class: demo-add-lexical-cxn inherits from repair
(defclass add-item-based (repair) 
  ((trigger :initform 'new-node)))

; Second repair : make an item based cxn. For the moment, they are of one slot only

(defun make-item-based-cxn (form-string meaning-predicates form-sequence-1 form-sequence-2)
  "based on meaning and form, makes an item-based cxn" ;; single-slot for now, to be continued
  (let* ((item-based-name (make-symbol (make-cxn-name form-string)))
        (cxn-cat (make-symbol (format nil "~a-cat" (make-cxn-name form-string))))
        (slot-cxn-cat (make-symbol (format nil "~a-slot-1-cat" (make-cxn-name form-string))))
        (item-based-cxn (make-instance 'fcg-construction
                                       :name item-based-name
                                       :contributing-part (list (make-instance 'contributing-unit
                                                                               :name '?item-based-unit
                                                                               :unit-structure `((category ,cxn-cat)
                                                                                                 (form-args (?left-1 ?right-2))
                                                                                                 (meaning-args ,(irl:get-target-var meaning-predicates))
                                                                                                 (subunits (?slot-1)))))
                                       :conditional-part (list (make-instance 'conditional-unit
                                                                              :name '?item-based-unit
                                                                              :formulation-lock `((HASH meaning ,meaning-predicates)) ;item-based-meaning
                                                                              :comprehension-lock `((HASH form ((sequence ,form-sequence-1 ?left-1 ?right-1)(sequence ,form-sequence-2 ?left-2 ?right-2)))))
                                                               (make-instance 'conditional-unit
                                                                              :name '?slot-1
                                                                              :formulation-lock `((meaning-args ,(irl:get-open-vars meaning-predicates)) (category ,slot-cxn-cat))
                                                                              :comprehension-lock `((form-args (?right-1 ?left-2)) (category ,slot-cxn-cat))))
                                       :attributes `((:cxn-cat . ,cxn-cat)
                                                     (:sequence . ,form-string)
                                                     (:meaning ,@meaning-predicates)
                                                     (:repair . holistic->item-based)
                                                     (:score . 0.5)
                                                     (:label . cxn))
                                       :description "A geo construction"
                                       :cxn-inventory *fcg-constructions*)))
    (add-cxn item-based-cxn *fcg-constructions*)
    (add-category cxn-cat *fcg-constructions*)
    (add-category slot-cxn-cat *fcg-constructions*)))

; (make-item-based-cxn "give-me-the-slot-1-in-usa-cxn-1" '((DOT ?COLUMN-1 ?ALIAS-0 ?COLUMN-2) (AS ?FILTER-0 ?TABLE-0 ?ALIAS-0) (FROM ?FILTER-1 ?FILTER-0) (SELECT ?RESULT-0 ?COLUMN-1 ?FILTER-1) (BIND CONCEPT ?ALIAS-0 CITYALIAS0) (BIND TABLE ?TABLE-0 CITY)) "give me the " " in usa")

; (clear *fcg-constructions*)

(defun tokenize-form-string (form-string)
  "A function to tokenize the form-string"
  (let ((form-tokens (split-string form-string " ")))
    form-tokens))

