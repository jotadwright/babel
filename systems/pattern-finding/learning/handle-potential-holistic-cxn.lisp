(in-package :pf)

(defgeneric handle-potential-holistic-cxn (form meaning args cxn-inventory)
  (:documentation "Apply all repairs recursively to the given form and meaning"))

(defmethod handle-potential-holistic-cxn (observation-form observation-meaning
                                          (args blackboard)
                                          (cxn-inventory fcg-construction-set))
  (loop with recursive-repairs
          = (if (get-configuration cxn-inventory :repair-recursively)
              '(add-categorial-links anti-unify-cxns add-cxn)
              '(add-cxn))
        for repair in recursive-repairs
        for repair-result = (do-repair observation-form observation-meaning
                                       args (processing-cxn-inventory cxn-inventory)
                                       nil repair)
        when repair-result return repair-result))