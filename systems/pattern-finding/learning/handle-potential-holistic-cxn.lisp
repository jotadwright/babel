(in-package :pf)

(defgeneric handle-potential-holistic-cxn (form meaning form-args meaning-args cxn-inventory)
  (:documentation "Apply all repairs recursively to the given form and meaning"))

(defmethod handle-potential-holistic-cxn (observation-form observation-meaning
                                          form-args meaning-args
                                          (cxn-inventory fcg-construction-set))
  (loop for repair in (repairs cxn-inventory)
        for repair-type = (type-of repair)
        for repair-result = (do-repair observation-form observation-meaning
                                       form-args meaning-args
                                       (processing-cxn-inventory cxn-inventory)
                                       nil repair-type)
        when repair-result return repair-result))