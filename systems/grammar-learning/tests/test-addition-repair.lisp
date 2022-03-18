(in-package :grammar-learning)

(deftest test-addition-repair-comprehension ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "The gray object is what shape?"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((get-context ?source-1)
                                             (filter ?target-2 ?target-1 ?color-2)
                                             (unique ?source-9 ?target-2)
                                             (bind shape-category ?shape-8 thing)
                                             (bind attribute-category ?attribute-2 shape)
                                             (filter ?target-1 ?source-1 ?shape-8)
                                             (bind color-category ?color-2 gray)
                                             (query ?target-7 ?source-9 ?attribute-2)))
    (test-repair-status 'holophrase->item-based+holistic--addition
                        (second (multiple-value-list
                                 (comprehend "The large gray object is what shape?"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((get-context ?source-1)
                                             (filter ?target-39552 ?target-2 ?size-4)
                                             (unique ?source-10 ?target-39552)
                                             (bind color-category ?color-2 gray)
                                             (filter ?target-1 ?source-1 ?shape-8)
                                             (bind attribute-category ?attribute-2 shape)
                                             (bind shape-category ?shape-8 thing)
                                             (filter ?target-2 ?target-1 ?color-2)
                                             (bind size-category ?size-4 large)
                                             (query ?target-8 ?source-10 ?attribute-2))))))))

(deftest test-double-addition-repair-comprehension ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "What is the shape of the large thing?"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((get-context ?source-1)
                                             (filter ?target-2 ?target-1 ?size-4)
                                             (unique ?target-object-1 ?target-2)
                                             (bind attribute-category ?attribute-2 shape)
                                             (bind shape-category ?shape-8 thing)
                                             (filter ?target-1 ?source-1 ?shape-8)
                                             (bind size-category ?size-4 large)
                                             (query ?target-4 ?target-object-1 ?attribute-2)))
    (test-repair-status 'holophrase->item-based+holistic--addition
                        (second (multiple-value-list
                                 (comprehend "What is the shape of the large gray shiny thing?"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((get-context ?source-1)
                                             (filter ?target-33324 ?target-33323 ?size-4)
                                             (unique ?target-object-1 ?target-33324)
                                             (bind color-category ?color-2 gray)
                                             (filter ?target-2 ?target-1 ?material-4)
                                             (bind material-category ?material-4 metal)
                                             (filter ?target-1 ?source-1 ?shape-8)
                                             (bind shape-category ?shape-8 thing)
                                             (bind attribute-category ?attribute-2 shape)
                                             (filter ?target-33323 ?target-2 ?color-2)
                                             (bind size-category ?size-4 large)
                                             (query ?target-4 ?target-object-1 ?attribute-2))))))))







(defun run-addition-tests ()
  (test-addition-repair-comprehension)
  (test-double-addition-repair-comprehension)
  )

;(run-addition-tests)

