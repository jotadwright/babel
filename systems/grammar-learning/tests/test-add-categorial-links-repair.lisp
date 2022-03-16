(in-package :grammar-learning)

(deftest test-categorial-links-repair-comprehension ()
         (let* ((experiment (set-up-cxn-inventory-and-repairs))
                (cxn-inventory (grammar (first (agents experiment)))))
           (comprehend "What is the size of the red cube?"
                       :cxn-inventory cxn-inventory
                       :gold-standard-meaning '((get-context ?source-1)
                                                (filter ?target-2 ?target-1 ?color-4)
                                                (unique ?target-object-1 ?target-2)
                                                (bind shape-category ?shape-2 cube)
                                                (bind attribute-category ?attribute-6 size)
                                                (filter ?target-1 ?source-1 ?shape-2)
                                                (bind color-category ?color-4 red)
                                                (query ?target-4 ?target-object-1 ?attribute-6)))
           (comprehend "What is the size of the blue cube?"
                       :cxn-inventory cxn-inventory
                       :gold-standard-meaning '((get-context ?source-1)
                                                (filter ?target-2 ?target-1 ?color-6)
                                                (unique ?target-object-1 ?target-2)
                                                (bind attribute-category ?attribute-6 size)
                                                (bind shape-category ?shape-2 cube)
                                                (filter ?target-1 ?source-1 ?shape-2)
                                                (bind color-category ?color-6 blue)
                                                (query ?target-4 ?target-object-1 ?attribute-6)))
           (comprehend "What is the size of the yellow cube?"
                       :cxn-inventory cxn-inventory
                       :gold-standard-meaning '((get-context ?source-1)
                                                (filter ?target-2 ?target-1 ?color-16)
                                                (unique ?target-object-1 ?target-2)
                                                (bind shape-category ?shape-2 cube)
                                                (bind attribute-category ?attribute-6 size)
                                                (filter ?target-1 ?source-1 ?shape-2)
                                                (bind color-category ?color-16 yellow)
                                                (query ?target-4 ?target-object-1 ?attribute-6)))
           (comprehend "What is the size of the green sphere?"
                       :cxn-inventory cxn-inventory
                       :gold-standard-meaning '((get-context ?source-1)
                                                (filter ?target-2 ?target-1 ?color-8)
                                                (unique ?target-object-1 ?target-2)
                                                (bind shape-category ?shape-4 sphere)
                                                (bind attribute-category ?attribute-6 size)
                                                (filter ?target-1 ?source-1 ?shape-4)
                                                (bind color-category ?color-8 green)
                                                (query ?target-4 ?target-object-1 ?attribute-6)))
    
           (comprehend "What is the size of the purple sphere?"
                       :cxn-inventory cxn-inventory
                       :gold-standard-meaning '((get-context ?source-1)
                                                (filter ?target-2 ?target-1 ?color-8)
                                                (unique ?target-object-1 ?target-2)
                                                (bind shape-category ?shape-4 sphere)
                                                (bind attribute-category ?attribute-6 size)
                                                (filter ?target-1 ?source-1 ?shape-4)
                                                (bind color-category ?color-8 purple)
                                                (query ?target-4 ?target-object-1 ?attribute-6)))
           (comprehend "What is the size of the purple cube?"
                       :cxn-inventory cxn-inventory
                       :gold-standard-meaning '((get-context ?source-1)
                                                (bind attribute-category ?attribute-6 size)
                                                (bind color-category ?color-12 purple)
                                                (filter ?target-1 ?source-1 ?shape-2)
                                                (bind shape-category ?shape-2 cube)
                                                (unique ?target-object-1 ?target-2)
                                                (filter ?target-2 ?target-1 ?color-12)
                                                (query ?target-4 ?target-object-1 ?attribute-6)))
           (test-repair-status 'add-categorial-links
                               (second (multiple-value-list
                                        (comprehend "What is the size of the yellow sphere?"
                                                    :cxn-inventory cxn-inventory
                                                    :gold-standard-meaning '((get-context ?source-1)
                                                                             (filter ?target-2 ?target-1 ?color-8)
                                                                             (unique ?target-object-1 ?target-2)
                                                                             (bind shape-category ?shape-4 sphere)
                                                                             (bind attribute-category ?attribute-6 size)
                                                                             (filter ?target-1 ?source-1 ?shape-4)
                                                                             (bind color-category ?color-8 yellow)
                                                                             (query ?target-4 ?target-object-1 ?attribute-6))))))))

(defun run-tests ()
  (test-categorial-links-repair-comprehension))

;(run-tests)

