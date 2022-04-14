(in-package :grammar-learning)
;(activate-monitor trace-fcg)
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
           (comprehend "What size is the green sphere?"
                       :cxn-inventory cxn-inventory
                       :gold-standard-meaning '((get-context ?source-1)
                                                (filter ?target-2 ?target-1 ?color-8)
                                                (unique ?target-object-1 ?target-2)
                                                (bind shape-category ?shape-4 sphere)
                                                (bind attribute-category ?attribute-6 size)
                                                (filter ?target-1 ?source-1 ?shape-4)
                                                (bind color-category ?color-8 green)
                                                (query ?target-4 ?target-object-1 ?attribute-6)))
    
           (comprehend "What size is the purple sphere?"
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
                                        (comprehend "What size is the yellow sphere?"
                                                    :cxn-inventory cxn-inventory
                                                    :gold-standard-meaning '((get-context ?source-1)
                                                                             (filter ?target-2 ?target-1 ?color-8)
                                                                             (unique ?target-object-1 ?target-2)
                                                                             (bind shape-category ?shape-4 sphere)
                                                                             (bind attribute-category ?attribute-6 size)
                                                                             (filter ?target-1 ?source-1 ?shape-4)
                                                                             (bind color-category ?color-8 yellow)
                                                                             (query ?target-4 ?target-object-1 ?attribute-6))))))))

(deftest test-add-categorial-link-after-holistic-to-item-based-comprehension ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "The tiny gray object is what shape?"
              :cxn-inventory cxn-inventory
              :gold-standard-meaning '((get-context ?source-1)
                                       (filter ?target-39552 ?target-2 ?size-4)
                                       (unique ?source-10 ?target-39552)
                                       (bind color-category ?color-2 gray)
                                       (filter ?target-1 ?source-1 ?shape-8)
                                       (bind attribute-category ?attribute-2 shape)
                                       (bind shape-category ?shape-8 thing)
                                       (filter ?target-2 ?target-1 ?color-2)
                                       (bind size-category ?size-4 small)
                                       (query ?target-8 ?source-10 ?attribute-2)))
    (comprehend "The large yellow object is what shape?"
              :cxn-inventory cxn-inventory
              :gold-standard-meaning '((get-context ?source-1)
                                       (filter ?target-39552 ?target-2 ?size-4)
                                       (unique ?source-10 ?target-39552)
                                       (bind color-category ?color-2 yellow)
                                       (filter ?target-1 ?source-1 ?shape-8)
                                       (bind attribute-category ?attribute-2 shape)
                                       (bind shape-category ?shape-8 thing)
                                       (filter ?target-2 ?target-1 ?color-2)
                                       (bind size-category ?size-4 large)
                                       (query ?target-8 ?source-10 ?attribute-2)))
    (test-repair-status 'holistic->item-based
                        (second (multiple-value-list
                                 (comprehend "What is the shape of the tiny gray object?"
              :cxn-inventory cxn-inventory
              :gold-standard-meaning '((get-context ?source-1)
                                       (filter ?target-39552 ?target-2 ?size-4)
                                       (unique ?source-10 ?target-39552)
                                       (bind color-category ?color-2 gray)
                                       (filter ?target-1 ?source-1 ?shape-8)
                                       (bind attribute-category ?attribute-2 shape)
                                       (bind shape-category ?shape-8 thing)
                                       (filter ?target-2 ?target-1 ?color-2)
                                       (bind size-category ?size-4 small)
                                       (query ?target-8 ?source-10 ?attribute-2))))))
    (test-repair-status 'add-categorial-links
                        (second (multiple-value-list
                                 (comprehend "What is the shape of the large yellow object?"
              :cxn-inventory cxn-inventory
              :gold-standard-meaning '((get-context ?source-1)
                                       (filter ?target-56342 ?target-2 ?size-4)
                                       (unique ?target-object-1 ?target-56342)
                                       (bind color-category ?color-16 yellow)
                                       (filter ?target-1 ?source-1 ?shape-8)
                                       (bind attribute-category ?attribute-2 shape)
                                       (bind shape-category ?shape-8 thing)
                                       (filter ?target-2 ?target-1 ?color-16)
                                       (bind size-category ?size-4 large)
                                       (query ?target-4 ?target-object-1 ?attribute-2))))))))

; (activate-monitor trace-fcg)
; (test-categorial-links-repair-comprehension)
; (test-add-categorial-link-after-holistic-to-item-based-comprehension)
 

