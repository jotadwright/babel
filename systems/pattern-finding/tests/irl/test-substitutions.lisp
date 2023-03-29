(in-package :pattern-finding)


(deftest test-substitution-repair-comprehension ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs))
         (cxn-inventory (grammar (first (agents experiment)))))
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
                                         (query ?target-8 ?source-10 ?attribute-2)))
    (test-repair-status 'holistic->item-based
                        (second (multiple-value-list
                                 (comprehend "The large yellow object is what shape?"
                                             :cxn-inventory cxn-inventory
                                             :gold-standard-meaning '((get-context ?source-1)
                                                                      (filter ?target-14197 ?target-2 ?size-2)
                                                                      (unique ?source-10 ?target-14197)
                                                                      (bind color-category ?color-16 yellow)
                                                                      (filter ?target-1 ?source-1 ?shape-8)
                                                                      (bind attribute-category ?attribute-2 shape)
                                                                      (bind shape-category ?shape-8 thing)
                                                                      (filter ?target-2 ?target-1 ?color-16)
                                                                      (bind size-category ?size-2 large)
                                                                      (query ?target-8 ?source-10 ?attribute-2))))))))
;(test-substitution-repair-comprehension)



(deftest test-substitution-repair-comprehension-right ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs))
         (cxn-inventory (grammar (first (agents experiment)))))
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
                                         (query ?target-8 ?source-10 ?attribute-2)))
    (test-repair-status 'holistic->item-based
                        (second (multiple-value-list
                                 (comprehend "The large gray object is what material?"
                                             :cxn-inventory cxn-inventory
                                             :gold-standard-meaning '((get-context ?source-1)
                                                                      (filter ?target-14197 ?target-2 ?size-2)
                                                                      (unique ?source-10 ?target-14197)
                                                                      (bind color-category ?color-16 gray)
                                                                      (filter ?target-1 ?source-1 ?shape-8)
                                                                      (bind attribute-category ?attribute-2 material)
                                                                      (bind shape-category ?shape-8 thing)
                                                                      (filter ?target-2 ?target-1 ?color-16)
                                                                      (bind size-category ?size-2 large)
                                                                      (query ?target-8 ?source-10 ?attribute-2))))))))
;(test-substitution-repair-comprehension-right)



(deftest test-substitution-repair-comprehension-multi-diff ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs))
         (cxn-inventory (grammar (first (agents experiment)))))
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
                                             (query ?target-8 ?source-10 ?attribute-2)))
    (test-repair-status 'holistic->item-based
                        (second (multiple-value-list
                                 (comprehend "The large yellow object is what material?"
                                             :cxn-inventory cxn-inventory
                                             :gold-standard-meaning '((get-context ?source-1)
                                                                      (filter ?target-14197 ?target-2 ?size-2)
                                                                      (unique ?source-10 ?target-14197)
                                                                      (bind color-category ?color-16 yellow)
                                                                      (filter ?target-1 ?source-1 ?shape-8)
                                                                      (bind attribute-category ?attribute-2 material)
                                                                      (bind shape-category ?shape-8 thing)
                                                                      (filter ?target-2 ?target-1 ?color-16)
                                                                      (bind size-category ?size-2 large)
                                                                      (query ?target-8 ?source-10 ?attribute-2))))))))
;(test-substitution-repair-comprehension-multi-diff)



(deftest test-double-substitution-repair-comprehension ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs))
         (cxn-inventory (grammar (first (agents experiment)))))
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
                                             (query ?target-8 ?source-10 ?attribute-2)))
    (test-repair-status 'holistic->item-based
                        (second (multiple-value-list
                                 (comprehend "The tiny yellow object is what shape?"
                                             :cxn-inventory cxn-inventory
                                             :gold-standard-meaning '((get-context ?source-1)
                                                                      (filter ?target-14197 ?target-2 ?size-2)
                                                                      (unique ?source-10 ?target-14197)
                                                                      (bind color-category ?color-16 yellow)
                                                                      (filter ?target-1 ?source-1 ?shape-8)
                                                                      (bind attribute-category ?attribute-2 shape)
                                                                      (bind shape-category ?shape-8 thing)
                                                                      (filter ?target-2 ?target-1 ?color-16)
                                                                      (bind size-category ?size-2 small)
                                                                      (query ?target-8 ?source-10 ?attribute-2))))))))
;(test-double-substitution-repair-comprehension)



(deftest test-double-discontinuous-substitution-repair-comprehension ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs))
         (cxn-inventory (grammar (first (agents experiment)))))
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
                                             (query ?target-8 ?source-10 ?attribute-2)))
    (test-repair-status 'holistic->item-based
                        (second (multiple-value-list
                                 (comprehend "The tiny yellow object is what material?"
                                             :cxn-inventory cxn-inventory
                                             :gold-standard-meaning '((get-context ?source-1)
                                                                      (filter ?target-14197 ?target-2 ?size-2)
                                                                      (unique ?source-10 ?target-14197)
                                                                      (bind color-category ?color-16 yellow)
                                                                      (filter ?target-1 ?source-1 ?shape-8)
                                                                      (bind attribute-category ?attribute-2 material)
                                                                      (bind shape-category ?shape-8 thing)
                                                                      (filter ?target-2 ?target-1 ?color-16)
                                                                      (bind size-category ?size-2 small)
                                                                      (query ?target-8 ?source-10 ?attribute-2))))))))
;(test-double-discontinuous-substitution-repair-comprehension)



(deftest test-triple-substitution-repair-comprehension ()
  "This test demonstrates the problem of synonymy. Small and tiny, thing and shape are synonymous, 
  so they are not part of the holistic chunk, but remain part of the item-based cxn"
  (let* ((experiment (set-up-cxn-inventory-and-repairs))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "The small purple thing is what shape?"
                :cxn-inventory cxn-inventory
                :gold-standard-meaning '((get-context ?source-1)
                                         (filter ?target-66128 ?target-2 ?size-2)
                                         (unique ?source-10 ?target-66128)
                                         (bind color-category ?color-12 purple)
                                         (filter ?target-1 ?source-1 ?shape-8)
                                         (bind shape-category ?shape-8 thing)
                                         (bind attribute-category ?attribute-2 shape)
                                         (filter ?target-2 ?target-1 ?color-12)
                                         (bind size-category ?size-2 small)
                                         (query ?target-8 ?source-10 ?attribute-2)))
    (test-repair-status 'holistic->item-based
                        (second (multiple-value-list
                                 (comprehend "The tiny blue object is what shape?"
                                             :cxn-inventory cxn-inventory
                                             :gold-standard-meaning '((get-context ?source-1)
                                                                      (filter ?target-93243 ?target-2 ?size-2)
                                                                      (unique ?source-10 ?target-93243)
                                                                      (bind size-category ?size-2 small)
                                                                      (filter ?target-2 ?target-1 ?color-6)
                                                                      (bind shape-category ?shape-8 thing)
                                                                      (filter ?target-1 ?source-1 ?shape-8)
                                                                      (bind color-category ?color-6 blue)
                                                                      (bind attribute-category ?attribute-2 shape)
                                                                      (query ?target-8 ?source-10 ?attribute-2))))))))
;(test-triple-substitution-repair-comprehension)



(deftest test-reordered-form-substitution-repair-comprehension ()
  "This test demonstrates the problem of synonymy and reordering. Small and tiny, thing and shape are synonymous, 
  so they are not part of the holistic chunk, but remain part of the item-based cxn"
  (let* ((experiment (set-up-cxn-inventory-and-repairs))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "What shape is the small purple thing?"
                :cxn-inventory cxn-inventory
                :gold-standard-meaning '((get-context ?source-1)
                                         (filter ?target-66128 ?target-2 ?size-2)
                                         (unique ?source-10 ?target-66128)
                                         (bind color-category ?color-12 purple)
                                         (filter ?target-1 ?source-1 ?shape-8)
                                         (bind shape-category ?shape-8 thing)
                                         (bind attribute-category ?attribute-2 shape)
                                         (filter ?target-2 ?target-1 ?color-12)
                                         (bind size-category ?size-2 small)
                                         (query ?target-8 ?source-10 ?attribute-2)))
    (test-repair-status 'holistic->item-based
                        (second (multiple-value-list
                                 (comprehend "The tiny blue object is what shape?"
                                             :cxn-inventory cxn-inventory
                                             :gold-standard-meaning '((get-context ?source-1)
                                                                      (filter ?target-93243 ?target-2 ?size-2)
                                                                      (unique ?source-10 ?target-93243)
                                                                      (bind size-category ?size-2 small)
                                                                      (filter ?target-2 ?target-1 ?color-6)
                                                                      (bind shape-category ?shape-8 thing)
                                                                      (filter ?target-1 ?source-1 ?shape-8)
                                                                      (bind color-category ?color-6 blue)
                                                                      (bind attribute-category ?attribute-2 shape)
                                                                      (query ?target-8 ?source-10 ?attribute-2))))))))
;(test-reordered-form-substitution-repair-comprehension)



(deftest test-varying-word-order-substitution-comprehension ()
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
    (test-repair-status 'item-based->holistic
                        (second (multiple-value-list
                                 (comprehend "What is the material of the tiny gray object?"
                                             :cxn-inventory cxn-inventory
                                             :gold-standard-meaning '((get-context ?source-1)
                                                                      (bind attribute-category ?attribute-8 material)
                                                                      (bind size-category ?size-2 small)
                                                                      (filter ?target-2 ?target-1 ?color-2)
                                                                      (bind shape-category ?shape-8 thing)
                                                                      (filter ?target-1 ?source-1 ?shape-8)
                                                                      (bind color-category ?color-2 gray)
                                                                      (unique ?target-object-1 ?target-77105)
                                                                      (filter ?target-77105 ?target-2 ?size-2)
                                                                      (query ?target-4 ?target-object-1 ?attribute-8))))))))
;(test-varying-word-order-substitution-comprehension)



(deftest test-varying-length-substitution-repair-comprehension ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs))
         (cxn-inventory (grammar (first (agents experiment)))))
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
                                             (query ?target-8 ?source-10 ?attribute-2)))
    (test-repair-status 'holistic->item-based
                        (second (multiple-value-list
                                 (comprehend "The yellow object is what shape?"
                                             :cxn-inventory cxn-inventory
                                             :gold-standard-meaning '((get-context ?source-1)
                                                                      (unique ?source-10 ?target-14197)
                                                                      (bind color-category ?color-16 yellow)
                                                                      (filter ?target-1 ?source-1 ?shape-8)
                                                                      (bind attribute-category ?attribute-2 shape)
                                                                      (bind shape-category ?shape-8 thing)
                                                                      (filter ?target-14197 ?target-1 ?color-16)
                                                                      (query ?target-8 ?source-10 ?attribute-2))))))))
;(test-varying-length-substitution-repair-comprehension)


(deftest test-varying-length-substitution-repair-comprehension-reversed ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "The yellow object is what shape?"
                :cxn-inventory cxn-inventory
                :gold-standard-meaning '((get-context ?source-1)
                                         (unique ?source-10 ?target-14197)
                                         (bind color-category ?color-16 yellow)
                                         (filter ?target-1 ?source-1 ?shape-8)
                                         (bind attribute-category ?attribute-2 shape)
                                         (bind shape-category ?shape-8 thing)
                                         (filter ?target-14197 ?target-1 ?color-16)
                                         (query ?target-8 ?source-10 ?attribute-2)))
    (test-repair-status 'holistic->item-based
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
;(test-varying-length-substitution-repair-comprehension-reversed)



(deftest test-discontinuous-substitution-common-middle-element ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "How many brown shiny objects are there?"
                :cxn-inventory cxn-inventory
                :gold-standard-meaning '((get-context ?source-1)
                                         (filter ?target-5862 ?target-2 ?color-10)
                                         (bind material-category ?material-4 metal)
                                         (filter ?target-1 ?source-1 ?shape-8)
                                         (bind shape-category ?shape-8 thing)
                                         (filter ?target-2 ?target-1 ?material-4)
                                         (bind color-category ?color-10 brown)
                                         (count! ?target-16 ?target-5862)))
    (test-repair-status 'holistic->item-based
                        (second (multiple-value-list
                                 (comprehend "How many big green shiny blocks are there?"
                                             :cxn-inventory cxn-inventory
                                             :gold-standard-meaning '((get-context ?source-1)
                                                                      (filter ?target-9641 ?target-9626 ?size-4)
                                                                      (bind color-category ?color-8 green)
                                                                      (filter ?target-2 ?target-1 ?material-5)
                                                                      (bind shape-category ?shape-2 cube)
                                                                      (filter ?target-1 ?source-1 ?shape-2)
                                                                      (bind material-category ?material-5 metal)
                                                                      (filter ?target-9626 ?target-2 ?color-8)
                                                                      (bind size-category ?size-4 large)
                                                                      (count! ?target-16 ?target-9641))))))))
;(test-discontinuous-substitution-common-middle-element)

