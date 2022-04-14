(in-package :grammar-learning)

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
    (test-repair-status 'holophrase->item-based+holistic+holistic--substitution
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
    (test-repair-status 'holophrase->item-based+holistic+holistic--substitution
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
    (test-repair-status 'nothing->holophrase
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
    (test-repair-status 'holophrase->item-based+holistic+holistic--substitution
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
    (test-repair-status 'nothing->holophrase
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


(deftest test-triple-substitution-repair-comprehension ()
  "This test demonstrates the problem of synonymy. Small and tiny, thing and shape are synonymous, so they are not part of the holistic chunk, but remain part of the item-based cxn"
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
    (test-repair-status 'holophrase->item-based+holistic+holistic--substitution
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

(deftest test-reordered-form-substitution-repair-comprehension ()
  "This test demonstrates the problem of synonymy and reordering. Small and tiny, thing and shape are synonymous, so they are not part of the holistic chunk, but remain part of the item-based cxn"
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
    (test-repair-status 'nothing->holophrase
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
    (test-repair-status 'nothing->holophrase
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
    (test-repair-status 'holophrase->item-based+holistic+holistic--substitution
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
    (test-repair-status 'holophrase->item-based+holistic+holistic--substitution
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

(deftest test-no-duplicate-item-based-cxns-substitution-comprehension ()
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
           
           
           (test-repair-status 'item-based->holistic
                               (second (multiple-value-list
                                        (comprehend "What is the size of the yellow metallic cube?"
                       :cxn-inventory cxn-inventory
                       :gold-standard-meaning '((get-context ?source-1)
                                                (filter ?target-86448 ?target-2 ?color-16)
                                                (unique ?target-object-1 ?target-86448)
                                                (bind material-category ?material-4 metal)
                                                (filter ?target-1 ?source-1 ?shape-2)
                                                (bind attribute-category ?attribute-6 size)
                                                (bind shape-category ?shape-2 cube)
                                                (filter ?target-2 ?target-1 ?material-4)
                                                (bind color-category ?color-16 yellow)
                                                (query ?target-4 ?target-object-1 ?attribute-6))))))
           (test-equal 5 (length (constructions cxn-inventory)))))


;; (activate-monitor trace-fcg)
;; (test-substitution-repair-comprehension) ;ok
;; (test-substitution-repair-comprehension-right) ;ok
;; (test-substitution-repair-comprehension-multi-diff) ;should be holophrase
;; (test-double-substitution-repair-comprehension) ;ok
;; (test-double-discontinuous-substitution-repair-comprehension) ;should be holophrase
;; (test-triple-substitution-repair-comprehension) ;ok
;; (test-reordered-form-substitution-repair-comprehension) ;should be holophrase
;; (test-varying-word-order-substitution-comprehension) ;should be holophrase
;; (test-varying-length-substitution-repair-comprehension) ;ok
;; (test-varying-length-substitution-repair-comprehension-reversed) ;ok
;; (test-no-duplicate-item-based-cxns-substitution-comprehension) ;ok
