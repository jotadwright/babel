(in-package :grammar-learning)

(defun test-item-based-to-holistic-multiple-item-based-cxns-comprehension ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "The gray object is what material?"
              :cxn-inventory cxn-inventory
              :gold-standard-meaning '((get-context ?source-1)
                                       (filter ?target-2 ?target-1 ?color-2)
                                       (unique ?source-9 ?target-2)
                                       (bind shape-category ?shape-8 thing)
                                       (bind attribute-category ?attribute-2 material)
                                       (filter ?target-1 ?source-1 ?shape-8)
                                       (bind color-category ?color-2 gray)
                                       (query ?target-7 ?source-9 ?attribute-2)))
    (comprehend "The large gray object is what material?"
              :cxn-inventory cxn-inventory
              :gold-standard-meaning '((get-context ?source-1)
                                       (filter ?target-39552 ?target-2 ?size-4)
                                       (unique ?source-10 ?target-39552)
                                       (bind color-category ?color-2 gray)
                                       (filter ?target-1 ?source-1 ?shape-8)
                                       (bind attribute-category ?attribute-2 material)
                                       (bind shape-category ?shape-8 thing)
                                       (filter ?target-2 ?target-1 ?color-2)
                                       (bind size-category ?size-4 large)
                                       (query ?target-8 ?source-10 ?attribute-2)))
    (comprehend "The blue sphere is what size?"
              :cxn-inventory cxn-inventory
              :gold-standard-meaning '((get-context ?source-1)
                                       (filter ?target-2 ?target-1 ?color-2)
                                       (unique ?source-9 ?target-2)
                                       (bind shape-category ?shape-8 sphere)
                                       (bind attribute-category ?attribute-2 size)
                                       (filter ?target-1 ?source-1 ?shape-8)
                                       (bind color-category ?color-2 blue)
                                       (query ?target-7 ?source-9 ?attribute-2)))
    (comprehend "The blue sphere is what material?"
              :cxn-inventory cxn-inventory
              :gold-standard-meaning '((get-context ?source-1)
                                       (filter ?target-2 ?target-1 ?color-2)
                                       (unique ?source-9 ?target-2)
                                       (bind shape-category ?shape-8 sphere)
                                       (bind attribute-category ?attribute-2 material)
                                       (filter ?target-1 ?source-1 ?shape-8)
                                       (bind color-category ?color-2 blue)
                                       (query ?target-7 ?source-9 ?attribute-2)))
    
    (test-repair-status 'item-based->holistic
                        (second (multiple-value-list
                                 (comprehend "The small gray object is what material?"
              :cxn-inventory cxn-inventory
              :gold-standard-meaning '((get-context ?source-1)
                                       (filter ?target-107036 ?target-2 ?size-2)
                                       (unique ?source-9 ?target-107036)
                                       (bind attribute-category ?attribute-2 material)
                                       (bind shape-category ?shape-8 thing)
                                       (filter ?target-1 ?source-1 ?shape-8)
                                       (bind color-category ?color-2 gray)
                                       (filter ?target-2 ?target-1 ?color-2)
                                       (bind size-category ?size-2 small)
                                       (query ?target-7 ?source-9 ?attribute-2))))))
    (test-equal 8 (length (constructions cxn-inventory)))))

(deftest test-item-based-to-holistic-comprehension ()
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
    (comprehend "The gray object is what color?"
              :cxn-inventory cxn-inventory
              :gold-standard-meaning '((get-context ?source-1)
                                       (filter ?target-2 ?target-1 ?color-2)
                                       (unique ?source-9 ?target-2)
                                       (bind shape-category ?shape-8 thing)
                                       (bind attribute-category ?attribute-2 color)
                                       (filter ?target-1 ?source-1 ?shape-8)
                                       (bind color-category ?color-2 gray)
                                       (query ?target-7 ?source-9 ?attribute-2)))
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
    (comprehend "The large gray object has what color?"
              :cxn-inventory cxn-inventory
              :gold-standard-meaning '((get-context ?source-1)
                                       (filter ?target-107036 ?target-2 ?size-2)
                                       (unique ?source-9 ?target-107036)
                                       (bind attribute-category ?attribute-2 color)
                                       (bind shape-category ?shape-8 thing)
                                       (filter ?target-1 ?source-1 ?shape-8)
                                       (bind color-category ?color-2 gray)
                                       (filter ?target-2 ?target-1 ?color-2)
                                       (bind size-category ?size-2 large)
                                       (query ?target-7 ?source-9 ?attribute-2)))
    (test-repair-status 'item-based->holistic
                        (second (multiple-value-list
                                 (comprehend "The shiny gray object has what color?"
              :cxn-inventory cxn-inventory
              :gold-standard-meaning '((get-context ?source-1)
                                       (filter ?target-107036 ?target-2 ?size-2)
                                       (unique ?source-9 ?target-107036)
                                       (bind attribute-category ?attribute-2 color)
                                       (bind shape-category ?shape-8 thing)
                                       (filter ?target-1 ?source-1 ?shape-8)
                                       (bind color-category ?color-2 gray)
                                       (filter ?target-2 ?target-1 ?color-2)
                                       (bind material-category ?size-2 metal)
                                       (query ?target-7 ?source-9 ?attribute-2))))))
    (test-equal 8 (length (constructions cxn-inventory)))))

(deftest test-item-based-to-holistic-double-comprehension ()
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
    (comprehend "The gray object is what color?"
              :cxn-inventory cxn-inventory
              :gold-standard-meaning '((get-context ?source-1)
                                       (filter ?target-2 ?target-1 ?color-2)
                                       (unique ?source-9 ?target-2)
                                       (bind shape-category ?shape-8 thing)
                                       (bind attribute-category ?attribute-2 color)
                                       (filter ?target-1 ?source-1 ?shape-8)
                                       (bind color-category ?color-2 gray)
                                       (query ?target-7 ?source-9 ?attribute-2)))
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
    (comprehend "The large gray object has what color?"
              :cxn-inventory cxn-inventory
              :gold-standard-meaning '((get-context ?source-1)
                                       (filter ?target-107036 ?target-2 ?size-2)
                                       (unique ?source-9 ?target-107036)
                                       (bind attribute-category ?attribute-2 color)
                                       (bind shape-category ?shape-8 thing)
                                       (filter ?target-1 ?source-1 ?shape-8)
                                       (bind color-category ?color-2 gray)
                                       (filter ?target-2 ?target-1 ?color-2)
                                       (bind size-category ?size-2 large)
                                       (query ?target-7 ?source-9 ?attribute-2)))
    (test-repair-status 'item-based->holistic
                        (second (multiple-value-list
                                 (comprehend "The large shiny gray object has what color?"
              :cxn-inventory cxn-inventory
              :gold-standard-meaning '((get-context ?source-1)
                                       (filter ?target-107036 ?target-2 ?material-9)
                                       (filter ?target-107037 ?target-107036 ?size-2)
                                       (unique ?source-9 ?target-107037)
                                       (bind attribute-category ?attribute-2 color)
                                       (bind shape-category ?shape-8 thing)
                                       (filter ?target-1 ?source-1 ?shape-8)
                                       (bind color-category ?color-2 gray)
                                       (filter ?target-2 ?target-1 ?color-2)
                                       (bind size-category ?material-9 metal)
                                       (bind material-category ?size-2 large)
                                       (query ?target-7 ?source-9 ?attribute-2))))))
    (test-equal 8 (length (constructions cxn-inventory)))))


(deftest test-item-based-to-holistic-repair-comprehension-leading-quote-amr ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs-amr))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "Hum !"
                :cxn-inventory cxn-inventory
                :gold-standard-meaning '((:MODE ?H EXPRESSIVE)
                                         (HUM ?H)))
    (comprehend "Ah !"
                :cxn-inventory cxn-inventory
                :gold-standard-meaning '((:MODE ?A EXPRESSIVE)
                                         (AH ?A)))
    (test-repair-status 'item-based->holistic
                        (second (multiple-value-list
                                 (comprehend "\" Oh !"
                                             :cxn-inventory cxn-inventory
                                             :gold-standard-meaning '((:MODE ?O EXPRESSIVE)
                                                                      (OH ?O))))))))

; (activate-monitor trace-fcg)
; (test-item-based-to-holistic-double-comprehension)
; (test-item-based-to-holistic-comprehension)
; (test-item-based-to-holistic-multiple-item-based-cxns-comprehension)
;
; (test-item-based-to-holistic-repair-comprehension-leading-quote-amr)