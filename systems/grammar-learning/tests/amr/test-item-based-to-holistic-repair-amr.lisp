(in-package :grammar-learning)


(deftest test-item-based-to-holistic-repair-comprehension-amr ()
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
                                 (comprehend "Oh !"
                                             :cxn-inventory cxn-inventory
                                             :gold-standard-meaning '((:MODE ?O EXPRESSIVE)
                                                                      (OH ?O))))))))

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

;
; (test-item-based-to-holistic-repair-comprehension-amr) ; ok
; (test-item-based-to-holistic-repair-comprehension-leading-quote-amr) ; ok
