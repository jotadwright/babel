(in-package :grammar-learning)

(deftest test-substitution-repair-comprehension-amr ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs-amr))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "Hum !"
                :cxn-inventory cxn-inventory
                :gold-standard-meaning '((:MODE ?H EXPRESSIVE)
                                         (HUM ?H)))
    (test-repair-status 'holistic->item-based--substitution
                        (second (multiple-value-list
                                 (comprehend "Ah !"
                                             :cxn-inventory cxn-inventory
                                             :gold-standard-meaning '((:MODE ?A EXPRESSIVE)
                                                                      (AH ?A))))))))

(deftest test-substitution-repair-comprehension-reverse-amr ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs-amr))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "! Hum"
                :cxn-inventory cxn-inventory
                :gold-standard-meaning '((:MODE ?H EXPRESSIVE)
                                         (HUM ?H)))
    (test-repair-status 'holistic->item-based--substitution
                        (second (multiple-value-list
                                 (comprehend "! Ah"
                                             :cxn-inventory cxn-inventory
                                             :gold-standard-meaning '((:MODE ?A EXPRESSIVE)
                                                                      (AH ?A))))))))


(deftest test-substitution-repair-comprehension-empty-meaning-amr ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs-amr))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "Chapter 1 ."
                :cxn-inventory cxn-inventory
                :gold-standard-meaning '((chapter ?c) (:mod ?c 1)))
    (test-repair-status 'nothing->holophrase
                        (second (multiple-value-list
                                 (comprehend "I said ."
                                             :cxn-inventory cxn-inventory
                                             :gold-standard-meaning '((say-01 ?s) (i ?i) (:arg0 ?s ?i))))))))





;; (activate-monitor trace-fcg)
;; AMR testcases
;; (test-substitution-repair-comprehension-amr)
;; (test-substitution-repair-comprehension-reverse-amr)
;; (test-substitution-repair-comprehension-empty-meaning-amr) ;should be holophrase
