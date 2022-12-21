(in-package :grammar-learning)

(defun test-addition-repair-comprehension-geo ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs-geo))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "How long is the Mississippi ?"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((ANSWER ?C ?A ?D)
                                             (LEN ?D ?B ?A)
                                             (CONST ?D ?B ?E)
                                             (RIVERID ?E ?F)
                                             (MISSISSIPPI ?F)))
    (test-repair-status 'holophrase->item-based+holistic--addition
                        (second (multiple-value-list
                                 (comprehend "How long is the Mississippi river ?"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((ANSWER ?C ?A ?D)
                                             (LEN ?D ?B ?A)
                                             (CONST ?D ?B ?E)
                                             (RIVERID ?E ?F)
                                             (MISSISSIPPI ?F)
                                             (RIVER ?D ?B))))))))

; (activate-monitor trace-fcg)
(defun run-addition-tests ()
  (test-addition-repair-comprehension-geo)
  
  )

;(run-addition-tests)

