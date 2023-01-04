(ql:quickload :grammar-learning) 
(in-package :grammar-learning)


;; full logging
(progn
  (deactivate-all-monitors)
  (activate-monitor display-metrics)
  (activate-monitor trace-fcg)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor summarize-results-after-n-interactions)
  (activate-monitor show-type-hierarchy-after-n-interactions)
  (activate-monitor trace-interactions-in-wi))



#|(deftest test-deletion-repair-clevr ()
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
    (test-repair-status 'holistic->item-based--deletion
                        (second (multiple-value-list
                                 (comprehend "The gray object is what shape?"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((get-context ?source-1)
                                             (filter ?target-2 ?target-1 ?color-2)
                                             (unique ?source-9 ?target-2)
                                             (bind shape-category ?shape-8 thing)
                                             (bind attribute-category ?attribute-2 shape)
                                             (filter ?target-1 ?source-1 ?shape-8)
                                             (bind color-category ?color-2 gray)
                                             (query ?target-7 ?source-9 ?attribute-2))))))))|#


;(test-deletion-repair-clevr)

;(define-configuration-default-value :meaning-representation :geo)


;default is IRL
;with GEO repair doesnt work (all NILs) since no cxn detected as T


(defun test-deletion-repair-comprehension-german ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "Die junge Frau gibt dem Mann den gruenen Apfel"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((geben-01 ?g)
                                             (young ?y)
                                             (woman ?w)
                                             (mod ?w ?y)
                                             (man ?m)
                                             (apple ?a)
                                             (green ?gr)
                                             (mod ?a ?gr)
                                             (arg0 ?g ?w)
                                             (arg1 ?g ?a)
                                             (arg2 ?g ?m)
                                             (topicalized ?w)))
    
    (test-repair-status 'holistic->item-based--deletion
                        (second (multiple-value-list
                                 (comprehend "Die junge Frau gibt dem Mann den Apfel"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((geben-01 ?g)
                                             (young ?y)
                                             (woman ?w)
                                             (mod ?w ?y)
                                             (man ?m)
                                             (apple ?a)
                                             (arg0 ?g ?w)
                                             (arg1 ?g ?a)
                                             (arg2 ?g ?m)
                                             (topicalized ?w))))))
    (comprehend "Die junge Frau gibt dem Mann den gruenen Apfel"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((geben-01 ?g)
                                             (young ?y)
                                             (woman ?w)
                                             (mod ?w ?y)
                                             (man ?m)
                                             (apple ?a)
                                             (green ?gr)
                                             (mod ?a ?gr)
                                             (arg0 ?g ?w)
                                             (arg1 ?g ?a)
                                             (arg2 ?g ?m)
                                             (topicalized ?w)))
    (add-element (make-html cxn-inventory))))




(test-deletion-repair-comprehension-german)


;;;error if set to AMR

;++++ Error in (LISPWORKS:TOP-LEVEL-FORM 5): 
  ;no deletion conditions implemented for this meaning representation (debugging I get the following with Check deletion conditions)

;;;;details

;OBSERVATION (Die junge Frau gibt dem Mann den Apfel)
;overlapping meaning NIL
;overlapping form NIL
;non-overlapping meaning ALL
;non-overlapping form  ALL



;CXN (Die junge Frau gibt dem Mann den groenen Apfel)
;overlapping meaning NIL (should be all but GREEN)
;overlapping form all but GREEN (should be all)
;non-overlapping meaning green (should be ALL)
;non-overlapping form  GREEN