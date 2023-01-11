(ql:quickload :grammar-learning) 
(in-package :grammar-learning)


;need to make deletion work first 

(deftest test-holistic-to-item-based-from-substitution-comprehension ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs-german))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "Die junge blonde Frau gibt dem Mann den Apfel"
              :cxn-inventory cxn-inventory
              :gold-standard-meaning '((geben-01 ?g)
                                       (young ?y)
                                       (blond ?b)
                                       (woman ?w)
                                       (mod ?w ?y)
                                       (mod ?w ?b)
                                       (man ?m)
                                       (apple ?a)
                                       (arg0 ?g ?w)
                                       (arg1 ?g ?a)
                                       (arg2 ?g ?m)
                                       (topicalized ?w)))
    (comprehend "Die alte blonde Frau gibt dem Mann den Apfel"
              :cxn-inventory cxn-inventory
              :gold-standard-meaning '((geben-01 ?g)
                                       (old ?o)
                                       (blond ?b)
                                       (woman ?w)
                                       (mod ?w ?o)
                                       (mod ?w ?b)
                                       (man ?m)
                                       (apple ?a)
                                       (arg0 ?g ?w)
                                       (arg1 ?g ?a)
                                       (arg2 ?g ?m)
                                       (topicalized ?w)))
    (test-repair-status 'holistic->item-based
                        (second (multiple-value-list
                                 (comprehend "Die alte Frau sieht den Mann"
              :cxn-inventory cxn-inventory
              :gold-standard-meaning '((sehen-01 ?s)
                                       (woman ?w)
                                       (old ?o)
                                       (mod ?w ?o)
                                       (man ?m)
                                       (arg0 ?s ?w)
                                       (arg1 ?s ?m)
                                       (topicalized ?w))))))))

(test-holistic-to-item-based-from-substitution-comprehension)
