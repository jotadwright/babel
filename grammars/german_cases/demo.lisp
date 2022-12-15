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





;;;POSTER EXAMPLE SUBSTITUTION + FORMULATION
(defun test-substitution-repair-comprehension-german ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs-german))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "Der Doktor verkauft dem Clown das Buch"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((verkaufen-01 ?v)
                                             (doctor ?d)
                                             (clown ?c)
                                             (book ?b)
                                             (arg0 ?v ?d)
                                             (arg1 ?v ?b)
                                             (arg2 ?v ?c)
                                             (topicalized ?d)))
    (test-repair-status 'holistic->item-based--substitution
                        (second (multiple-value-list
                                 (comprehend "Der Doktor verkauft der Frau das Buch"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((verkaufen-01 ?v)
                                             (doctor ?d)
                                             (woman ?w)
                                             (book ?b)
                                             (arg0 ?v ?d)
                                             (arg1 ?v ?b)
                                             (arg2 ?v ?w)
                                             (topicalized ?d))))))
    (formulate '((verkaufen-01 ?v)
                                             (doctor ?d)
                                             (woman ?w)
                                             (book ?b)
                                             (arg0 ?v ?d)
                                             (arg1 ?v ?b)
                                             (arg2 ?v ?w)
                                             (topicalized ?d)) :cxn-inventory cxn-inventory)))

(test-substitution-repair-comprehension-german)



;;;ADDITION REPAIR TEST
(defun test-addition-repair-comprehension-german ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs-german))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "Der Doktor verkauft der Frau das Buch"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((verkaufen-01 ?v)
                                             (doctor ?d)
                                             (woman ?w)
                                             (book ?b)
                                             (arg0 ?v ?d)
                                             (arg1 ?v ?b)
                                             (arg2 ?v ?w)
                                             (topicalized ?d)))
    (test-repair-status 'holistic->item-based--addition
                        (second (multiple-value-list
                                 (comprehend "Der Doktor verkauft der Frau das neue Buch"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((verkaufen-01 ?v)
                                             (doctor ?d)
                                             (woman ?w)
                                             (book ?b)
                                             (new ?n)
                                             (mod ?b ?n)
                                             (arg0 ?v ?d)
                                             (arg1 ?v ?b)
                                             (arg2 ?v ?w)
                                             (topicalized ?d))))))
    (formulate '((verkaufen-01 ?v)
                                             (doctor ?d)
                                             (woman ?w)
                                             (book ?b)
                                             (arg0 ?v ?d)
                                             (arg1 ?v ?b)
                                             (arg2 ?v ?w)
                                             (topicalized ?d)) :cxn-inventory cxn-inventory)))

(test-addition-repair-comprehension-german)











(defun run-tests-corpora()
  (test-addition-repair-comprehension-german)
  (test-substitution-repair-comprehension-german))

;(run-tests-corpora)