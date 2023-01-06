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

;(test-substitution-repair-comprehension-german)


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

;(test-addition-repair-comprehension-german)

(defun test-addition-and-substitution-repair-comprehension-german ()
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
    (comprehend "Der Doktor verkauft der Frau das rote Buch"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((verkaufen-01 ?v)
                                             (doctor ?d)
                                             (woman ?w)
                                             (book ?b)
                                             (red ?r)
                                             (mod ?b ?r)
                                             (arg0 ?v ?d)
                                             (arg1 ?v ?b)
                                             (arg2 ?v ?w)
                                             (topicalized ?d)))
    (formulate '((verkaufen-01 ?v)
                                             (doctor ?d)
                                             (woman ?w)
                                             (book ?b)
                                             (arg0 ?v ?d)
                                             (arg1 ?v ?b)
                                             (arg2 ?v ?w)
                                             (topicalized ?d)) :cxn-inventory cxn-inventory)))

;(test-addition-and-substitution-repair-comprehension-german)

(defun test-substitution-repair-2-comprehension-german ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs-german))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "Der Doktor verkauft der Frau die Blumen"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((verkaufen-01 ?v)
                                             (doctor ?d)
                                             (woman ?w)
                                             (flowers ?f)
                                             (arg0 ?v ?d)
                                             (arg1 ?v ?f)
                                             (arg2 ?v ?w)
                                             (topicalized ?d)))
    (test-repair-status 'holistic->item-based--substitution
                        (second (multiple-value-list
                                 (comprehend "Der Doktor verkauft dem Clown die Blumen"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((verkaufen-01 ?v)
                                             (doctor ?d)
                                             (clown ?c)
                                             (flowers ?f)
                                             (arg0 ?v ?d)
                                             (arg1 ?v ?f)
                                             (arg2 ?v ?c)
                                             (topicalized ?d))))))
    (test-repair-status 'holistic->item-based--substitution
                        (third (multiple-value-list
                                 (comprehend "Der Doktor verkauft dem Clown das Buch"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((verkaufen-01 ?v)
                                             (doctor ?d)
                                             (clown ?c)
                                             (book ?b)
                                             (arg0 ?v ?d)
                                             (arg1 ?v ?b)
                                             (arg2 ?v ?c)
                                             (topicalized ?d))))))
    (fourth (multiple-value-list
                                 (comprehend "Der Doktor verkauft der Frau das Buch"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((verkaufen-01 ?v)
                                             (doctor ?d)
                                             (woman ?w)
                                             (book ?b)
                                             (arg0 ?v ?d)
                                             (arg1 ?v ?b)
                                             (arg2 ?v ?w)
                                             (topicalized ?d)))))
    (fifth (multiple-value-list
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
                                             (topicalized ?d)))))
    (sixth (multiple-value-list
                                 (comprehend "Die Frau verkauft dem Doktor die roten Blumen"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((verkaufen-01 ?v)
                                             (doctor ?d)
                                             (woman ?w)
                                             (flowers ?f)
                                             (roten ?r)
                                             (mod ?f ?r)
                                             (arg0 ?v ?w)
                                             (arg1 ?v ?f)
                                             (arg2 ?v ?d)
                                             (topicalized ?w)))))
    (seventh (multiple-value-list
                                 (comprehend "Der Clown verkauft dem Doktor die roten Blumen"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((verkaufen-01 ?v)
                                             (doctor ?d)
                                             (clown ?c)
                                             (flowers ?f)
                                             (roten ?r)
                                             (mod ?f ?r)
                                             (arg0 ?v ?c)
                                             (arg1 ?v ?f)
                                             (arg2 ?v ?d)
                                             (topicalized ?c)))))
    (eighth (multiple-value-list
                                 (comprehend "Der Clown verkauft der Frau das neue Buch"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((verkaufen-01 ?v)
                                             (clown ?c)
                                             (woman ?w)
                                             (book ?b)
                                             (new ?n)
                                             (mod ?b ?n)
                                             (arg0 ?v ?c)
                                             (arg1 ?v ?b)
                                             (arg2 ?v ?w)
                                             (topicalized ?c)))))
    (ninth (multiple-value-list
                                 (comprehend "Der Doktor verkauft dem Clown das neue Buch"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((verkaufen-01 ?v)
                                             (doctor ?d)
                                             (clown ?c)
                                             (new ?n)
                                             (book ?b)
                                             (mod ?b ?n)
                                             (arg0 ?v ?d)
                                             (arg1 ?v ?b)
                                             (arg2 ?v ?c)
                                             (topicalized ?d)))))

    
    ))



;(test-substitution-repair-2-comprehension-german)   we obtain x verkauft y z




(defun run-tests-corpora()
  (test-addition-repair-comprehension-german)
  (test-addition-and-substitution-repair-comprehension-german)
  (test-substitution-repair-comprehension-german))

;(run-tests-corpora)