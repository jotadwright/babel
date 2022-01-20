(ql:quickload :grammar-learning)
(in-package :grammar-learning)



;; full logging except trace-fcg
(progn
  (deactivate-all-monitors)
  (activate-monitor display-metrics)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor summarize-results-after-n-interactions)
  (activate-monitor show-type-hierarchy-after-n-interactions)
  (activate-monitor trace-interactions-in-wi))

;; minimal logging after 100 interactions with type hierarchy
(progn
  (deactivate-all-monitors)
  (activate-monitor display-metrics)
  (activate-monitor summarize-results-after-n-interactions)
  (activate-monitor show-type-hierarchy-after-n-interactions)
  (activate-monitor print-a-dot-for-each-interaction))

;; minimal logging after 100 interactions
(progn
  (deactivate-all-monitors)
  (activate-monitor display-metrics)
  (activate-monitor summarize-results-after-n-interactions)
  (activate-monitor print-a-dot-for-each-interaction))

;; full logging
(progn
  (deactivate-all-monitors)
  (activate-monitor display-metrics)
  (activate-monitor trace-fcg)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor summarize-results-after-n-interactions)
  (activate-monitor show-type-hierarchy-after-n-interactions)
  (activate-monitor trace-interactions-in-wi))


(defun disable-learning (grammar)
  (set-configuration grammar :update-th-links nil)
  (set-configuration grammar :use-meta-layer nil)
  (set-configuration grammar :consolidate-repairs nil))


(defun enable-learning (grammar)
  (set-configuration grammar :update-th-links t)
  (set-configuration grammar :use-meta-layer t)
  (set-configuration grammar :consolidate-repairs t))



(defun set-up-cxn-inventory-and-repairs ()
  (wi::reset)
  (notify reset-monitors)
  (make-instance 'grammar-learning-experiment
                 :entries '((:observation-sample-mode . :debug) ;; random or sequential
                            (:determine-interacting-agents-mode . :corpus-learner)
                            (:remove-cxn-on-lower-bound . nil)
                            (:learner-th-connected-mode . :neighbours))))


(defun test-add-th-links-repair-comprehension ()
  (defparameter *inventory* (grammar (first (agents (set-up-cxn-inventory-and-repairs)))))

    
    ;; enable learning
    (enable-learning *inventory*)

    (comprehend "What is the size of the red cube?"
                :cxn-inventory *inventory*
                :gold-standard-meaning '((get-context ?source-1)
                                         (filter ?target-2 ?target-1 ?color-4)
                                         (unique ?target-object-1 ?target-2)
                                         (bind shape-category ?shape-2 cube)
                                         (bind attribute-category ?attribute-6 size)
                                         (filter ?target-1 ?source-1 ?shape-2)
                                         (bind color-category ?color-4 red)
                                         (query ?target-4 ?target-object-1 ?attribute-6)))
    
    (comprehend "What is the size of the blue cube?"
                :cxn-inventory *inventory*
                :gold-standard-meaning '((get-context ?source-1)
                                         (filter ?target-2 ?target-1 ?color-6)
                                         (unique ?target-object-1 ?target-2)
                                         (bind attribute-category ?attribute-6 size)
                                         (bind shape-category ?shape-2 cube)
                                         (filter ?target-1 ?source-1 ?shape-2)
                                         (bind color-category ?color-6 blue)
                                         (query ?target-4 ?target-object-1 ?attribute-6)))
    (comprehend "What is the size of the yellow cube?"
                :cxn-inventory *inventory*
                :gold-standard-meaning '((get-context ?source-1)
                                         (filter ?target-2 ?target-1 ?color-16)
                                         (unique ?target-object-1 ?target-2)
                                         (bind shape-category ?shape-2 cube)
                                         (bind attribute-category ?attribute-6 size)
                                         (filter ?target-1 ?source-1 ?shape-2)
                                         (bind color-category ?color-16 yellow)
                                         (query ?target-4 ?target-object-1 ?attribute-6)))
    (comprehend "What is the size of the green sphere?"
                :cxn-inventory *inventory*
                :gold-standard-meaning '((get-context ?source-1)
                                         (filter ?target-2 ?target-1 ?color-8)
                                         (unique ?target-object-1 ?target-2)
                                         (bind shape-category ?shape-4 sphere)
                                         (bind attribute-category ?attribute-6 size)
                                         (filter ?target-1 ?source-1 ?shape-4)
                                         (bind color-category ?color-8 green)
                                         (query ?target-4 ?target-object-1 ?attribute-6)))
    
    (comprehend "What is the size of the purple sphere?"
                :cxn-inventory *inventory*
                :gold-standard-meaning '((get-context ?source-1)
                                         (filter ?target-2 ?target-1 ?color-8)
                                         (unique ?target-object-1 ?target-2)
                                         (bind shape-category ?shape-4 sphere)
                                         (bind attribute-category ?attribute-6 size)
                                         (filter ?target-1 ?source-1 ?shape-4)
                                         (bind color-category ?color-8 purple)
                                         (query ?target-4 ?target-object-1 ?attribute-6)))
    (comprehend "What is the size of the purple cube?"
                :cxn-inventory *inventory*
                :gold-standard-meaning '((get-context ?source-1)
                                         (bind attribute-category ?attribute-6 size)
                                         (bind color-category ?color-12 purple)
                                         (filter ?target-1 ?source-1 ?shape-2)
                                         (bind shape-category ?shape-2 cube)
                                         (unique ?target-object-1 ?target-2)
                                         (filter ?target-2 ?target-1 ?color-12)
                                         (query ?target-4 ?target-object-1 ?attribute-6)))
    (comprehend "What is the size of the yellow sphere?"
                :cxn-inventory *inventory*
                :gold-standard-meaning '((get-context ?source-1)
                                         (filter ?target-2 ?target-1 ?color-8)
                                         (unique ?target-object-1 ?target-2)
                                         (bind shape-category ?shape-4 sphere)
                                         (bind attribute-category ?attribute-6 size)
                                         (filter ?target-1 ?source-1 ?shape-4)
                                         (bind color-category ?color-8 yellow)
                                         (query ?target-4 ?target-object-1 ?attribute-6)))
    
    ;; disable learning
    (disable-learning *inventory*)
    (comprehend "What is the size of the yellow sphere?"
                :cxn-inventory *inventory*
                :gold-standard-meaning '((get-context ?source-1)
                                         (filter ?target-2 ?target-1 ?color-8)
                                         (unique ?target-object-1 ?target-2)
                                         (bind shape-category ?shape-4 sphere)
                                         (bind attribute-category ?attribute-6 size)
                                         (filter ?target-1 ?source-1 ?shape-4)
                                         (bind color-category ?color-8 yellow)
                                         (query ?target-4 ?target-object-1 ?attribute-6))))

(defun test-add-th-links-repair-production ()
  (defparameter *inventory* (grammar (first (agents (set-up-cxn-inventory-and-repairs)))))

    
    ;; enable learning
    (enable-learning *inventory*)

    
        (comprehend "What is the size of the red cube?"
                :cxn-inventory *inventory*
                :gold-standard-meaning '((get-context ?source-1)
                                         (filter ?target-2 ?target-1 ?color-4)
                                         (unique ?target-object-1 ?target-2)
                                         (bind shape-category ?shape-2 cube)
                                         (bind attribute-category ?attribute-6 size)
                                         (filter ?target-1 ?source-1 ?shape-2)
                                         (bind color-category ?color-4 red)
                                         (query ?target-4 ?target-object-1 ?attribute-6)))
    
    (comprehend "What is the size of the blue cube?"
                :cxn-inventory *inventory*
                :gold-standard-meaning '((get-context ?source-1)
                                         (filter ?target-2 ?target-1 ?color-6)
                                         (unique ?target-object-1 ?target-2)
                                         (bind attribute-category ?attribute-6 size)
                                         (bind shape-category ?shape-2 cube)
                                         (filter ?target-1 ?source-1 ?shape-2)
                                         (bind color-category ?color-6 blue)
                                         (query ?target-4 ?target-object-1 ?attribute-6)))
    (comprehend "What is the size of the yellow cube?"
                :cxn-inventory *inventory*
                :gold-standard-meaning '((get-context ?source-1)
                                         (filter ?target-2 ?target-1 ?color-16)
                                         (unique ?target-object-1 ?target-2)
                                         (bind shape-category ?shape-2 cube)
                                         (bind attribute-category ?attribute-6 size)
                                         (filter ?target-1 ?source-1 ?shape-2)
                                         (bind color-category ?color-16 yellow)
                                         (query ?target-4 ?target-object-1 ?attribute-6)))
    (comprehend "What is the size of the green sphere?"
                :cxn-inventory *inventory*
                :gold-standard-meaning '((get-context ?source-1)
                                         (filter ?target-2 ?target-1 ?color-8)
                                         (unique ?target-object-1 ?target-2)
                                         (bind shape-category ?shape-4 sphere)
                                         (bind attribute-category ?attribute-6 size)
                                         (filter ?target-1 ?source-1 ?shape-4)
                                         (bind color-category ?color-8 green)
                                         (query ?target-4 ?target-object-1 ?attribute-6)))
    
    (comprehend "What is the size of the purple sphere?"
                :cxn-inventory *inventory*
                :gold-standard-meaning '((get-context ?source-1)
                                         (filter ?target-2 ?target-1 ?color-8)
                                         (unique ?target-object-1 ?target-2)
                                         (bind shape-category ?shape-4 sphere)
                                         (bind attribute-category ?attribute-6 size)
                                         (filter ?target-1 ?source-1 ?shape-4)
                                         (bind color-category ?color-8 purple)
                                         (query ?target-4 ?target-object-1 ?attribute-6)))
    (comprehend "What is the size of the purple cube?"
                :cxn-inventory *inventory*
                :gold-standard-meaning '((get-context ?source-1)
                                         (bind attribute-category ?attribute-6 size)
                                         (bind color-category ?color-12 purple)
                                         (filter ?target-1 ?source-1 ?shape-2)
                                         (bind shape-category ?shape-2 cube)
                                         (unique ?target-object-1 ?target-2)
                                         (filter ?target-2 ?target-1 ?color-12)
                                         (query ?target-4 ?target-object-1 ?attribute-6)))
    (comprehend "What is the size of the yellow sphere?"
                :cxn-inventory *inventory*
                :gold-standard-meaning '((get-context ?source-1)
                                         (filter ?target-2 ?target-1 ?color-8)
                                         (unique ?target-object-1 ?target-2)
                                         (bind shape-category ?shape-4 sphere)
                                         (bind attribute-category ?attribute-6 size)
                                         (filter ?target-1 ?source-1 ?shape-4)
                                         (bind color-category ?color-8 yellow)
                                         (query ?target-4 ?target-object-1 ?attribute-6)))
    
   

    ;; disable learning
    (disable-learning *inventory*)


    (formulate '((get-context ?source-1)
                                         (filter ?target-2 ?target-1 ?color-8)
                                         (unique ?target-object-1 ?target-2)
                                         (bind shape-category ?shape-4 sphere)
                                         (bind attribute-category ?attribute-6 size)
                                         (filter ?target-1 ?source-1 ?shape-4)
                                         (bind color-category ?color-8 yellow)
                                         (query ?target-4 ?target-object-1 ?attribute-6))
               :cxn-inventory *inventory*
               :gold-standard-utterance "what is the size of the yellow sphere"))



(defun run-tests ()
  (test-add-th-links-repair-comprehension) ;; second merge fails for yellow??
  (test-add-th-links-repair-production)
  )



;(run-tests)

