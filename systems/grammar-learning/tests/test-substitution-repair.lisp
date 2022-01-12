(ql:quickload :clevr-grammar-learning)
(in-package :clevr-grammar-learning)



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
  (make-instance 'clevr-grammar-learning-experiment
                 :entries '((:observation-sample-mode . :debug) ;; random or sequential
                            (:determine-interacting-agents-mode . :corpus-learner)
                            (:remove-cxn-on-lower-bound . nil)
                            (:learner-th-connected-mode . :neighbours))))


(defun test-substitution-repair-comprehension ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs))
         (cxn-inventory (grammar (first (agents experiment)))))
    
    ;; enable learning
    (enable-learning cxn-inventory)
    (multiple-value-bind (meaning cipn)
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
    (determine-communicative-success cipn))


    (comprehend "The large metallic object is what shape?"
                :cxn-inventory cxn-inventory
                :gold-standard-meaning '((get-context ?source-1)
                                         (filter ?target-40315 ?target-2 ?size-4)
                                         (unique ?source-10 ?target-40315)
                                         (bind material-category ?material-4 metal)
                                         (filter ?target-1 ?source-1 ?shape-8)
                                         (bind attribute-category ?attribute-2 shape)
                                         (bind shape-category ?shape-8 thing)
                                         (filter ?target-2 ?target-1 ?material-4)
                                         (bind size-category ?size-4 large)
                                         (query ?target-8 ?source-10 ?attribute-2)))

    ;; disable learning
    (disable-learning cxn-inventory)
    
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

    

    (comprehend "The large metallic object is what shape?"
                :cxn-inventory cxn-inventory
                :gold-standard-meaning '((get-context ?source-1)
                                         (filter ?target-40315 ?target-2 ?size-4)
                                         (unique ?source-10 ?target-40315)
                                         (bind material-category ?material-4 metal)
                                         (filter ?target-1 ?source-1 ?shape-8)
                                         (bind attribute-category ?attribute-2 shape)
                                         (bind shape-category ?shape-8 thing)
                                         (filter ?target-2 ?target-1 ?material-4)
                                         (bind size-category ?size-4 large)
                                         (query ?target-8 ?source-10 ?attribute-2)))))


(defun test-substitution-repair-production ()
  (defparameter *inventory* (grammar (first (agents (set-up-cxn-inventory-and-repairs)))))

    
    ;; enable learning
    (enable-learning *inventory*)
    
    (comprehend "The large gray object is what shape?"
                :cxn-inventory *inventory*
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


    (comprehend "The large metallic object is what shape?"
                :cxn-inventory *inventory*
                :gold-standard-meaning '((get-context ?source-1)
                                         (filter ?target-40315 ?target-2 ?size-4)
                                         (unique ?source-10 ?target-40315)
                                         (bind material-category ?material-4 metal)
                                         (filter ?target-1 ?source-1 ?shape-8)
                                         (bind attribute-category ?attribute-2 shape)
                                         (bind shape-category ?shape-8 thing)
                                         (filter ?target-2 ?target-1 ?material-4)
                                         (bind size-category ?size-4 large)
                                         (query ?target-8 ?source-10 ?attribute-2)))

    ;; disable learning
    (disable-learning *inventory*)

    (formulate '((get-context ?source-1)
                 (filter ?target-40315 ?target-2 ?size-4)
                 (unique ?source-10 ?target-40315)
                 (bind material-category ?material-4 metal)
                 (filter ?target-1 ?source-1 ?shape-8)
                 (bind attribute-category ?attribute-2 shape)
                 (bind shape-category ?shape-8 thing)
                 (filter ?target-2 ?target-1 ?material-4)
                 (bind size-category ?size-4 large)
                 (query ?target-8 ?source-10 ?attribute-2))
               :cxn-inventory *inventory*
               :gold-standard-utterance "the large metallic object is what shape")
    

    (formulate '((get-context ?source-1)
                 (filter ?target-39552 ?target-2 ?size-4)
                 (unique ?source-10 ?target-39552)
                 (bind color-category ?color-2 gray)
                 (filter ?target-1 ?source-1 ?shape-8)
                 (bind attribute-category ?attribute-2 shape)
                 (bind shape-category ?shape-8 thing)
                 (filter ?target-2 ?target-1 ?color-2)
                 (bind size-category ?size-4 large)
                 (query ?target-8 ?source-10 ?attribute-2))
               :cxn-inventory *inventory*
               :gold-standard-utterance "the large gray object is what shape"))


(defun run-tests ()
  (test-substitution-repair-comprehension)
  (test-substitution-repair-production)
  )



;(run-tests)

