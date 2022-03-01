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


(defun disable-learning (grammar)
  (set-configuration grammar :update-categorial-links nil)
  (set-configuration grammar :use-meta-layer nil)
  (set-configuration grammar :consolidate-repairs nil))


(defun enable-learning (grammar)
  (set-configuration grammar :update-categorial-links t)
  (set-configuration grammar :use-meta-layer t)
  (set-configuration grammar :consolidate-repairs t))

(defun set-up-cxn-inventory-and-repairs ()
  (wi::reset)
  (notify reset-monitors)
  (make-instance 'grammar-learning-experiment
                 :entries '((:observation-sample-mode . :debug) ;; random or sequential
                            (:determine-interacting-agents-mode . :corpus-learner)
                            (:de-render-mode . :de-render-string-meets-no-punct)
                            (:remove-cxn-on-lower-bound . nil)
                            (:learner-th-connected-mode . :neighbours))))

(defun test-addition-repair-comprehension ()
  (defparameter *experiment* (set-up-cxn-inventory-and-repairs))
  (let* (
         (cxn-inventory (grammar (first (agents *experiment*)))))
    
    ;; enable learning
    (enable-learning cxn-inventory)
    (multiple-value-bind (meaning cipn)
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
        
    (determine-communicative-success cipn))


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
                                             (query ?target-8 ?source-10 ?attribute-2))))
  (add-element (make-html (categorial-network (grammar (first (agents *experiment*)))))))



(defun test-double-addition-repair-comprehension ()
  (defparameter *experiment* (set-up-cxn-inventory-and-repairs))
  (let* (
         (cxn-inventory (grammar (first (agents *experiment*)))))
    
    ;; enable learning
    (enable-learning cxn-inventory)
    (multiple-value-bind (meaning cipn)
        (comprehend "What is the shape of the large thing?"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((get-context ?source-1)
                                             (filter ?target-2 ?target-1 ?size-4)
                                             (unique ?target-object-1 ?target-2)
                                             (bind attribute-category ?attribute-2 shape)
                                             (bind shape-category ?shape-8 thing)
                                             (filter ?target-1 ?source-1 ?shape-8)
                                             (bind size-category ?size-4 large)
                                             (query ?target-4 ?target-object-1 ?attribute-2)))
        
    (determine-communicative-success cipn))


    (comprehend "What is the shape of the large gray shiny thing?"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((get-context ?source-1)
                                             (filter ?target-33324 ?target-33323 ?size-4)
                                             (unique ?target-object-1 ?target-33324)
                                             (bind color-category ?color-2 gray)
                                             (filter ?target-2 ?target-1 ?material-4)
                                             (bind material-category ?material-4 metal)
                                             (filter ?target-1 ?source-1 ?shape-8)
                                             (bind shape-category ?shape-8 thing)
                                             (bind attribute-category ?attribute-2 shape)
                                             (filter ?target-33323 ?target-2 ?color-2)
                                             (bind size-category ?size-4 large)
                                             (query ?target-4 ?target-object-1 ?attribute-2))))
  (add-element (make-html (categorial-network (grammar (first (agents *experiment*)))))))



(defun run-addition-tests ()
  (test-addition-repair-comprehension)
  (test-double-addition-repair-comprehension)
  )


;(run-addition-tests)

