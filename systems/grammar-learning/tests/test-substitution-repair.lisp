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
                            (:remove-cxn-on-lower-bound . nil)
                            (:learner-th-connected-mode . :neighbours))))

(defun test-substitution-repair-comprehension ()
  (defparameter *experiment* (set-up-cxn-inventory-and-repairs))
  (let* (
         (cxn-inventory (grammar (first (agents *experiment*)))))
    
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


    (comprehend "The large yellow object is what shape?"
                :cxn-inventory cxn-inventory
                :gold-standard-meaning '((get-context ?source-1)
                                         (filter ?target-14197 ?target-2 ?size-2)
                                         (unique ?source-10 ?target-14197)
                                         (bind color-category ?color-16 yellow)
                                         (filter ?target-1 ?source-1 ?shape-8)
                                         (bind attribute-category ?attribute-2 shape)
                                         (bind shape-category ?shape-8 thing)
                                         (filter ?target-2 ?target-1 ?color-16)
                                         (bind size-category ?size-2 large)
                                         (query ?target-8 ?source-10 ?attribute-2))))
  (add-element (make-html (categorial-network (grammar (first (agents *experiment*)))))))

(defun test-substitution-repair-comprehension-right ()
  (defparameter *experiment* (set-up-cxn-inventory-and-repairs))
  (let* (
         (cxn-inventory (grammar (first (agents *experiment*)))))
    
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


    (comprehend "The large gray object is what material?"
                :cxn-inventory cxn-inventory
                :gold-standard-meaning '((get-context ?source-1)
                                         (filter ?target-14197 ?target-2 ?size-2)
                                         (unique ?source-10 ?target-14197)
                                         (bind color-category ?color-16 gray)
                                         (filter ?target-1 ?source-1 ?shape-8)
                                         (bind attribute-category ?attribute-2 material)
                                         (bind shape-category ?shape-8 thing)
                                         (filter ?target-2 ?target-1 ?color-16)
                                         (bind size-category ?size-2 large)
                                         (query ?target-8 ?source-10 ?attribute-2))))
  (add-element (make-html (categorial-network (grammar (first (agents *experiment*)))))))

(defun test-substitution-repair-comprehension-multi-diff ()
  (defparameter *experiment* (set-up-cxn-inventory-and-repairs))
  (let* (
         (cxn-inventory (grammar (first (agents *experiment*)))))
    
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


    (comprehend "The large yellow object is what material?"
                :cxn-inventory cxn-inventory
                :gold-standard-meaning '((get-context ?source-1)
                                         (filter ?target-14197 ?target-2 ?size-2)
                                         (unique ?source-10 ?target-14197)
                                         (bind color-category ?color-16 yellow)
                                         (filter ?target-1 ?source-1 ?shape-8)
                                         (bind attribute-category ?attribute-2 material)
                                         (bind shape-category ?shape-8 thing)
                                         (filter ?target-2 ?target-1 ?color-16)
                                         (bind size-category ?size-2 large)
                                         (query ?target-8 ?source-10 ?attribute-2))))
  (add-element (make-html (categorial-network (grammar (first (agents *experiment*)))))))

(defun test-double-substitution-repair-comprehension ()
  (defparameter *experiment* (set-up-cxn-inventory-and-repairs))
  (let* (
         (cxn-inventory (grammar (first (agents *experiment*)))))
    
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


    (comprehend "The tiny yellow object is what shape?"
                :cxn-inventory cxn-inventory
                :gold-standard-meaning '((get-context ?source-1)
                                         (filter ?target-14197 ?target-2 ?size-2)
                                         (unique ?source-10 ?target-14197)
                                         (bind color-category ?color-16 yellow)
                                         (filter ?target-1 ?source-1 ?shape-8)
                                         (bind attribute-category ?attribute-2 shape)
                                         (bind shape-category ?shape-8 thing)
                                         (filter ?target-2 ?target-1 ?color-16)
                                         (bind size-category ?size-2 small)
                                         (query ?target-8 ?source-10 ?attribute-2))))
  (add-element (make-html (categorial-network (grammar (first (agents *experiment*)))))))

(defun test-double-discontinuous-substitution-repair-comprehension ()
  (defparameter *experiment* (set-up-cxn-inventory-and-repairs))
  (let* (
         (cxn-inventory (grammar (first (agents *experiment*)))))
    
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


    (comprehend "The tiny yellow object is what material?"
                :cxn-inventory cxn-inventory
                :gold-standard-meaning '((get-context ?source-1)
                                         (filter ?target-14197 ?target-2 ?size-2)
                                         (unique ?source-10 ?target-14197)
                                         (bind color-category ?color-16 yellow)
                                         (filter ?target-1 ?source-1 ?shape-8)
                                         (bind attribute-category ?attribute-2 material)
                                         (bind shape-category ?shape-8 thing)
                                         (filter ?target-2 ?target-1 ?color-16)
                                         (bind size-category ?size-2 small)
                                         (query ?target-8 ?source-10 ?attribute-2))))
  (add-element (make-html (categorial-network (grammar (first (agents *experiment*)))))))

(defun test-triple-substitution-repair-comprehension ()
  "This test demonstrates the problem of synonymy. Small and tiny, thing and shape are synonymous, so they are not part of the holistic chunk, but remain part of the item-based cxn"
  (defparameter *experiment* (set-up-cxn-inventory-and-repairs))
  (let* (
         (cxn-inventory (grammar (first (agents *experiment*)))))
    
    ;; enable learning
    (enable-learning cxn-inventory)
    (multiple-value-bind (meaning cipn)
        (comprehend "The small purple thing is what shape?"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((CLEVR-WORLD:GET-CONTEXT GRAMMAR-LEARNING::?SOURCE-1) (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-82186 GRAMMAR-LEARNING::?TARGET-2 GRAMMAR-LEARNING::?SIZE-2) (CLEVR-WORLD:UNIQUE GRAMMAR-LEARNING::?SOURCE-10 GRAMMAR-LEARNING::?TARGET-82186) (UTILS:BIND CLEVR-WORLD:COLOR-CATEGORY GRAMMAR-LEARNING::?COLOR-12 CLEVR-WORLD:PURPLE) (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-1 GRAMMAR-LEARNING::?SOURCE-1 GRAMMAR-LEARNING::?SHAPE-8) (UTILS:BIND CLEVR-WORLD:ATTRIBUTE-CATEGORY GRAMMAR-LEARNING::?ATTRIBUTE-2 CLEVR-WORLD:SHAPE) (UTILS:BIND CLEVR-WORLD:SHAPE-CATEGORY GRAMMAR-LEARNING::?SHAPE-8 CLEVR-WORLD:THING) (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-2 GRAMMAR-LEARNING::?TARGET-1 GRAMMAR-LEARNING::?COLOR-12) (UTILS:BIND CLEVR-WORLD:SIZE-CATEGORY GRAMMAR-LEARNING::?SIZE-2 CLEVR-WORLD:SMALL) (CLEVR-WORLD:QUERY GRAMMAR-LEARNING::?TARGET-8 GRAMMAR-LEARNING::?SOURCE-10 GRAMMAR-LEARNING::?ATTRIBUTE-2)))
      (determine-communicative-success cipn))


    (comprehend "The tiny blue object is what shape?"
                :cxn-inventory cxn-inventory
                :gold-standard-meaning '((CLEVR-WORLD:GET-CONTEXT GRAMMAR-LEARNING::?SOURCE-1) (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-81772 GRAMMAR-LEARNING::?TARGET-2 GRAMMAR-LEARNING::?SIZE-2) (CLEVR-WORLD:UNIQUE GRAMMAR-LEARNING::?SOURCE-10 GRAMMAR-LEARNING::?TARGET-81772) (UTILS:BIND CLEVR-WORLD:COLOR-CATEGORY GRAMMAR-LEARNING::?COLOR-6 CLEVR-WORLD:BLUE) (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-1 GRAMMAR-LEARNING::?SOURCE-1 GRAMMAR-LEARNING::?SHAPE-8) (UTILS:BIND CLEVR-WORLD:SHAPE-CATEGORY GRAMMAR-LEARNING::?SHAPE-8 CLEVR-WORLD:THING) (UTILS:BIND CLEVR-WORLD:ATTRIBUTE-CATEGORY GRAMMAR-LEARNING::?ATTRIBUTE-2 CLEVR-WORLD:SHAPE) (CLEVR-WORLD:FILTER GRAMMAR-LEARNING::?TARGET-2 GRAMMAR-LEARNING::?TARGET-1 GRAMMAR-LEARNING::?COLOR-6) (UTILS:BIND CLEVR-WORLD:SIZE-CATEGORY GRAMMAR-LEARNING::?SIZE-2 CLEVR-WORLD:SMALL) (CLEVR-WORLD:QUERY GRAMMAR-LEARNING::?TARGET-8 GRAMMAR-LEARNING::?SOURCE-10 GRAMMAR-LEARNING::?ATTRIBUTE-2))))
  (add-element (make-html (categorial-network (grammar (first (agents *experiment*)))))))


(defun run-substitution-tests ()
  (test-substitution-repair-comprehension)
  (test-substitution-repair-comprehension-right)
  (test-substitution-repair-comprehension-multi-diff)
  (test-double-substitution-repair-comprehension)
  (test-double-discontinuous-substitution-repair-comprehension)
  (test-triple-substitution-repair-comprehension)
  )
  


;(run-substitution-tests)

;*experiment*

#| ISSUES


|#



