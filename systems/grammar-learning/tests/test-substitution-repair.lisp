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
                            (:de-render-mode . :de-render-string-meets-no-punct)
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
                    :gold-standard-meaning '((get-context ?source-1)
                                             (filter ?target-66128 ?target-2 ?size-2)
                                             (unique ?source-10 ?target-66128)
                                             (bind color-category ?color-12 purple)
                                             (filter ?target-1 ?source-1 ?shape-8)
                                             (bind shape-category ?shape-8 thing)
                                             (bind attribute-category ?attribute-2 shape)
                                             (filter ?target-2 ?target-1 ?color-12)
                                             (bind size-category ?size-2 small)
                                             (query ?target-8 ?source-10 ?attribute-2)))
      (determine-communicative-success cipn))


    (comprehend "The tiny blue object is what shape?"
                :cxn-inventory cxn-inventory
                :gold-standard-meaning '((get-context ?source-1)
                                         (filter ?target-93243 ?target-2 ?size-2)
                                         (unique ?source-10 ?target-93243)
                                         (bind size-category ?size-2 small)
                                         (filter ?target-2 ?target-1 ?color-6)
                                         (bind shape-category ?shape-8 thing)
                                         (filter ?target-1 ?source-1 ?shape-8)
                                         (bind color-category ?color-6 blue)
                                         (bind attribute-category ?attribute-2 shape)
                                         (query ?target-8 ?source-10 ?attribute-2))))
  (add-element (make-html (categorial-network (grammar (first (agents *experiment*)))))))


(defun test-reordered-form-substitution-repair-comprehension ()
  "This test demonstrates the problem of synonymy. Small and tiny, thing and shape are synonymous, so they are not part of the holistic chunk, but remain part of the item-based cxn"
  (defparameter *experiment* (set-up-cxn-inventory-and-repairs))
  (let* (
         (cxn-inventory (grammar (first (agents *experiment*)))))
    
    ;; enable learning
    (enable-learning cxn-inventory)
    (multiple-value-bind (meaning cipn)
        (comprehend "What shape is the small purple thing?"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((get-context ?source-1)
                                             (filter ?target-66128 ?target-2 ?size-2)
                                             (unique ?source-10 ?target-66128)
                                             (bind color-category ?color-12 purple)
                                             (filter ?target-1 ?source-1 ?shape-8)
                                             (bind shape-category ?shape-8 thing)
                                             (bind attribute-category ?attribute-2 shape)
                                             (filter ?target-2 ?target-1 ?color-12)
                                             (bind size-category ?size-2 small)
                                             (query ?target-8 ?source-10 ?attribute-2)))
      (determine-communicative-success cipn))


    (comprehend "The tiny blue object is what shape?"
                :cxn-inventory cxn-inventory
                :gold-standard-meaning '((get-context ?source-1)
                                         (filter ?target-93243 ?target-2 ?size-2)
                                         (unique ?source-10 ?target-93243)
                                         (bind size-category ?size-2 small)
                                         (filter ?target-2 ?target-1 ?color-6)
                                         (bind shape-category ?shape-8 thing)
                                         (filter ?target-1 ?source-1 ?shape-8)
                                         (bind color-category ?color-6 blue)
                                         (bind attribute-category ?attribute-2 shape)
                                         (query ?target-8 ?source-10 ?attribute-2))))
  (add-element (make-html (categorial-network (grammar (first (agents *experiment*)))))))

(defun run-substitution-tests ()
  (test-substitution-repair-comprehension) ;ok
  (test-substitution-repair-comprehension-right) ;includes query!
  (test-substitution-repair-comprehension-multi-diff) ;should be holophrase
  (test-double-substitution-repair-comprehension) ;ok
  (test-double-discontinuous-substitution-repair-comprehension) ;should be holophrase
  (test-triple-substitution-repair-comprehension) ;ok
  (test-reordered-form-substitution-repair-comprehension) ;ok
  )
  


;(run-substitution-tests)

;*experiment*

#| ISSUES
todo:
'what shape is the X' vs 'the X is what shape', tries to apply 'what shape is the X', but why not just learn 'the X is what shape?', this will always work.

test eens een langere en kortere

deftest invoeren, zie test-construction-
|#



