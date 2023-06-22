
(ql:quickload :clevr-learning)
(in-package :cgl)

(let ((experiment-names
       '(th-link-mixed-mode-smart-tutor
         th-link-mixed-mode-random-tutor
         th-link-mixed-mode-listen-only-smart-tutor
         th-link-mixed-mode-listen-only-random-tutor)))
  (run-parallel-batch-for-different-configurations 
   :asdf-system "clevr-learning"
   :package "clevr-learning"
   :experiment-class "clevr-learning-experiment"
   :number-of-interactions 250000
   :number-of-series 10
   :monitors (append (get-all-lisp-monitors)
                     (get-all-export-monitors))
   :configurations '(
                     (th-link-mixed-mode-smart-tutor
                      ((:tutor-sample-mode . :smart)
                       (:composer-strategy . :store-past-scenes)
                       (:composer-past-scenes-window . 50)
                       (:th-link-repair-mode-comprehension . :no-path-required)
                       (:th-link-repair-mode-formulation . :path-required)
                       (:determine-interacting-agents-mode . :default)))
                     (th-link-mixed-mode-random-tutor
                      ((:tutor-sample-mode . :random)
                       (:composer-strategy . :store-past-scenes)
                       (:composer-past-scenes-window . 50)
                       (:th-link-repair-mode-comprehension . :no-path-required)
                       (:th-link-repair-mode-formulation . :path-required)
                       (:determine-interacting-agents-mode . :default)))
                     (th-link-mixed-mode-listen-only-smart-tutor
                      ((:tutor-sample-mode . :smart)
                       (:composer-strategy . :store-past-scenes)
                       (:composer-past-scenes-window . 50)
                       (:th-link-repair-mode-comprehension . :no-path-required)
                       (:th-link-repair-mode-formulation . :path-required)
                       (:determine-interacting-agents-mode . :tutor-learner)))
                     (th-link-mixed-mode-listen-only-random-tutor
                      ((:tutor-sample-mode . :random)
                       (:composer-strategy . :store-past-scenes)
                       (:composer-past-scenes-window . 50)
                       (:th-link-repair-mode-comprehension . :no-path-required)
                       (:th-link-repair-mode-formulation . :path-required)
                       (:determine-interacting-agents-mode . :tutor-learner)))
                     )
   :shared-configuration '((:question-sample-mode . :all)    
                           (:scenes-per-question . 100)
                           (:confidence-threshold . 1.1)
                           (:cxn-decf-score . 0.4)
                           (:cxn-inhibit-score . 0.1)
                           (:primitives . :symbolic)
                           (:learner-cxn-supplier . :hashed-and-scored)
                           (:alignment-strategy . :lateral-inhibition)
                           (:hide-type-hierarchy . t)
                           (:composer-force-shape-category . nil)
                           (:remove-cxn-on-lower-bound . t))
   :output-dir (babel-pathname :directory '("experiments" "clevr-learning" "raw-data"))
   :heap-size 18000)

  ;; create all plots for each experiment separately
  (loop for experiment in experiment-names
        do (create-all-graphs-for-single-experiment experiment))

  ;; create all plots comparing experiments
  ;; (success and grammar size)
  ;(create-all-graphs-comparing-experiment experiment-names)

)

  