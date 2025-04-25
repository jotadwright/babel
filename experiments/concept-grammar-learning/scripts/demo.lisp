;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           ;;
;; Script for running a DEMO ;;
;;                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ql:quickload :clg)
(in-package :clg)

;; -----------------------------
;; + Experiment Configurations +
;; -----------------------------

;; Finding the data
(progn
  (define-configuration-default-value :challenge-files-root
                                      (merge-pathnames
                                       (make-pathname :directory '(:relative "CLEVR-intention-reading-data" "val"))
                                       cl-user:*babel-corpora*))
  (define-configuration-default-value :challenge-1-files
                                      (make-pathname :directory '(:relative "stage-1")
                                                     :name :wild :type "lisp"))
  (define-configuration-default-value :challenge-2-files
                                      (make-pathname :directory '(:relative "stage-2")
                                                     :name :wild :type "lisp"))
  (define-configuration-default-value :challenge-3-files
                                      (make-pathname :directory '(:relative "stage-3")
                                                     :name :wild :type "lisp"))
  (define-configuration-default-value :questions-per-challenge 5000)
  (define-configuration-default-value :scenes-per-question 20)
  (define-configuration-default-value :question-sample-mode :first) ; random or first or all
  (define-configuration-default-value :clevr-world-data-sets '("val"))

  ;; Strategies and scores
  (define-configuration-default-value :initial-cxn-score 0.5)
  (define-configuration-default-value :initial-chunk-score 0.5)
  (define-configuration-default-value :initial-th-link-weight 0.1)

  (define-configuration-default-value :cxn-incf-score 0.1)
  (define-configuration-default-value :cxn-decf-score 0.4)
  (define-configuration-default-value :cxn-inhibit-score 0.1)
  (define-configuration-default-value :chunk-incf-score 0.1)
  (define-configuration-default-value :chunk-decf-score 0.1)

  (define-configuration-default-value :alignment-strategy :lateral-inhibition)
  (define-configuration-default-value :determine-interacting-agents-mode :default)
  (define-configuration-default-value :tutor-sample-mode :deterministic) ; :random or :debug or :smart
  (define-configuration-default-value :learner-cxn-supplier :hashed-and-scored)
  (define-configuration-default-value :composer-strategy :store-past-scenes)
  (define-configuration-default-value :composer-past-scenes-window 100)
  (define-configuration-default-value :remove-cxn-on-lower-bound t)
  (define-configuration-default-value :composer-force-shape-category nil)
  (define-configuration-default-value :th-link-repair-mode-comprehension :no-path-required)
  (define-configuration-default-value :th-link-repair-mode-formulation :path-required)

  ;; Autotelic principle
  (define-configuration-default-value :current-challenge-level 1)
  (define-configuration-default-value :max-challenge-level 3)
  (define-configuration-default-value :evaluation-window-size 1000)
  (define-configuration-default-value :confidence-threshold 1.00)

  ;; Hybrid or symbolic primitives
  (define-configuration-default-value :primitives :symbolic) ; :symbolic or hybrid

  ;; Misc
  (define-configuration-default-value :dot-interval 100)
  (define-configuration-default-value :hide-type-hierarchy t)
  )

(defparameter *configuration* (make-configuration
                               :entries '((:determine-interacting-agents-mode . :tutor-learner)
                                          (:question-sample-mode . :all)
                                          ;(:questions-per-challenge . 1000)
                                          (:scenes-per-question . 50)
                                          (:confidence-threshold . 1.1)
                                          (:tutor-sample-mode . :deterministic) ;; or :random
                                          (:cxn-decf-score . 0.4)
                                          (:cxn-inhibit-score . 0.1)
                                          (:primitives . :symbolic)
                                          (:learner-cxn-supplier . :hashed-and-scored)
                                          (:alignment-strategy . :lateral-inhibition)
                                          (:hide-type-hierarchy . nil)
                                          (:remove-cxn-on-lower-bound . t)
                                          (:composer-strategy . :store-past-scenes)
                                          (:th-link-repair-mode-comprehension . :no-path-required)
                                          (:th-link-repair-mode-formulation . :path-required))))

(progn
  (format t "~% Starting a new experiment.~%")
  ;; reset the web interface
  (wi::reset)
  ;; deactivate all monitors (as a sanity check)
  (monitors::notify reset-monitors)
  ;; instantiate an experiment
  (defparameter *experiment* (make-instance 'clevr-learning-experiment :configuration *configuration*)))


;; Option 1: run experiment with real-time plotting (using gnuplot)
(progn
  ;; reset monitors
  (deactivate-all-monitors)

  ;; activate monitors
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor display-metrics)

  (run-series *experiment* 2500))


;; Option 2: run experiment with real-time tracing in the web-interface
(progn
  (wi::reset)
  ;; reset monitors
  (deactivate-all-monitors)

  ;; activate monitors
  (activate-monitor trace-fcg)
  (activate-monitor trace-irl)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor trace-interactions-in-wi)
  ;(activate-monitor trace-tasks-and-processes)

  (run-interaction *experiment*)
  )

(setf inventory (cl-store::restore (babel-pathname
                                     :directory `("experiments" 
                                                  "concept-emergence2" 
                                                  "storage"
                                                  "cle4-grammar")
                                     :name (format nil "inventory")
                                     :type "store")))

