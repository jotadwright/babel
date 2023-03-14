(ql:quickload :propbank-grammar)
(in-package :propbank-grammar)

(load (babel-pathname :directory
                      '("grammars" "propbank-grammar" "cleaning-and-evaluation" "parameter-evaluation")
                      :name "parameter-learn-eval" :type "lisp"))

(load (babel-pathname :directory
                      '("grammars" "propbank-grammar" "cleaning-and-evaluation" "parameter-evaluation")
                      :name "simulated-annealing" :type "lisp"))

(load (babel-pathname :directory
                      '("grammars" "propbank-grammar" "cleaning-and-evaluation" "parameter-evaluation")
                      :name "parallel-simulated-annealing" :type "lisp"))


;; initial training config f1-score already calculated

(defun parallel-simulated-annealing-plots (initial-params list-of-combinations &key (test-batch-size 500) (num-threads 2) (temperature 1000) (cooling-rate 0.95) (steps 16) (train-set *train-corpus*) (dev-set *dev-corpus*))
  "Runs simulated annealing in parallel on multiple threads."
  (let* ((num-combinations (length list-of-combinations))
         (num-combinations-per-thread (floor (/ num-combinations num-threads)))
         (combinations-lists (split-list list-of-combinations num-threads))
         (best-score 0.30)
         (f1-scores (list (cons initial-params best-score)))
         (init-temp temperature)
         (best-params initial-params)
         (start-time (get-internal-real-time)))
    (format t "Running simulated annealing in parallel on ~a threads~%" num-threads)
    (format t "Each thread will handle ~a combinations~%" num-combinations-per-thread)
    (format t "Total number of combinations to explore: ~a~%" num-combinations)
    (format t "Starting annealing process...~%")
    (let ((threads (list)))
      (dotimes (i num-threads)
        (let* ((combinations (nth i combinations-lists))
               (thread-nmb (1+ i))
               (thread (bt:make-thread
                        (lambda ()
                          (dolist (score (simulated-annealing-for-par (random-neighbour best-params combinations initial-params) combinations :temperature temperature :cooling-rate cooling-rate :steps steps :train-set train-set :dev-set dev-set :thread-nmb thread-nmb :test-batch-size test-batch-size))
                            (push score f1-scores))))))
          (push thread threads)
          ))
      (mapc #'bt:join-thread threads))
    (let ((sorted-f1-scores (sort f1-scores #'> :key #'cdr)))
      (setf best-score (cdr (car sorted-f1-scores))
            best-params (car (car sorted-f1-scores)))
      (plot-f1-scores (reverse sorted-f1-scores))
      (store-f1-params-for-par sorted-f1-scores temperature cooling-rate steps (length train-set) (length dev-set))
      (let ((total-runtime (/ (- (get-internal-real-time) start-time) internal-time-units-per-second)))
        (store-run-info-par total-runtime init-temp temperature cooling-rate steps (length train-set) (length dev-set) (reverse f1-scores) list-of-combinations num-threads)
        (format t "Total runtime all threads: ~f seconds~%" total-runtime)))
    (format t "All threads finished. Best score: ~a, Best parameters: ~a~%" best-score best-params)
    (list f1-scores)))



;; Activating spacy-api locally
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf nlp-tools::*penelope-host* "http://127.0.0.1:5000")

;; Loading the Propbank annotations (takes a couple of minutes)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-propbank-annotations 'ewt :ignore-stored-data nil) ; *ewt-annotations*
(load-propbank-annotations 'ontonotes :ignore-stored-data nil) ; *ontonotes-annotations*


(defparameter *train-corpus* (shuffle (append (train-split *ontonotes-annotations*)
                                              (train-split *ewt-annotations*))))

(defparameter *dev-corpus* (shuffle (append (dev-split *ontonotes-annotations*)
                                              (dev-split *ewt-annotations*))))


;; Setting the globals
;;;;;;;;;;;;;;;;;;;;;;

(defparameter *training-configuration-all*
  `((:de-render-mode . :de-render-constituents-dependents)
    (:node-tests :check-double-role-assignment)
    (:parse-goal-tests :no-valid-children)
    (:max-nr-of-nodes . 100)

    (:construction-inventory-processor-mode . :heuristic-search)
    (:search-algorithm . :best-first)   
    (:cxn-supplier-mode . :hashed-categorial-network)
    
    (:heuristics
     ((:nr-of-applied-cxns
     :nr-of-units-matched-x2))
     ((:nr-of-applied-cxns
     :nr-of-units-matched))
     :argm-prediction ;; Don't forget to activate the text-to-role-classification server!!!!!
     :edge-weight
     :prefer-local-bindings
     :frequency
     )
    (:heuristic-value-mode . :sum-heuristics-and-parent)
    (:sort-cxns-before-application . nil)

    (:node-expansion-mode . :full-expansion)
    (:hash-mode . :hash-lemma)
    
    (:replace-when-equivalent . nil)
    (:learning-modes
     :core-roles
     ((:argm-leaf
     :argm-pp
     :argm-sbar
     :argm-phrase-with-string)))
    (:excluded-rolesets
     ((:be.01 :be.02 :be.03
     :have.01 :have.02 :have.03 :have.04 :have.05 :have.06 :have.07 :have.08 :have.09 :have.10 :have.11
     :get.03 :get.06 :get.24)))))

; (:excluded-rolesets
;      :be.01 :be.02 :be.03
;      :have.01 :have.02 :have.03 :have.04 :have.05 :have.06 :have.07 :have.08 :have.09 :have.10 :have.11
;      :get.03 :get.06 :get.24)

(defparameter training-configuration-new nil)

(defparameter test-grammar nil)
    
;; Learn and make predictions for a PropBank grammar using simulated annealing to explore the search space
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make a list of parameter combinations
(setf combinations-parameters (all-combinations-parameters '(:HEURISTICS :LEARNING-MODES :EXCLUDED-ROLESETS)))

;; filter that list to only include combinations with certain parameters
(setf filtered-combinations (filter-combinations combinations-parameters
                                         :parameters-to-exclude '((:FREQUENCY :EDGE-WEIGHT :PREFER-LOCAL-BINDINGS) (:nr-of-units-matched-x2 :nr-of-units-matched) (:FREQUENCY :EDGE-WEIGHT) (:FREQUENCY :PREFER-LOCAL-BINDINGS) (:EDGE-WEIGHT :PREFER-LOCAL-BINDINGS))
                                         :parameters-to-include '((:NR-OF-APPLIED-CXNS :CORE-ROLES))))

;; use simulated annealing to explore the search space of the list of combinations. Steps indicate how many configurations it will learn and predict in every thread.
(parallel-simulated-annealing-plots '((:HEURISTICS :NR-OF-APPLIED-CXNS) (:LEARNING-MODES :CORE-ROLES) (:EXCLUDED-ROLESETS)) filtered-combinations :num-threads 8 :steps 8 :test-batch-size 100)


