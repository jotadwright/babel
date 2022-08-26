(ql:quickload :grammar-learning)
(in-package :grammar-learning)



(progn
  (deactivate-all-monitors)
  ;(activate-monitor display-metrics)
  (activate-monitor trace-fcg)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor summarize-results-after-n-interactions)
  (activate-monitor show-type-hierarchy-after-n-interactions)
  (activate-monitor trace-interactions-in-wi))

(progn
  (wi::reset)
  (notify reset-monitors)
  (defparameter *experiment*
    (make-instance 'grammar-learning-experiment
                   :entries `((:observation-sample-mode . :train)
                              (:meaning-representation . :irl)
                              (:de-render-mode . :de-render-string-meets-no-punct)
                              (:corpus-files-root . ,(merge-pathnames
                                     (make-pathname :directory '(:relative "clevr-grammar-learning"))
                                     cl-user:*babel-corpora*))
                              (:corpus-data-file . ,(make-pathname :directory '(:relative "train")
                                                   :name "stage-1" :type "jsonl"))))))

(defparameter *cxn-inventory* (grammar (first (agents *experiment*))))
;(run-interaction *experiment*)

        
(defun run-training ()
  (wi::reset)
  (let ((experiment-name 'training-stage-1))
    (run-experiments `(
                       (,experiment-name
                        ((:determine-interacting-agents-mode . :corpus-learner)
                         (:observation-sample-mode . :train)
                         (:meaning-representation . :irl)
                         (:de-render-mode . :de-render-string-meets-no-punct)
                         (:corpus-files-root . ,(merge-pathnames
                                     (make-pathname :directory '(:relative "clevr-grammar-learning"))
                                     cl-user:*babel-corpora*))
                         (:corpus-data-file . ,(make-pathname :directory '(:relative "train")
                                                   :name "stage-1" :type "jsonl"))
                         (:number-of-epochs . 1)
                         ))
                       )
                     :number-of-interactions 47134
                     :number-of-series 1
                     :monitors (append '("print-a-dot-for-each-interaction"
                                         "summarize-results-after-n-interactions")
                                       (get-all-export-monitors)
                                       (get-all-lisp-monitors)
                                       (get-all-csv-monitors)))))

;(run-training)

#|(defun run-training-stage-2 (stored-grammar)
  ;(wi::reset)
  (let ((experiment-name 'training-stage-2))
    (run-experiments `(
                       (,experiment-name
                        ((:determine-interacting-agents-mode . :corpus-learner)
                         (:observation-sample-mode . :train)
                         (:evaluation-grammar . ,stored-grammar)
                         (:category-linking-mode . :neighbours)
                         (:current-challenge-level . 2)
                         ))
                       )
                     :number-of-interactions 200;408656
                     :number-of-series 1
                     :monitors (append '("print-a-dot-for-each-interaction"
                                         "summarize-results-after-n-interactions")
                                       (get-all-export-monitors)
                                       (get-all-csv-monitors)))))

(defun run-evaluation (stored-grammar)
  (wi::reset)
  (let ((experiment-name 'evaluation))
    (run-experiments `(
                       (,experiment-name
                        ((:determine-interacting-agents-mode . :corpus-learner)
                         (:observation-sample-mode . :evaluation)
                         (:evaluation-grammar . ,stored-grammar)
                         (:category-linking-mode . :path-exists)
                         ))
                       )
                     :number-of-interactions 10043
                     :number-of-series 1
                     :monitors (append '("print-a-dot-for-each-interaction"
                                         "evaluation-after-n-interactions")
                                       (get-all-export-monitors)
                                       (get-all-csv-monitors)))))

(defun run-dev-set (stored-grammar)
  (wi::reset)
  (let ((experiment-name 'development))
    (run-experiments `(
                       (,experiment-name
                        ((:determine-interacting-agents-mode . :corpus-learner)
                         (:observation-sample-mode . :development)
                         (:evaluation-grammar . ,stored-grammar)
                         (:category-linking-mode . :neighbours)
                         ))
                       )
                     :number-of-interactions 10181
                     :number-of-series 1
                     :monitors (append '("print-a-dot-for-each-interaction"
                                         "evaluation-after-n-interactions")
                                       (get-all-export-monitors)
                                       (get-all-csv-monitors)))))|#

#|
(progn
  (activate-monitor trace-fcg)
  (formulate '((get-context ?source-1)
               (query ?target-51 ?target-object-1 ?attribute-15)
               (bind attribute-category ?attribute-15 material)
               (filter ?target-2 ?target-1 ?size-2)
               (unique ?target-object-1 ?target-2)
               (bind shape-category ?shape-2 cube)
               (filter ?target-1 ?source-1 ?shape-2)
               (bind size-category ?size-2 small))
             :gold-standard-utterance "What is the small cube made of?"
             :cxn-inventory *saved-inventory*))

|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; train, load the exported grammar and evaluate it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(activate-monitor trace-fcg)
; (run-training)
; (defparameter *saved-inventory* (cl-store:restore (babel-pathname :directory '("experiments" "clevr-grammar-learning" "raw-data" "training-stage-1") :name "grammar" :type "store")))
;(run-training-stage-2 *saved-inventory*)
; (defparameter *saved-inventory* (cl-store:restore (babel-pathname :directory '("experiments" "clevr-grammar-learning" "raw-data" "mac-pro-stage-1") :name "cxn-inventory-training-latest" :type "store")))
; (run-evaluation *saved-inventory*)
; (run-dev-set *saved-inventory*)
; (defparameter *error-file* (cl-store:restore (babel-pathname :directory '("experiments" "clevr-grammar-learning" "raw-data" "development") :name "errors-training-latest" :type "store")))
; (failure-analysis *error-file* *saved-inventory*)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; visualise all constructions per type in web interface.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (add-element (make-html *saved-inventory*))
; (summarize-cxn-types *saved-inventory*)
; (add-element (make-html (get-type-hierarchy *saved-inventory*)))

