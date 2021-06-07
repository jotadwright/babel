(ql:quickload :clevr-grammar-learning)
(in-package :clevr-grammar-learning)


(defun summarize-cxn-types (cxn-inventory)
  (let ((holophrase-cxns (sort (find-all 'gl::holophrase (constructions-list cxn-inventory)
                                         :key #'get-cxn-type)  #'> :key (lambda (cxn) (attr-val cxn :score))))
        (lexical-cxns (sort (find-all 'gl::lexical (constructions-list cxn-inventory)
                                      :key #'get-cxn-type)  #'> :key (lambda (cxn) (attr-val cxn :score))))
        (item-based-cxns (sort (find-all 'gl::item-based (constructions-list cxn-inventory)
                                         :key #'get-cxn-type)  #'> :key (lambda (cxn) (attr-val cxn :score)))))
        
    (add-element `((h2) ,(format nil "Holophrases: ~a" (length holophrase-cxns))))
    (loop for cxn in holophrase-cxns
          do (add-element (make-html cxn)))
    (add-element '((hr)))
    (add-element `((h2) ,(format nil "Lexical cxns: ~a" (length lexical-cxns))))
    (loop for cxn in lexical-cxns
          do (add-element (make-html cxn)))
    (add-element '((hr)))
    (add-element `((h2) ,(format nil "Item-based cxns: ~a" (length item-based-cxns))))
    (loop for cxn in item-based-cxns
          do (add-element (make-html cxn)))
    (add-element '((hr)))
    (add-element '((hr)))))


(defun run-training ()
  (wi::reset)
  (let ((experiment-name 'training))
    (run-experiments `(
                       (,experiment-name
                        ((:determine-interacting-agents-mode . :corpus-learner)
                         (:observation-sample-mode . :random)
                         (:learner-th-connected-mode . :neighbours)
                         ))
                       )
                     :number-of-interactions 47134
                     :number-of-series 1
                     :monitors (append '("print-a-dot-for-each-interaction"
                                         "summarize-results-after-n-interactions")
                                       (get-all-lisp-monitors)
                                       (get-all-export-monitors)))))

(defun run-evaluation (stored-grammar-path)
  (wi::reset)
  (let ((learned-grammar (cl-store:restore stored-grammar-path))
        (experiment-name 'evaluation))
    (run-experiments `(
                       (,experiment-name
                        ((:determine-interacting-agents-mode . :corpus-learner)
                         (:observation-sample-mode . :evaluation)
                         (:evaluation-grammar . ,learned-grammar)
                         (:learner-th-connected-mode . :neighbours)
                         ))
                       )
                     :number-of-interactions 10044
                     :number-of-series 1
                     :monitors (append '("print-a-dot-for-each-interaction"
                                         "evaluation-after-n-interactions")
                                       (get-all-lisp-monitors)
                                       (get-all-export-monitors)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; train, load the exported grammar and evaluate it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (run-training)
; (defparameter *saved-inventory* (cl-store:restore (babel-pathname :directory '("experiments" "clevr-grammar-learning" "raw-data") :name "cxn-inventory-train-latest" :type "store")))
; (run-evaluation *saved-inventory*)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; visualise all constructions per type in web interface.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (add-element (make-html *saved-inventory*))
; (summarize-cxn-types *saved-inventory*)
; (add-element (make-html (get-type-hierarchy *saved-inventory*)))
