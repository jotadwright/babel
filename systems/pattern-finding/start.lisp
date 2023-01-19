(ql:quickload :pattern-finding)
(in-package :pattern-finding)



(progn
  (deactivate-all-monitors)
  ;(activate-monitor display-metrics)
  (activate-monitor trace-fcg)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor summarize-results-after-n-interactions)
  (activate-monitor show-type-hierarchy-after-n-interactions)
  (activate-monitor trace-interactions-in-wi)
  )

(progn
  (wi::reset)
  (notify reset-monitors)
  (defparameter *experiment*
    (make-instance 'pattern-finding-experiment
                   :entries `((:observation-sample-mode . :development)
                              (:meaning-representation . :irl)
                              (:de-render-mode . :de-render-string-meets-no-punct)
                              (:corpus-files-root . ,(merge-pathnames
                                     (make-pathname :directory '(:relative "clevr-grammar-learning"))
                                     cl-user:*babel-corpora*))
                              (:corpus-data-file . ,(make-pathname :directory '(:relative "train")
                                                   :name "stage-1" :type "jsonl"))))))

;(defparameter *cxn-inventory* (grammar (first (agents *experiment*))))
;(add-element (make-html *cxn-inventory*))


(run-interaction *experiment*)




;;;;;;;;;;;;;

(defun set-up-cxn-inventory-and-repairs ()
  (wi::reset)
  (notify reset-monitors)
  (make-instance 'pattern-finding-experiment
                 :entries '((:observation-sample-mode . :do-nothing)
                            (:meaning-representation . :irl))))

(defparameter *test-experiment* (set-up-cxn-inventory-and-repairs))
(defparameter *agent-cxn-inventory* (grammar (first (population *test-experiment*))))
(add-element (make-html *agent-cxn-inventory*))


;; learn a holophrase cxn
(comprehend-all "What size is the blue metal cube?"
                :cxn-inventory *agent-cxn-inventory*
                :gold-standard-meaning '((get-context ?context)
                                         (filter ?set-1 ?context ?cube)
                                         (bind shape ?cube cube)
                                         (filter ?set-2 ?set-1 ?metal)
                                         (bind material ?metal metal)
                                         (filter ?set-3 ?set-2 ?blue)
                                         (bind color ?blue blue)
                                         (unique ?obj-1 ?set-3)
                                         (query ?answer ?obj-1 ?size)
                                         (bind attribute ?size size)))

;; learn what-size-is-the-X + blue-metal-cube + cyan-rubber-cylinder
(comprehend-all "What size is the cyan rubber cylinder?"
                :cxn-inventory *agent-cxn-inventory*
                :gold-standard-meaning '((get-context ?context)
                                         (filter ?set-1 ?context ?cylinder)
                                         (bind shape ?cylinder cylinder)
                                         (filter ?set-2 ?set-1 ?rubber)
                                         (bind material ?rubber rubber)
                                         (filter ?set-3 ?set-2 ?cyan)
                                         (bind color ?cyan cyan)
                                         (unique ?obj-1 ?set-3)
                                         (query ?answer ?obj-1 ?size)
                                         (bind attribute ?size size)))

;; learn X-cube + blue-metal + the
(comprehend-all "There is a blue metal ball; what is its size?"
                :cxn-inventory *agent-cxn-inventory*
                :gold-standard-meaning '((get-context ?context)
                                         (filter ?set-1 ?context ?ball)
                                         (bind shape ?ball ball)
                                         (filter ?set-2 ?set-1 ?metal)
                                         (bind material ?metal metal)
                                         (filter ?set-3 ?set-2 ?blue)
                                         (bind color ?blue blue)
                                         (unique ?obj-1 ?set-3)
                                         (query ?answer ?obj-1 ?size)
                                         (bind attribute ?size size)))
