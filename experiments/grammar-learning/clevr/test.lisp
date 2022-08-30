
(ql:quickload :grammar-learning)
(in-package :grammar-learning)


;(setf *raise-errors* nil)
;; full logging except trace-fcg
(progn
  (deactivate-all-monitors)
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
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor export-categorial-network-evolution-to-jsonl)
  (activate-monitor export-type-hierarchy-to-json))

  

;; full logging
(progn
  (deactivate-all-monitors)
  (activate-monitor display-metrics)
  (activate-monitor trace-fcg)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor summarize-results-after-n-interactions)
  (activate-monitor show-type-hierarchy-after-n-interactions)
  (activate-monitor trace-interactions-in-wi))



;; sparse logging, no trace-fcg
(progn
  (deactivate-all-monitors)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor summarize-results-after-n-interactions))

(defun create-experiment ()
  (wi::reset)
  (notify reset-monitors)
  (defparameter *experiment*
    (eval `(make-instance 'grammar-learning-experiment
                   :entries '((:repairs . (add-categorial-links
                                           item-based->item-based--substitution
                                           item-based->holistic
                                           holistic->item-based--substitution
                                           ;holistic->item-based--addition
                                           ;holistic->item-based--deletion
                                           holistic->item-based
                                           nothing->holistic))
                         (:observation-sample-mode . :debug)
                         (:categorial-network-export-interval . 1000)
                         (:meaning-representation . :irl)
                         (:de-render-mode . :de-render-string-meets-no-punct)
                         (:corpus-files-root . ,(merge-pathnames
                                     (make-pathname :directory '(:relative "clevr-grammar-learning"))
                                     cl-user:*babel-corpora*))
                         (:corpus-data-file . ,(make-pathname :directory '(:relative "train")
                                                   :name "stage-1" :type "jsonl")))))))



;(cl-store:store (grammar (first (agents *experiment*))) (babel-pathname :directory '("experiments" "clevr-grammar-learning" "raw-data") :name "cxn-inventory-train-random" :type "store"))

;(add-element (make-html (categorial-network (grammar (first (agents *experiment*)))) :weights? t :render-program "circo"))
;(add-element (make-html (categorial-network (grammar (first (agents *experiment*)))) :weights t :render-program "fdp"))
;(add-element (make-html (grammar (first (agents *experiment*))) :sort-by-type-and-score t :routine-only t))

;(defparameter *cxn-inventory* (grammar (first (agents *experiment*))))

;(defparameter *th* (categorial-network (grammar (first (interacting-agents *experiment*)))))

(create-experiment)
;;; test single interaction
;(run-interaction *experiment*)


;;; test series of interactions
;(run-series *experiment* (length (question-data *experiment*)))

;(run-series *experiment* 174)   ;  

;(run-series *experiment* 2000) ;



#|
ISSUES:
observation 34 holistic -> item-based

 
TODO:
- check of onze determine-comm-success fn juist werkt in comprehend-all!
- meerdere epochs na elkaar, zien of de alignment nog iets verwijdert
- comprehend-all ipv comprehend en het gemiddelde van het pad doen

|#
