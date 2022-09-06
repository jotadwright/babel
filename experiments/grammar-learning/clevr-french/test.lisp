
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
  ;(activate-monitor export-categorial-network-evolution-to-jsonl)
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
                          :entries '((:repairs . (;add-categorial-links
                                                  ;item-based->item-based--substitution
                                                  ;item-based->holistic
                                                  ;holistic->item-based--substitution
                                                  ;holistic->item-based--addition
                                                  ;holistic->item-based--deletion
                                                  ;holistic->item-based
                                                  nothing->holistic))
                                     (:determine-interacting-agents-mode . :corpus-learner)
                                     (:observation-sample-mode . :debug)
                                     (:meaning-representation . :irl)
                                     (:de-render-mode . :de-render-string-meets-no-punct)
                                     (:corpus-files-root . ,(merge-pathnames
                                                             (make-pathname :directory '(:relative "clevr-grammar-learning"))
                                                             cl-user:*babel-corpora*))
                                     (:corpus-data-file . ,(make-pathname :directory '(:relative "clevr-french" "train")
                                                                          :name "stage-1" :type "jsonl")))))))


                              

;(cl-store:store (grammar (first (agents *experiment*))) (babel-pathname :directory '("experiments" "grammar-learning" "clevr-french") :name "cxn-inventory-train-french-ordered" :type "store"))

;(cl-store::store (grammar (first (agents *experiment*))) "/Users/jonasdoumen/Projects/ehai-babel/experiments/grammar-learning/clevr-french/cxn-inventory-train-french-ordered.store")

; (cl-store::store *experiment* "/Users/jonasdoumen/Projects/ehai-babel/experiments/grammar-learning/clevr-french/experiment-train-french-ordered.store")


;(add-element (make-html (categorial-network (grammar (first (agents *experiment*)))) :weights? t :render-program "circo"))
;(add-element (make-html (categorial-network (grammar (first (agents *experiment*)))) :weights t :render-program "fdp"))
;(add-element (make-html (grammar (first (agents *experiment*))) :sort-by-type-and-score t :routine-only t))

;(defparameter *cxn-inventory* (grammar (first (agents *experiment*))))

;(defparameter *cn* (categorial-network (grammar (first (interacting-agents *experiment*)))))


(create-experiment)
;;; test single interaction
;(run-interaction *experiment*)

;(fcg::configure-grammar *cxn-inventory*)

;;; test series of interactions
(run-series *experiment* (length (question-data *experiment*)))

;(run-series *experiment* 119)

;(run-series *experiment* 15000) ; 

;(run-series *experiment* 47040) ;



#|
- test repairs separately, subst first, then add one by one
 
ISSUES:

73=121
116=128
119=197
72=120


 
TODO:
- check of onze determine-comm-success fn juist werkt in comprehend-all!
- meerdere epochs na elkaar, zien of de alignment nog iets verwijdert
- comprehend-all ipv comprehend en het gemiddelde van het pad doen

|#
