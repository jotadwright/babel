
(ql:quickload :mwm)
(in-package :mwm)

(activate-monitor trace-interaction-in-web-interface)
;(deactivate-monitor trace-interaction-in-web-interface)

(activate-monitor print-a-dot-for-each-interaction)

(activate-monitor display-communicative-success)
;(deactivate-monitor display-communicative-success)

;; --------------------
;; + Run interactions +
;; --------------------

;; define some import path variables
(progn
  (defparameter *baseline-simulated-data-sets* '("val"))
  (defparameter *baseline-extracted-data-path*
    (merge-pathnames (make-pathname :directory '(:relative "CLEVR-v1.0" "scenes" "val-ns-vqa"))
                     cl-user:*babel-corpora*))
  
  (defparameter *cogent-simulated-data-sets* '("valA"))
  (defparameter *cogent-extracted-data-path*
    (merge-pathnames (make-pathname :directory '(:relative "CLEVR-CoGenT" "scenes" "valA-ns-vqa"))
                     cl-user:*babel-corpora*))
  
  
  (defparameter *incremental-simulated-data-sets* '("phase_1"))
  (defparameter *incremental-extracted-data-path*
    (merge-pathnames (make-pathname :directory '(:relative "CLEVR-incremental" "scenes" "phase_1-ns_vqa"))
                     cl-user:*babel-corpora*)))

;; define some configurations
(progn
(defparameter *baseline-simulated-configuration*
  (make-configuration
   :entries `((:experiment-type . :baseline)
              (:data-type . :simulated)
              (:category-representation . :prototype)
              (:determine-interacting-agents-mode . :tutor-speaks) 
              (:data-sets . ,*baseline-simulated-data-sets*))))

(defparameter *baseline-extracted-configuration*
  (make-configuration
   :entries `((:experiment-type . :baseline)
              (:data-type . :extracted)
              (:category-representation . :prototype)
              (:determine-interacting-agents-mode . :tutor-speaks)
              (:data-sets . ,*baseline-simulated-data-sets*)
              (:data-path . ,*baseline-extracted-data-path*)
              (:extracted-colour-space . :hsv))))

(defparameter *cogent-simulated-configuration*
  (make-configuration
   :entries `((:experiment-type . :cogent)
              (:data-type . :simulated)
              (:category-representation . :prototype)
              (:determine-interacting-agents-mode . :tutor-speaks)
              (:switch-conditions-after-n-interactions . ,1000)
              (:data-sets . ,*cogent-simulated-data-sets*))))

(defparameter *cogent-extracted-configuration*
  (make-configuration
   :entries `((:experiment-type . :cogent)
              (:data-type . :extracted)
              (:category-representation . :prototype)
              (:determine-interacting-agents-mode . :tutor-speaks)
              (:switch-conditions-after-n-interactions . ,100)
              (:data-sets . ,*cogent-simulated-data-sets*)
              (:data-path . ,*cogent-extracted-data-path*))))

(defparameter *incremental-simulated-configuration*
  (make-configuration
   :entries `((:experiment-type . :incremental)
              (:data-type . :simulated)
              (:category-representation . :prototype)
              (:determine-interacting-agents-mode . :default)
              (:switch-conditions-after-n-interactions . 1000)
              (:data-sets . ,*incremental-simulated-data-sets*))))

(defparameter *incremental-extracted-configuration*
  (make-configuration
   :entries `((:experiment-type . :incremental)
              (:data-type . :extracted)
              (:category-representation . :prototype)
              (:determine-interacting-agents-mode . :tutor-speaks)
              (:switch-conditions-after-n-interactions . 10)
              (:data-sets . ,*incremental-simulated-data-sets*)
              (:data-path . ,*incremental-extracted-data-path*))))
)

(defparameter *experiment*
  (make-instance 'mwm-experiment
                 :configuration *baseline-simulated-configuration*))

(run-interaction *experiment*)

(run-series *experiment* 10)

(display-lexicon (find 'learner (population *experiment*) :key #'id))

;; ---------------------------------
;; + Running series of experiments +
;; ---------------------------------

(run-experiments `(
                   (test
                    ((:experiment-type . :baseline)
                     (:data-type . :simulated)
                     (:category-representation . :prototype)
                     (:determine-interacting-agents-mode . :tutor-speaks)
                     (:data-sets . ,*baseline-simulated-data-sets*)
                     (:data-path . ,*baseline-extracted-data-path*)
                     (:extracted-colour-space . :hsv)))
                   )
                 :number-of-interactions 2500
                 :number-of-series 1
                 :monitors (list "export-communicative-success"
                                 ;"export-lexicon-size"
                                 ;"export-features-per-form"
                                 ;"export-lexicon-evolution"
                                 ;"export-tutor-utterance-length-1"
                                 ;"export-tutor-utterance-length-2"
                                 ;"export-tutor-utterance-length-3"
                                 ;"export-tutor-utterance-length-4"
                                 ;"export-tutor-uses-xpos"
                                 ;"export-tutor-uses-ypos"
                                 ;"export-tutor-uses-color"
                                 ;"export-tutor-uses-size"
                                 ;"export-tutor-uses-material"
                                 ;"export-tutor-uses-shape"
                                 ))

(create-graph-for-single-strategy
 :experiment-name "test"
 :measure-names '("communicative-success")
 :y-axis '(1)
 :y1-max 1
 :xlabel "Number of games"
 :y1-label "Success")

(create-graph-for-single-strategy
 :experiment-name "test-extracted-filter-one"
 :measure-names '("communicative-success")
 :y-axis '(1)
 :y1-max 1
 :xlabel "Number of games"
 :y1-label "Success")


(create-graph-comparing-strategies
 :experiment-names '("bidirectional-learner-extracted-1"
                     "bidirectional-learner-extracted-2"
                     "bidirectional-learner-extracted-3"
                     "bidirectional-learner-extracted-4")
 :measure-name "communicative-success"
 :y-min 0 :y-max 1 :xlabel "Number of games" :y1-label "Communicative Success"
 :title nil :end nil)

(create-stacked-bars-comparing-strategies
 :experiment-names '("max-tutor-utterance-length-4-experiment-type-baseline-data-type-simulated"
                     "max-tutor-utterance-length-4-experiment-type-baseline-data-type-extracted")
 :measure-names '("tutor-utterance-length-1"
                  "tutor-utterance-length-2"
                  "tutor-utterance-length-3"
                  "tutor-utterance-length-4")
 :cluster-labels '("tutor word use")
 :bar-labels '("1 word" "2 words" "3 words" "4 words")
 :y-max 1)

(with-open-file (stream "/Users/jensnevens/Projects/Babel3/experiments/multidimensional-word-meanings/raw-data/test/communicative-success.lisp")
  (let ((data (car (read stream))))
    (average (mapcar #'average (mapcar #'(lambda (d) (subseq d 2000)) data)))))

;; ------------------------------------------
;; + Running experiments for alist monitors +
;; ------------------------------------------

(create-tutor-word-use-graph
 :configurations (entries *baseline-simulated-configuration*)
 :nr-of-interactions 5000)

(create-learner-attribute-use-graph
 :configurations (entries *baseline-simulated-configuration*)
 :nr-of-interactions 5000)

(create-success-per-attribute-type-graph
 :configurations (entries *baseline-simulated-configuration*)
 :nr-of-interactions 5000)

(create-game-outcome-graph
 :configurations `((:experiment-type . :baseline)
                   (:data-type . :extracted)
                   (:scale-world . ,nil)
                   (:category-representation . :prototype)
                   (:determine-interacting-agents-mode . :learner-speaks-after-training-period)
                   (:training-period . 2000)
                   (:data-sets . ,*baseline-simulated-data-sets*)
                   (:data-path . ,*baseline-extracted-data-path*)
                   (:max-tutor-utterance-length . ,4)
                   (:extracted-colour-space . :lab)
                   (:alignment-filter . :all))
 :nr-of-interactions 5000)
