
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
  (merge-pathnames (make-pathname :directory '(:relative "CLEVR-incremental" "scenes" "phase_1-ns-vqa"))
                   cl-user:*babel-corpora*))
  
(defparameter *baseline-simulated-configuration*
  (make-configuration
   :entries `((:experiment-type . :baseline)
              (:data-type . :simulated)
              (:scale-world . ,nil)
              (:category-representation . :prototype)
              (:determine-interacting-agents-mode . :tutor-speaks)
              (:data-sets . ,*baseline-simulated-data-sets*)
              (:max-tutor-utterance-length . ,3))))

(defparameter *baseline-extracted-configuration*
  (make-configuration
   :entries `((:experiment-type . :baseline)
              (:data-type . :extracted)
              (:scale-world . ,nil)
              (:category-representation . :prototype)
              (:determine-interacting-agents-mode . :tutor-speaks)
              (:data-sets . ,*baseline-simulated-data-sets*)
              (:data-path . ,*baseline-extracted-data-path*))))

(defparameter *cogent-simulated-configuration*
  (make-configuration
   :entries `((:experiment-type . :cogent)
              (:data-type . :simulated)
              (:scale-world . ,nil)
              (:category-representation . :prototype)
              (:determine-interacting-agents-mode . :tutor-speaks)
              (:switch-conditions-after-n-interactions . ,100)
              (:data-sets . ,*cogent-simulated-data-sets*))))

(defparameter *cogent-extracted-configuration*
  (make-configuration
   :entries `((:experiment-type . :cogent)
              (:data-type . :extracted)
              (:scale-world . ,nil)
              (:category-representation . :prototype)
              (:determine-interacting-agents-mode . :tutor-speaks)
              (:switch-conditions-after-n-interactions . ,100)
              (:data-sets . ,*cogent-simulated-data-sets*)
              (:data-path . ,*cogent-extracted-data-path*))))

(defparameter *incremental-simulated-configuration*
  (make-configuration
   :entries `((:experiment-type . :incremental)
              (:data-type . :simulated)
              (:scale-world . ,nil)
              (:category-representation . :prototype)
              (:determine-interacting-agents-mode . :tutor-speaks)
              (:switch-conditions-after-n-interactions . 10)
              (:data-sets . ,*incremental-simulated-data-sets*))))

(defparameter *incremental-extracted-configuration*
  (make-configuration
   :entries `((:experiment-type . :incremental)
              (:data-type . :extracted)
              (:scale-world . ,nil)
              (:category-representation . :prototype)
              (:determine-interacting-agents-mode . :tutor-speaks)
              (:switch-conditions-after-n-interactions . 10)
              (:data-sets . ,*incremental-simulated-data-sets*)
              (:data-path . ,*incremental-extracted-data-path*))))
)

(defparameter *experiment*
  (make-instance 'mwm-experiment :configuration *baseline-simulated-configuration*))

(run-interaction *experiment*)

(run-series *experiment* 2000)

(display-lexicon (find 'learner (population *experiment*) :key #'id))
(lexicon->pdf (find 'learner (population *experiment*) :key #'id))
(lexicon->function-plots (find 'learner (population *experiment*) :key #'id))

(make-table *experiment*)

;; ---------------------------------
;; + Running series of experiments +
;; ---------------------------------

;;;; Experiments to run:
;; Baseline simulated and extracted with multi-word utterances (up to 4)
;; Baseline extracted with different colour spaces (HSV, RGB, LAB)
;; CoGenT (simulated and extracted) with mutli-word utterances
;; Incremental (simulated and extracted) with new dataset (need to create this)
;; Incremental (simulated and extracted) with multi-word utterances

(run-experiments `(
                   (test
                    ((:experiment-type . :baseline)
                     (:data-type . :simulated)
                     (:scale-world . ,nil)
                     (:category-representation . :prototype)
                     (:determine-interacting-agents-mode . :tutor-speaks)
                     (:data-sets . ,*baseline-simulated-data-sets*)
                     (:data-path . ,*baseline-extracted-data-path*)
                     (:max-tutor-utterance-length . ,4)))
                   )
                 :number-of-interactions 5000
                 :number-of-series 1
                 :monitors (list "export-communicative-success"
                                 ;"export-lexicon-size"
                                 ;"export-features-per-form"
                                 ;"export-lexicon-evolution"
                                 "export-tutor-utterance-length-1"
                                 "export-tutor-utterance-length-2"
                                 "export-tutor-utterance-length-3"
                                 "export-tutor-utterance-length-4"
                                 "export-tutor-uses-xpos"
                                 "export-tutor-uses-ypos"
                                 "export-tutor-uses-color"
                                 "export-tutor-uses-size"
                                 "export-tutor-uses-material"
                                 "export-tutor-uses-shape"
                                 ))

(create-graph-for-single-strategy
 :experiment-name "baseline-mwe-3"
 :measure-names '("communicative-success")
 :y-axis '(1)
 :y1-max 1
 :xlabel "Number of games"
 :y1-label "Success")

(create-graph-comparing-strategies
 :experiment-names '("subsets-incremental-extracted-100"
                     "subsets-incremental-extracted-500"
                     "subsets-incremental-extracted-1000")
 :measure-name "communicative-success"
 :y-min 0 :y-max 1 :xlabel "Number of games" :y1-label "Communicative Success"
 :captions '("switch=100" "500" "1000")
 :title nil :end nil)

(create-graph-comparing-strategies
 :experiment-names '("data-type-simulated-experiment-type-incremental-switch-conditions-after-n-interactions-5000-category-representation-min-max"
                     "data-type-simulated-experiment-type-incremental-switch-conditions-after-n-interactions-5000-category-representation-prototype"
                     "data-type-simulated-experiment-type-incremental-switch-conditions-after-n-interactions-5000-category-representation-prototype-min-max"
                     "data-type-simulated-experiment-type-incremental-switch-conditions-after-n-interactions-5000-category-representation-exponential")
 :measure-name "lexicon-size"
 :y-min 0 :y-max nil :xlabel "Number of games" :y1-label "Lexicon Size"
 :captions '("min-max" "prototype" "pmm" "exponential")
 :title nil :end 25000)

(create-stacked-bars-comparing-strategies
 :experiment-names '("test")
 :measure-names '("tutor-utterance-length-1"
                  "tutor-utterance-length-2"
                  "tutor-utterance-length-3"
                  "tutor-utterance-length-4"))

(create-stacked-bars-comparing-strategies
 :experiment-names '("test")
 :measure-names '("tutor-uses-xpos"
                  "tutor-uses-ypos"
                  "tutor-uses-color"
                  "tutor-uses-size"
                  "tutor-uses-material"
                  "tutor-uses-shape")
 :bar-labels '("xpos" "ypos"
               "color" "size"
               "material" "shape")
 :title "Tutor attribute use")

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
