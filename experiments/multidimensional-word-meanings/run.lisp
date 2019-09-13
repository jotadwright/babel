
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


(defparameter *baseline-simulated-data-sets* '("val"))
(defparameter *baseline-extracted-data-path*
  (merge-pathnames (make-pathname :directory '(:relative "CLEVR" "CLEVR-v1.0" "scenes" "val-ns-vqa"))
                   cl-user:*babel-corpora*))

(defparameter *cogent-simulated-data-sets* '("valA"))
(defparameter *cogent-extracted-data-path*
  (merge-pathnames (make-pathname :directory '(:relative "CLEVR-CoGenT" "scenes" "valA-ns-vqa"))
                   cl-user:*babel-corpora*))


(defparameter *incremental-simulated-data-sets* '("incr1"))
(defparameter *incremental-extracted-data-path*
  (merge-pathnames (make-pathname :directory '(:relative "CLEVR" "CLEVR-incremental" "scenes" "incr1-ns-vqa"))
                   cl-user:*babel-corpora*))

(defparameter *baseline-simulated-configuration*
  (make-configuration
   :entries `((:experiment-type . :baseline)
              (:data-type . :simulated)
              (:scale-world . ,nil)
              (:category-representation . :prototype)
              (:determine-interacting-agents-mode . :tutor-speaks)
              (:data-sets . ,*baseline-simulated-data-sets*))))

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

(defparameter *experiment*
  (make-instance 'mwm-experiment :configuration *incremental-simulated-configuration*))

(run-interaction *experiment*)

(run-series *experiment* 10)

(display-lexicon (find 'learner (population *experiment*) :key #'id))
(lexicon->pdf (find 'learner (population *experiment*) :key #'id) :experiment-name 'test)
(lexicon->function-plots (find 'learner (population *experiment*) :key #'id))

(make-table *experiment*)

;; ---------------------------------
;; + Running series of experiments +
;; ---------------------------------

(run-experiments `(
                   (protoype-ns-vqa
                    ((:data-source . :extracted)
                     (:scale-world . ,nil)
                     (:category-representation . :prototype)
                     (:determine-interacting-agents-mode . :tutor-speaks)
                     (:data-path . ,(merge-pathnames
                                     (make-pathname :directory '(:relative "CLEVR" "CLEVR-v1.0" "scenes" "val-ns-vqa"))
                                     cl-user:*babel-corpora*))))
                   )
                 :number-of-interactions 50000
                 :number-of-series 1
                 :monitors (list "export-communicative-success"
                                 ;"export-lexicon-size"
                                 ;"export-features-per-form"
                                 ;"export-utterance-length"
                                 ))

(create-graph-for-single-strategy
 :experiment-name "protoype-ns-vqa"
 :measure-names '("communicative-success")
 :y-axis '(1)
 :y1-max 1
 :xlabel "Number of games"
 :y1-label "Success")

(create-graph-comparing-strategies
 :experiment-names '("ns-vqa-cogent-min-max-5000"
                     "ns-vqa-cogent-prototype-5000"
                     "ns-vqa-cogent-pmm-5000"
                     "ns-vqa-cogent-exponential-5000")
 :measure-name "communicative-success"
 :y-min 0 :y-max 1 :xlabel "Number of games" :y1-label "Communicative Success"
 :captions '("min-max" "prototype" "pmm" "exponential")
 :title nil :end nil)
