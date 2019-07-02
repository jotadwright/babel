
(ql:quickload :mwm)
(in-package :mwm)

(activate-monitor trace-interaction-in-web-interface)
;(deactivate-monitor trace-interaction-in-web-interface)

(activate-monitor print-a-dot-for-each-interaction)

(activate-monitor display-communicative-success)
;(deactivate-monitor display-communicative-success)


;; ----------
;; + CoGenT +
;; ----------

(setf *clevr-data-path*
      (merge-pathnames (make-pathname :directory '(:relative "CLEVR-CoGenT"))
                       cl-user:*babel-corpora*))

(reset-clevr-data-path)

;; --------------------
;; + Run interactions +
;; --------------------

(defparameter *configuration*
  (make-configuration
   :entries '((:data-source . :clevr)
              (:scale-world . nil)
              (:category-representation . :prototype)
              (:determine-interacting-agents-mode . :tutor-speaks)
              (:test-after-n-interactions . 100)
              (:data-sets . ("valA")))))

(defparameter *experiment*
  (make-instance 'mwm-experiment :configuration *configuration*))

(run-interaction *experiment*)

(run-series *experiment* 200)

(display-lexicon (find 'learner (population *experiment*) :key #'id))
(display-lexicon (find 'tutor (population *experiment*) :key #'id))
(lexicon->function-plots (find 'learner (population *experiment*) :key #'id))

(make-table *experiment*)

;; ---------------------------------
;; + Running series of experiments +
;; ---------------------------------

(run-experiments '(
                   (cogent-test
                    ((:data-source . :clevr)
                     (:scale-world . nil)
                     (:category-representation . :min-max)
                     (:determine-interacting-agents-mode . :tutor-speaks)
                     (:test-after-n-interactions . 5000)
                     (:data-sets . ("valA"))))
                   )
                 :number-of-interactions 10000
                 :number-of-series 1
                 :monitors (list "export-communicative-success"
                                 ;"export-lexicon-size"
                                 ;"export-features-per-form"
                                 ;"export-utterance-length"
                                 ))

(create-graph-for-single-strategy
 :experiment-name "cogent-test"
 :measure-names '("communicative-success")
 :y-axis '(1)
 :y1-max 1
 :xlabel "Number of games"
 :y1-label "Success")

(create-graph-comparing-strategies
 :experiment-names '("cogent-min-max-train-10"
                     "cogent-min-max-train-50"
                     "cogent-min-max-train-100"
                     "cogent-min-max-train-200"
                     "cogent-min-max-train-500"
                     "cogent-min-max-train-1000"
                     "cogent-min-max-train-2000")
 :measure-name "communicative-success"
 :y-min 0 :y-max 1 :xlabel "Number of games" :y1-label "Success"
 :captions '("train=10" "train=50" "train=100" "train=200" "train=500" "train=1000" "train=2000") ;
 :title "CLEVR CoGenT (min-max)" :end nil)
