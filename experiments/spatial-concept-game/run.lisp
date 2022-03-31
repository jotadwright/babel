
(ql:quickload :spatial-concepts)
(in-package :spatial-concepts)

(activate-monitor trace-interaction-in-web-interface)
;(deactivate-monitor trace-interaction-in-web-interface)

(activate-monitor print-a-dot-for-each-interaction)

(activate-monitor display-communicative-success)
;(deactivate-monitor display-communicative-success)

;; --------------------
;; + Run interactions +
;; --------------------

;;;; CONFIGURATIONS
(defparameter *simulated*
  (make-configuration
   :entries '((:world-type . :simulated)
              (:alignment-filter . :all))))

(defparameter *extracted*
  (make-configuration
   :entries '((:world-type . :extracted)
              (:alignment-filter . :all))))


;;;; EXPERIMENT
(defparameter *experiment*
  (make-instance 'spatial-experiment
                 :configuration *simulated*))

(run-interaction *experiment*)

(run-series *experiment* 100)

(display-lexicon (find 'learner (population *experiment*) :key #'id))

;; ---------------------------------
;; + Running series of experiments +
;; ---------------------------------

(run-experiments `(
                   (test
                    ((:world-type . :simulated)
                     (:determine-interacting-agents-mode . :default
                     (:alignment-filter . :all)))
                   )
                 :number-of-interactions 2000
                 :number-of-series 1
                 :monitors (list "export-communicative-success"
                                 "export-lexicon-size"
                                 "export-communicative-success-given-conceptualisation"
                                 ;"export-learner-concepts-to-pdf"
                                 ;"export-learner-concepts-to-store"
                                 ;"export-experiment-configurations"
                                 ))

(create-graph-for-single-strategy
 "test" '("communicative-success" "lexicon-size")
 :plot-file-name "baseline-simulated"
 :average-windows '(100 1)
 :use-y-axis '(1 2)
 :y1-min 0 :y1-max 1
 :y2-min 0 :y2-max 30
 :x-label "Number of Games"
 :y1-label "Communicative Success"
 :y2-label "Number of Concepts"
 :captions '("communicative success"
             "concept repertoire size")
 :error-bars '(:percentile 5 95)
 :error-bar-modes '(:lines)
 :key-location "bottom"
 :open nil)

(create-tutor-word-use-graph
 :configurations
 '((:world-type . :extracted))
 :nr-of-interactions 2500)

(create-learner-failed-conceptualisation-graph
 :configurations
 '((:world-type . :extracted))
 :nr-of-interactions 5000)



;; -------------
;; + All plots +
;; -------------
(create-graph-mixing-strategies
 :experiment-measure-conses
 '(("baseline-simulated" . "communicative-success")
   ("baseline-simulated-bidirectional" . "communicative-success")
   ("baseline-simulated-bidirectional" . "communicative-success-given-conceptualisation")
   ("baseline-simulated-bidirectional" . "lexicon-size"))
 :plot-file-name "baseline-simulated-comparison"
 :xlabel "Number of Games"
 :y1-label "Communicative Success"
 :y2-label "Number of Concepts"
 :captions '("communicative success (always listener)"
             "communicative success (both roles)"
             "communicative success given conceptualisation (both roles)"
             "concept repertoire size")
 :window '(100 100 100 1)
 :use-y-axis '(1 1 1 2) :y1-max 1 :y2-max 30
 :end 5000)

(create-graph-mixing-strategies
 :experiment-measure-conses
 '(("baseline-extracted" . "communicative-success")
   ("baseline-extracted-bidirectional" . "communicative-success")
   ("baseline-extracted-bidirectional" . "communicative-success-given-conceptualisation")
   ("baseline-extracted-bidirectional" . "lexicon-size"))
 :plot-file-name "baseline-extracted-comparison"
 :xlabel "Number of Games"
 :y1-label "Communicative Success"
 :y2-label "Number of Concepts"
 :captions '("communicative success (always listener)"
             "communicative success (both roles)"
             "communicative success given conceptualisation (both roles)"
             "concept repertoire size")
 :window '(100 100 100 1)
 :use-y-axis '(1 1 1 2) :y1-max 1 :y2-max 30
 :end 5000)


(create-graph-mixing-strategies
 '(("cogent-simulated-bidirectional-switch-1000" . "communicative-success")
   ("cogent-extracted-bidirectional-switch-500" . "communicative-success"))
 :plot-file-name "cogent-bidirectional-switch-500"
 :x-label "Number of Games"
 :y1-label "Communicative Success"
 :captions '("simulated environment"
             "noisy environment")
 :average-windows '(100 100)
 :use-y-axis '(1 1) :y1-min 0 :y1-max 1
 :error-bars '(:percentile 5 95)
 :error-bar-modes '(:lines)
 :key-location "bottom"
 :fsize 12 :open nil)


;; -----------------------------
;; + Computing average success +
;; -----------------------------

(with-open-file
    (stream (babel-pathname
             :directory '("experiments" "spatial-concept-game"
                          "raw-data" "thesis-main-results"
                          "baseline-simulated")
             :name "communicative-success" :type "lisp"))
  (defparameter *simulated-success-data* (read stream)))

(with-open-file
    (stream (babel-pathname
             :directory '("experiments" "spatial-concept-game"
                          "raw-data" "thesis-main-results"
                          "baseline-simulated-bidirectional")
             :name "communicative-success" :type "lisp"))
  (defparameter *bidirectional-simulated-success-data* (read stream)))

(with-open-file
    (stream (babel-pathname
             :directory '("experiments" "spatial-concept-game"
                          "raw-data" "thesis-main-results"
                          "baseline-simulated-bidirectional")
             :name "communicative-success-given-conceptualisation"
             :type "lisp"))
  (defparameter *bidirectional-simulated-success-given-conceptualisation-data* (read stream)))

(with-open-file
    (stream (babel-pathname
             :directory '("experiments" "spatial-concept-game"
                          "raw-data" "thesis-main-results"
                          "baseline-extracted")
             :name "communicative-success" :type "lisp"))
  (defparameter *extracted-success-data* (read stream)))

(with-open-file
    (stream (babel-pathname
             :directory '("experiments" "spatial-concept-game"
                          "raw-data" "thesis-main-results"
                          "baseline-extracted-bidirectional")
             :name "communicative-success" :type "lisp"))
  (defparameter *bidirectional-extracted-success-data* (read stream)))

(with-open-file
    (stream (babel-pathname
             :directory '("experiments" "spatial-concept-game"
                          "raw-data" "thesis-main-results"
                          "baseline-extracted-bidirectional")
             :name "communicative-success-given-conceptualisation"
             :type "lisp"))
  (defparameter *bidirectional-extracted-success-given-conceptualisation-data* (read stream)))

(with-open-file
    (stream (babel-pathname
             :directory '("experiments" "spatial-concept-game"
                          "raw-data" "thesis-cogent"
                          "cogent-simulated-switch-500")
             :name "communicative-success"
             :type "lisp"))
  (defparameter *cogent-simulated-switch-500* (read stream)))

(with-open-file
    (stream (babel-pathname
             :directory '("experiments" "spatial-concept-game"
                          "raw-data" "thesis-cogent"
                          "cogent-simulated-switch-1000")
             :name "communicative-success"
             :type "lisp"))
  (defparameter *cogent-simulated-switch-1000* (read stream)))

(with-open-file
    (stream (babel-pathname
             :directory '("experiments" "spatial-concept-game"
                          "raw-data" "thesis-cogent"
                          "cogent-simulated-bidirectional-switch-500")
             :name "communicative-success"
             :type "lisp"))
  (defparameter *cogent-simulated-bidirectional-switch-500* (read stream)))

(with-open-file
    (stream (babel-pathname
             :directory '("experiments" "spatial-concept-game"
                          "raw-data" "thesis-cogent"
                          "cogent-simulated-bidirectional-switch-1000")
             :name "communicative-success"
             :type "lisp"))
  (defparameter *cogent-simulated-bidirectional-switch-1000* (read stream)))

(with-open-file
    (stream (babel-pathname
             :directory '("experiments" "spatial-concept-game"
                          "raw-data" "thesis-cogent"
                          "cogent-extracted-switch-500")
             :name "communicative-success"
             :type "lisp"))
  (defparameter *cogent-extracted-switch-500* (read stream)))

(with-open-file
    (stream (babel-pathname
             :directory '("experiments" "spatial-concept-game"
                          "raw-data" "thesis-cogent"
                          "cogent-extracted-switch-1000")
             :name "communicative-success"
             :type "lisp"))
  (defparameter *cogent-extracted-switch-1000* (read stream)))

(with-open-file
    (stream (babel-pathname
             :directory '("experiments" "spatial-concept-game"
                          "raw-data" "thesis-cogent"
                          "cogent-extracted-bidirectional-switch-500")
             :name "communicative-success"
             :type "lisp"))
  (defparameter *cogent-extracted-bidirectional-switch-500* (read stream)))

(with-open-file
    (stream (babel-pathname
             :directory '("experiments" "spatial-concept-game"
                          "raw-data" "thesis-cogent"
                          "cogent-extracted-bidirectional-switch-1000")
             :name "communicative-success"
             :type "lisp"))
  (defparameter *cogent-extracted-bidirectional-switch-1000* (read stream)))

(defun compute-success-at-point (data point &optional last-n)
  (loop for series in (first data)
        if last-n
        sum (average (subseq series (- point last-n) point)) into sum-list
        else
        sum (nth point series) into sum-list
        end
        count series into denom
        finally (return (float (/ sum-list denom)))))

(compute-success-at-point *simulated-success-data* 5000 100) ;; 0.996
(compute-success-at-point *cogent-simulated-switch-500* 5000 100) ;; 0.983
(compute-success-at-point *cogent-simulated-switch-1000* 5000 100) ;; 0.993


(compute-success-at-point *bidirectional-simulated-success-data* 5000 100) ;; 0.983
(compute-success-at-point *bidirectional-simulated-success-given-conceptualisation-data* 5000 100)
(compute-success-at-point *cogent-simulated-bidirectional-switch-500* 5000 100) ;; 0.948
(compute-success-at-point *cogent-simulated-bidirectional-switch-1000* 5000 100) ;; 0.968


(compute-success-at-point *extracted-success-data* 5000 100) ;; 0.913
(compute-success-at-point *cogent-extracted-switch-500* 5000 100) ;; 0.879
(compute-success-at-point *cogent-extracted-switch-1000* 5000 100) ;; 0.892


(compute-success-at-point *bidirectional-extracted-success-data* 5000 100) ;; 0.812
(compute-success-at-point *bidirectional-extracted-success-given-conceptualisation-data* 5000 100)
(compute-success-at-point *cogent-extracted-bidirectional-switch-500* 5000 100) ;; 0.799
(compute-success-at-point *cogent-extracted-bidirectional-switch-1000* 5000 100) ;; 0.805


