
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

;;;; CONFIGURATIONS
(defparameter *baseline-simulated*
  (make-configuration
   :entries '((:experiment-type . :baseline)
              (:world-type . :simulated)
              (:determine-interacting-agents-mode . :default)
              (:alignment-filter . :at-least-one))))

(defparameter *baseline-extracted*
  (make-configuration
   :entries '((:experiment-type . :baseline)
              (:world-type . :extracted)
              (:determine-interacting-agents-mode . :tutor-speaks))))

(defparameter *cogent-simulated*
  (make-configuration
   :entries '((:experiment-type . :cogent)
              (:world-type . :simulated)
              (:determine-interacting-agents-mode . :tutor-speaks)
              (:switch-conditions-after-n-interactions . 100))))

(defparameter *cogent-extracted*
  (make-configuration
   :entries '((:experiment-type . :cogent)
              (:world-type . :extracted)
              (:determine-interacting-agents-mode . :tutor-speaks)
              (:switch-conditions-after-n-interactions . 100))))

(defparameter *incremental-simulated*
  (make-configuration
   :entries '((:experiment-type . :incremental)
              (:world-type . :simulated)
              (:determine-interacting-agents-mode . :tutor-speaks)
              (:switch-conditions-after-n-interactions . 100))))

(defparameter *incremental-extracted*
  (make-configuration
   :entries '((:experiment-type . :incremental)
              (:world-type . :extracted)
              (:determine-interacting-agents-mode . :tutor-speaks)
              (:switch-conditions-after-n-interactions . 100))))

;;;; EXPERIMENT
(defparameter *experiment*
  (make-instance 'mwm-experiment
                 :configuration *baseline-simulated*))

(run-interaction *experiment*)

(run-series *experiment* 100)

(display-lexicon (find 'learner (population *experiment*) :key #'id))

;; ---------------------------------
;; + Running series of experiments +
;; ---------------------------------

(run-experiments `(
                   (test
                    ((:experiment-type . :baseline)
                     (:world-type . :simulated)
                     (:determine-interacting-agents-mode . :default)))
                   )
                 :number-of-interactions 5000
                 :number-of-series 3
                 :monitors (list "export-communicative-success"
                                 "export-lexicon-size"
                                 "export-communicative-success-given-conceptualisation"
                                 ;"export-learner-concepts"
                                 ))

(create-graph-for-single-strategy
 :experiment-name "test"
 :measure-names '("communicative-success"
                  "communicative-success-given-conceptualisation"
                  "lexicon-size"
                  )
 :average-windows '(200 200 1)
 :y-axis '(1 1 2)
 :y1-min 0 :y1-max 1
 :y2-max nil
 :xlabel "Number of games"
 :y1-label "Communicative Success"
 :y2-label "Concept Repertoire Size"
 :open t)

(create-tutor-word-use-graph
 :configurations
 '((:experiment-type . :baseline)
   (:world-type . :extracted)
   (:determine-interacting-agents-mode . :tutor-speaks))
 :nr-of-interactions 2500)

(create-learner-failed-conceptualisation-graph
 :configurations
 '((:experiment-type . :baseline)
   (:world-type . :extracted)
   (:determine-interacting-agents-mode . :default))
 :nr-of-interactions 5000)


;;;; Computing average success
(with-open-file
    (stream (babel-pathname
             :directory '("experiments" "multidimensional-word-meanings"
                          "raw-data" "thesis"
                          "baseline-simulated")
             :name "communicative-success" :type "lisp"))
  (defparameter *simulated-success-data* (read stream)))

(with-open-file
    (stream (babel-pathname
             :directory '("experiments" "multidimensional-word-meanings"
                          "raw-data" "thesis"
                          "baseline-simulated-bidirectional")
             :name "communicative-success" :type "lisp"))
  (defparameter *bidirectional-simulated-success-data* (read stream)))

(with-open-file
    (stream (babel-pathname
             :directory '("experiments" "multidimensional-word-meanings"
                          "raw-data" "thesis"
                          "baseline-simulated-bidirectional")
             :name "communicative-success-given-conceptualisation"
             :type "lisp"))
  (defparameter *bidirectional-simulated-success-given-conceptualisation-data* (read stream)))

(with-open-file
    (stream (babel-pathname
             :directory '("experiments" "multidimensional-word-meanings"
                          "raw-data" "thesis"
                          "baseline-extracted")
             :name "communicative-success" :type "lisp"))
  (defparameter *extracted-success-data* (read stream)))

(with-open-file
    (stream (babel-pathname
             :directory '("experiments" "multidimensional-word-meanings"
                          "raw-data" "thesis"
                          "baseline-extracted-bidirectional")
             :name "communicative-success" :type "lisp"))
  (defparameter *bidirectional-extracted-success-data* (read stream)))

(with-open-file
    (stream (babel-pathname
             :directory '("experiments" "multidimensional-word-meanings"
                          "raw-data" "thesis"
                          "baseline-extracted-bidirectional")
             :name "communicative-success-given-conceptualisation"
             :type "lisp"))
  (defparameter *bidirectional-extracted-success-given-conceptualisation-data* (read stream)))

(defun compute-success-at-point (data point &optional last-n)
  (loop for series in (first data)
        if last-n
        sum (average (subseq series (- point last-n) point)) into sum-list
        else
        sum (nth point series) into sum-list
        end
        count series into denom
        finally (return (float (/ sum-list denom)))))

(compute-success-at-point *simulated-success-data* 3000) ;; 0.998
(compute-success-at-point *bidirectional-simulated-success-data* 3000) ;; 0.986
(compute-success-at-point *bidirectional-simulated-success-given-conceptualisation-data* 3000 100) ;; 0.997
;; ==> 100% when removing spatial relations!

(compute-success-at-point *extracted-success-data* 3000) ;; 0.894
(compute-success-at-point *bidirectional-extracted-success-data* 3000) ;; 0.811
(compute-success-at-point *bidirectional-extracted-success-given-conceptualisation-data* 3000 100) ;; 0.922


