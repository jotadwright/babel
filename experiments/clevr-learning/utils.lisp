;;;; utils.lisp

(in-package :clevr-learning)

(defun extract-meanings-from-cipn (cipn)
  (extract-meanings
   (left-pole-structure
    (car-resulting-cfs
     (cipn-car cipn)))))

(defun extract-forms-from-cipn (cipn)
  (extract-forms
   (left-pole-structure
    (car-resulting-cfs
     (cipn-car cipn)))))

(defun get-cxn-type (cxn)
  (attr-val cxn :cxn-type))



(defun run-experiments (strategies
                         &key
                         (number-of-interactions 5)
                         (number-of-series 1)
                         (monitors
                           (list "export-communicative-success"
                                 "export-lexicon-size"
                                 "export-meanings-per-form"
                                 "export-lexicon-change"
                                 "export-avg-cxn-score"
                                 "export-confidence-level"))
                         (determine-interacting-agents-mode :tutor-learner)
                         (questions-per-challenge 1000)
                         (alignment-strategy :minimal-holophrases+lateral-inhibition)
                         (composer-strategy :store-past-scenes)
                         (hide-type-hierarchy nil))
  (format t "~%Starting experimental runs")
  (run-batch-for-different-configurations
    :experiment-class 'clevr-learning-experiment 
    :number-of-interactions number-of-interactions
    :number-of-series number-of-series
    :named-configurations strategies
    :shared-configuration `((:determine-interacting-agents-mode . ,determine-interacting-agents-mode)
                            (:alignment-strategy . ,alignment-strategy)
                            (:composer-strategy . ,composer-strategy)
                            (:questions-per-challenge . ,questions-per-challenge)
                            (:hide-type-hierarchy . ,hide-type-hierarchy))
    :monitors monitors
    :output-dir (babel-pathname :directory '("experiments" "clevr-learning" "raw-data")))
  (format t "~%Experimental runs finished and data has been generated. You can now plot graphs."))

(defun create-graph-for-single-strategy (&key experiment-name measure-names
                                              y-axis y1-max y2-max xlabel y1-label y2-label
                                              captions)
  ;; This function allows you to plot one or more measures for a single experiment
  ;; e.g. communicative success and lexicon size
  (format t "~%Creating graph for experiment ~a with measures ~a" experiment-name measure-names)
  (raw-files->evo-plot
    :raw-file-paths
    (loop for measure-name in measure-names
          collect `("experiments" "clevr-learning" "raw-data" ,experiment-name ,measure-name))
    :average-windows 100
    :plot-directory `("experiments" "clevr-learning" "graphs")
    :error-bars '(:stdev)
    :error-bar-modes '(:lines)
    :captions captions
    :use-y-axis y-axis
    :y1-min 0
    :y1-max y1-max
    :y2-min 0
    :y2-max y2-max
    :x-label (if xlabel xlabel "Number of Games")
    :y1-label (when y1-label y1-label)
    :y2-label (when y2-label y2-label))
  (format t "~%Graphs have been created"))

(defun create-graph-comparing-strategies (&key experiment-names measure-name
                                               (y-min 0) (y-max 1) xlabel y1-label y2-label
                                               captions)
  ;; This function allows you to compare a given measure accross different
  ;; experiments, e.g. comparing lexicon size
  (format t "~%Creating graph for experiments ~a with measure ~a" experiment-names measure-name)
  (raw-files->evo-plot
    :raw-file-paths
    (loop for experiment-name in experiment-names
          collect `("experiments" "clevr-learning" "raw-data" ,experiment-name ,measure-name))
    :average-windows 500
    :captions (if captions captions experiment-names)
    :plot-directory '("experiments" "clevr-learning" "graphs")
    :error-bars '(:stdev)
    :error-bar-modes '(:lines)
    :y1-min y-min
    :y1-max y-max
    :x-label (if xlabel xlabel "Number of Games")
    :y1-label (when y1-label y1-label)
    :y2-label (when y2-label y2-label))
  (format t "~%Graphs have been created"))

(defun create-num-cxns-per-type-graph (&key 
                                       (configurations nil)
                                       (nr-of-interactions 2000))
  (format t "~%Running ~a interactions in order to create the graph. Please be patient." nr-of-interactions)
  (activate-monitor plot-num-cxns-per-type)
  (run-batch 'clevr-learning-experiment nr-of-interactions 1
             :configuration (make-configuration :entries configurations))
  (deactivate-monitor plot-num-cxns-per-type)
  (format t "~%Graphs have been created"))

(defun create-cxn-scores-per-type-graph (&key 
                                         (configurations nil)
                                         (nr-of-interactions 2000))
  (format t "~%Running ~a interactions in order to create the graph. Please be patient." nr-of-interactions)
  (activate-monitor plot-cxn-score-per-type)
  (run-batch 'clevr-learning-experiment nr-of-interactions 1
             :configuration (make-configuration :entries configurations))
  (deactivate-monitor plot-cxn-score-per-type)
  (format t "~%Graphs have been created"))