(in-package :grounded-color-naming-game)

(defun run-experiments (strategies
                         &key
                         (number-of-interactions 5)
                         (number-of-series 1)
                         (monitors
                          (list "export-communicative-success"
                                "export-lexicon-size"
                                "export-ontology-size"
                                "export-avg-forms-per-meaning"
                                "export-avg-meanings-per-form"))
                         (population-size 10)
                         (export-lexicon-interval 10))
  (format t "~%Starting experimental runs")
  (run-batch-for-different-configurations
    :experiment-class 'grounded-color-naming-game-experiment 
    :number-of-interactions number-of-interactions
    :number-of-series number-of-series
    :monitors monitors
    :shared-configuration `((:population-size . ,population-size)
                            (:export-lexicon-interval . ,export-lexicon-interval))
    :configurations strategies
    :output-dir (babel-pathname :directory `("tutorial" "language-games" "grounded-color-naming-game" "raw-data")))
  (format t "~%Experimental runs finished and data has been generated. You can now plot graphs."))

(defun create-graph-for-single-strategy (&key experiment-name measure-names y-axis y1-max y2-max xlabel y1-label y2-label)
  ;; This function allows you to plot one or more measures for a single experiment
  ;; e.g. communicative success and lexicon size
  (format t "~%Creating graph for experiment ~a with measures ~a" experiment-name measure-names)
  (raw-files->evo-plot
    :raw-file-paths
    (loop for measure-name in measure-names
          collect `("tutorial" "language-games" "grounded-color-naming-game" "raw-data" ,experiment-name ,measure-name))
    :average-windows 1000
    :plot-directory `("tutorial" "language-games" "grounded-color-naming-game" "graphs")
    :error-bars '(:stdev)
    :error-bar-modes '(:lines)
    :use-y-axis y-axis
    :y1-min 0
    :y1-max y1-max
    :y2-min 0
    :y2-max y2-max
    :x-label (if xlabel xlabel "Number of Games")
    :y1-label (when y1-label y1-label)
    :y2-label (when y2-label y2-label))
  (format t "~%Graphs have been created"))

(defun create-graph-comparing-strategies (&key experiment-names measure-name (y-min 0) (y-max 1) xlabel y1-label y2-label)
  ;; This function allows you to compare a given measure accross different
  ;; experiments, e.g. comparing lexicon size
  (format t "~%Creating graph for experiments ~a with measure ~a" experiment-names measure-name)
  (raw-files->evo-plot
    :raw-file-paths
    (loop for experiment-name in experiment-names
          collect `("tutorial" "language-games" "grounded-color-naming-game" "raw-data" ,experiment-name ,measure-name))
    :average-windows 1000
    :captions experiment-names
    :plot-directory '("tutorial" "language-games" "grounded-color-naming-game" "graphs")
    :error-bars '(:stdev)
    :error-bar-modes '(:lines)
    :y1-min y-min
    :y1-max y-max
    :x-label (if xlabel xlabel "Number of Games")
    :y1-label (when y1-label y1-label)
    :y2-label (when y2-label y2-label))
  (format t "~%Graphs have been created"))
