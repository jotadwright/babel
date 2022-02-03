(in-package :mwm)

(defun run-experiments (strategies
                         &key
                         (number-of-interactions 5)
                         (number-of-series 1)
                         (monitors
                           (list "export-communicative-success"
                                 "export-lexicon-size"
                                 "export-features-per-form"
                                 "export-utterance-length"
                                 "export-lexicon-evolution"))
                         shared-configurations)
  (format t "~%Starting experimental runs")
  (run-batch-for-different-configurations
    :experiment-class 'mwm-experiment
    :number-of-interactions number-of-interactions
    :number-of-series number-of-series
    :monitors monitors
    :shared-configuration shared-configurations
    :named-configurations strategies
    :output-dir (babel-pathname :directory '("experiments" "multidimensional-word-meanings" "raw-data")))
  (format t "~%Experimental runs finished and data has been generated. You can now plot graphs."))

(defun create-graph-for-single-strategy (&key experiment-name measure-names average-windows
                                              y-axis (y1-min 0) y1-max y2-max xlabel y1-label y2-label
                                              captions (open t) start plot-file-name)
  ;; This function allows you to plot one or more measures for a single experiment
  ;; e.g. communicative success and lexicon size
  (format t "~%Creating graph for experiment ~a with measures ~a" experiment-name measure-names)
  (let* ((raw-file-paths
          (loop for measure-name in measure-names
                collect `("experiments" "multidimensional-word-meanings" "raw-data" ,experiment-name ,measure-name)))
         (default-plot-file-name
          (reduce #'(lambda (str1 str2) (string-append str1 "+" str2)) 
                  raw-file-paths  :key #'(lambda (path) (first (last path))))))
  (raw-files->evo-plot
    :raw-file-paths raw-file-paths
    :average-windows average-windows
    :plot-directory `("experiments" "multidimensional-word-meanings" "graphs")
    :plot-file-name (if plot-file-name plot-file-name default-plot-file-name)
    :error-bars '(:percentile 5 95) ; '(:stdev)
    :error-bar-modes '(:lines)
    :captions captions
    :use-y-axis y-axis
    :y1-min y1-min
    :y1-max y1-max
    :y2-min 0
    :y2-max y2-max
    :x-label (if xlabel xlabel "Number of Games")
    :y1-label (when y1-label y1-label)
    :y2-label (when y2-label y2-label)
    :open open
    :start start))
  (format t "~%Graphs have been created"))

(defun create-graph-comparing-strategies (&key experiment-names measure-name
                                               (y-min 0) (y-max 1) xlabel y1-label y2-label
                                               captions title start end (window 1000)
                                               plot-file-name)
  ;; This function allows you to compare a given measure accross different
  ;; experiments, e.g. comparing lexicon size
  (format t "~%Creating graph for experiments ~a with measure ~a" experiment-names measure-name)
  (let* ((raw-file-paths
          (loop for experiment-name in experiment-names
                collect `("experiments" "multidimensional-word-meanings" "raw-data" ,experiment-name ,measure-name)))
         (default-plot-file-name
          (reduce #'(lambda (str1 str2) (string-append str1 "+" str2)) 
                  raw-file-paths  :key #'(lambda (path) (first (last path))))))
  (raw-files->evo-plot
    :raw-file-paths raw-file-paths
    :average-windows window
    :captions (if captions captions experiment-names)
    :title title
    :plot-directory '("experiments" "multidimensional-word-meanings" "graphs")
    :plot-file-name (if plot-file-name plot-file-name default-plot-file-name)
    :error-bars '(:percentile 5 95) ;'(:stdev)
    :error-bar-modes '(:lines)
    :y1-min y-min
    :y1-max y-max
    :x-label (if xlabel xlabel "Number of Games")
    :y1-label (when y1-label y1-label)
    :y2-label (when y2-label y2-label)
    :start start :end end
    :fsize 12))
  (format t "~%Graphs have been created"))

(defun create-graph-mixing-strategies (&key experiment-measure-conses
                                            (y-min 0) (y-max 1) xlabel y1-label y2-label
                                            captions title start end (window 1000)
                                            plot-file-name)
  (let* ((raw-file-paths
          (loop for (experiment . measure) in experiment-measure-conses
                collect `("experiments" "multidimensional-word-meanings" "raw-data" ,experiment ,measure)))
         (default-plot-file-name
          (reduce #'(lambda (str1 str2) (string-append str1 "+" str2)) 
                  raw-file-paths  :key #'(lambda (path) (first (last path))))))
  (raw-files->evo-plot
   :raw-file-paths raw-file-paths
   :average-windows window
   :captions (if captions captions
               (loop for (experiment . measure) in experiment-measure-conses
                     collect (format nil "~a-~a" experiment measure)))
   :title title
   :plot-directory '("experiments" "multidimensional-word-meanings" "graphs")
   :plot-file-name (if plot-file-name plot-file-name default-plot-file-name)
   :error-bars '(:percentile 5 95) ;'(:stdev)
   :error-bar-modes '(:lines)
   :y1-min y-min
   :y1-max y-max
   :x-label (if xlabel xlabel "Number of Games")
   :y1-label (when y1-label y1-label)
   :y2-label (when y2-label y2-label)
   :start start :end end
   :fsize 12))
  (format t "~%Graphs have been created"))

(defun create-stacked-bars-comparing-strategies (&key experiment-names measure-names
                                                      (x-label "") (y-label "")
                                                      cluster-labels bar-labels
                                                      (title "") (open t)
                                                      (y-max nil))
  ;; This functions allows you to plot one or multiple measures for different
  ;; experiments. These will be plotted as stacked bars.
  (format t "~%Creating graph for experiments ~a with measures ~a" experiment-names measure-names)
  (let ((raw-file-paths
         (loop for experiment-name in experiment-names
               collect (loop for measure-name in measure-names
                             collect `("experiments" "multidimensional-word-meanings" "raw-data" "final" ,experiment-name ,measure-name)))))
    (file-structure->stacked-bar-plot
     :raw-file-paths raw-file-paths
     :plot-directory '("experiments" "multidimensional-word-meanings" "graphs")
     :x-label x-label
     :y-label y-label
     :title title
     :labels-a (if bar-labels
                 bar-labels
                 (mapcar #'(lambda (path) (first (last path))) (first raw-file-paths)))
     :labels-b (if cluster-labels
                 cluster-labels
                 (mapcar #'(lambda (path) (first (last (butlast (first path))))) raw-file-paths))
     :y-max y-max
     :open open)))

(defun create-grouped-bars-comparing-strategies (&key experiment-names measure-names
                                                      (x-label "") (y-label "")
                                                      cluster-labels bar-labels
                                                      (title "") (open t)
                                                      (y-max nil))
  ;; This functions allows you to plot one or multiple measures for different
  ;; experiments. These will be plotted as grouped bars.
  (format t "~%Creating graph for experiments ~a with measures ~a" experiment-names measure-names)
  (let ((raw-file-paths
         (loop for experiment-name in experiment-names
               collect (loop for measure-name in measure-names
                             collect `("experiments" "multidimensional-word-meanings" "raw-data" ,experiment-name ,measure-name)))))
    (file-structure->grouped-bar-plot
     :raw-file-paths raw-file-paths
     :plot-directory '("experiments" "multidimensional-word-meanings" "graphs")
     :x-label x-label :y-label y-label
     :title title
     :labels-a (if bar-labels bar-labels
                 (mapcar #'(lambda (path) (first (last path))) (first raw-file-paths)))
     :labels-b (if cluster-labels cluster-labels
                (mapcar #'(lambda (path) (first (last (butlast (first path))))) raw-file-paths))
     :error-bars :stdev
     :y-max y-max
     :open open)))