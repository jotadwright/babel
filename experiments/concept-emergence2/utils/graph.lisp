(in-package :cle)

;; ----------------------------------------
;; + Graph Creation and parallel batching +
;; ----------------------------------------

(defun create-graph-for-single-strategy (exp-dir experiment-name measure-names
                                         &rest evo-plot-keyword-args)
  ;; take some arguments, but pass along the rest to raw-files->evo-plot
  (format t "~%Creating graph for experiment ~a with measures ~a" experiment-name measure-names)
  (let* ((raw-file-paths
          (loop for measure-name in measure-names
                collect `("experiments" "concept-emergence2" "logging" ,exp-dir "experiments" ,experiment-name ,measure-name)))
         (default-plot-file-name
          (reduce #'(lambda (str1 str2) (string-append str1 "+" str2)) 
                  raw-file-paths  :key #'(lambda (path) (first (last path)))))
         (plot-file-name
          (when (find :plot-file-name evo-plot-keyword-args)
            (nth (1+ (position :plot-file-name evo-plot-keyword-args)) evo-plot-keyword-args))))
    (apply #'raw-files->evo-plot
           (append `(:raw-file-paths ,raw-file-paths
                     :plot-directory ("experiments" "concept-emergence2" "logging" ,exp-dir "experiments" ,experiment-name)
                     :plot-file-name ,(if plot-file-name plot-file-name default-plot-file-name))
                   evo-plot-keyword-args)))
  (format t "~%Graphs have been created."))

(defun create-graph-comparing-strategies (&key title base-dir experiment-names measure-name (y-max 1) (y-min 0) (start nil) (end nil) xlabel y1-label y2-label captions average-windows plot-file-name)
  ;; This function allows you to compare a given measure accross different
  ;; experiments, e.g. comparing lexicon size
  (format t "~%Creating graph for experiments ~a with measure ~a" experiment-names measure-name)
  (raw-files->evo-plot
    :title title
    :raw-file-paths
    (loop for experiment-name in experiment-names
          collect `("experiments" "concept-emergence2" "logging" ,base-dir "experiments" ,experiment-name ,measure-name))
    :average-windows average-windows
    :captions captions
    :plot-directory `("experiments" "concept-emergence2" "logging" ,base-dir "plots")
    :plot-file-name plot-file-name
    :error-bars '(:stdev)
    :error-bar-modes '(:lines)
    ;:series-numbers '(0 1)
    :y1-min y-min
    :y1-max y-max
    :start start
    :end end
    :x-label (if xlabel xlabel "Number of Games")
    :y1-label (when y1-label y1-label)
    :y2-label (when y2-label y2-label))
  (format t "~%Graphs have been created"))