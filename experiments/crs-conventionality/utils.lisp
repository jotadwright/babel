(in-package :crs-conventionality)

;; Helper functions for running and plotting experiments

(defun run-experiments-sequentially (experiment-class
                        &key
                        strategies
                        number-of-interactions
                        monitors
                        (number-of-series 1))
  
  (format t "~%Starting experimental runs")
  (run-batch-for-different-configurations
   :experiment-class experiment-class
   :number-of-interactions number-of-interactions
   :number-of-series number-of-series
   :named-configurations strategies
   :shared-configuration nil
   :monitors monitors
   :output-dir (babel-pathname :directory '("experiments" "crs-conventionality" "raw-data")))
  (format t "~%Experimental runs finished and data has been generated. You can now plot graphs."))

(defun create-graph-for-single-strategy (experiment-name measure-names
                                         &rest evo-plot-keyword-args)
  ;; take some arguments, but pass along the rest to raw-files->evo-plot
  (format t "~%Creating graph for experiment ~a with measures ~a" experiment-name measure-names)
  (let* ((raw-file-paths
          (loop for measure-name in measure-names
                collect `("experiments" "crs-conventionality" "raw-data" ,measure-name)))  ;; ,experiment-name
         (default-plot-file-name
          (reduce #'(lambda (str1 str2) (string-append str1 "+" str2)) 
                  raw-file-paths :key #'(lambda (path) (first (last path)))))
         (plot-file-name
          (when (find :plot-file-name evo-plot-keyword-args)
            (nth (1+ (position :plot-file-name evo-plot-keyword-args)) evo-plot-keyword-args))))
    (apply #'raw-files->evo-plot
           (append `(:raw-file-paths ,raw-file-paths
                     :plot-directory ("experiments" "crs-conventionality" "raw-data" ,experiment-name)
                     :plot-file-name ,(if plot-file-name plot-file-name default-plot-file-name))
                   evo-plot-keyword-args)))
  (format t "~%Graphs have been created."))
