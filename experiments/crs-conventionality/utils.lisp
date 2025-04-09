(in-package :crs-conventionality)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                         ;;
;;  Helper functions for running and plotting experiments  ;;
;;                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-experiments-sequentially (experiment-class
                                     &key
                                     strategies
                                     number-of-interactions
                                     monitors
                                     (number-of-series 1))
  "Runs a set of experiments (sequentially)."
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
  "Creates a plot of the evolutionary dynamics from a given experiment (using exported data)."
  (format t "~%Creating graph for experiment ~a with measures ~a" experiment-name measure-names)
  (let* ((raw-file-paths
          (loop for measure-name in measure-names
                collect `("experiments" "crs-conventionality" "raw-data", experiment-name ,measure-name)))  ;; ,experiment-name
         (default-plot-file-name
          (reduce #'(lambda (str1 str2) (string-append str1 "+" str2)) 
                  raw-file-paths :key #'(lambda (path) (first (last path)))))
         (plot-file-name
          (when (find :plot-file-name evo-plot-keyword-args)
            (nth (1+ (position :plot-file-name evo-plot-keyword-args)) evo-plot-keyword-args))))
    (apply #'raw-files->evo-plot
           (append `(:raw-file-paths ,raw-file-paths
                     :plot-directory ("experiments" "crs-conventionality" "plots" "naming-game-alignment-strategies")
                     :plot-file-name ,(if plot-file-name plot-file-name default-plot-file-name))
                   evo-plot-keyword-args)))
  (format t "~%Graphs have been created."))

(defun create-graph-for-single-measure (measure-name experiment-names
                                                     &rest evo-plot-keyword-args)
  "Creates a plot for a single measure from several experiments (using exported data)."
  (format t "~%Creating graph for measure ~a from experiments ~a" measure-name experiment-names)
  (let* ((raw-file-paths
          (loop for experiment-name in experiment-names
                collect `("experiments" "crs-conventionality" "raw-data", experiment-name , measure-name)))  ;; ,experiment-name
         (default-plot-file-name
          (reduce #'(lambda (str1 str2) (string-append str1 "+" str2)) 
                  raw-file-paths :key #'(lambda (path) (first (last path)))))
         (plot-file-name
          (when (find :plot-file-name evo-plot-keyword-args)
            (nth (1+ (position :plot-file-name evo-plot-keyword-args)) evo-plot-keyword-args))))
    (apply #'raw-files->evo-plot
           (append `(:raw-file-paths ,raw-file-paths
                     :plot-directory ("experiments" "crs-conventionality" "plots" "naming-game-alignment-strategies")
                     :plot-file-name ,(if plot-file-name plot-file-name default-plot-file-name))
                   evo-plot-keyword-args)))
  (format t "~%Graphs have been created."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                     ;;
;;  Helper functions for hash tables   ;;
;;                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hash-keys (ht)
  (loop for key being the hash-keys of ht
        collect key))

(defun hash-values (ht)
  (loop for value being the hash-values of ht
        collect value))

;;  Data loading utils  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-path (dataset-name dataset-split)
  "Creates path for concept emergence experiments."
  (merge-pathnames
   (make-pathname :directory `(:relative
                               "concept-emergence2" ;; ADAPT
                               "split-by-entities"  ;; ADAPT
                               ,dataset-name)
                  :name (format nil
                                "~a-~a"
                                dataset-name
                                dataset-split)
                  :type "jsonl")
   cl-user:*babel-corpora*))

(defun get-current-date ()
  (multiple-value-bind
      (second minute hour day month year day-of-week dst-p tz)
      (get-decoded-time)
    (format nil "~d-~2,'0d-~d_~dh~dm~ds" year month day hour minute second)))

(defun generate-log-dir-name (seed)
  ;; set a random seed to generate the 5-character random number (to avoid collisions) 
  (set-seed -1)
  ;; create a log-dir-name based on the current-data, the seed, and the random number
  (mkstr (internal-symb (list-of-strings->string
                         (list (get-current-date)
                               (mkstr (format nil "seed~a" seed))
                               (mkstr (random 10) (random 10) (random 10) (random 10) (random 10)))
                         :separator "-"))))

(defun parse-keyword (string)
  (intern (string-upcase (string-left-trim ":" string)) :keyword))

(defun set-up-monitors (monitors config)
  (monitors::deactivate-all-monitors)
  (loop for monitor-string in monitors
        for monitor = (monitors::get-monitor (read-from-string monitor-string))
        do (monitors::activate-monitor-method (read-from-string monitor-string))
        when (slot-exists-p monitor 'file-name)
          do (setf (slot-value monitor 'file-name)
                    (ensure-directories-exist
                    (merge-pathnames (make-pathname :directory `(:relative ,(assqv :log-dir-name config))
                                                    :name (pathname-name (file-name monitor)) 
                                                    :type (pathname-type (file-name monitor)))
                                      (babel-pathname :directory `("experiments"
                                                                  "crs-conventionality"
                                                                  "logging"
                                                                  ,(assqv :exp-top-dir config)
                                                                  ,(assqv :datasplit config)
                                                                  ,(assqv :exp-name config))))))))

(defun run-batch-and-save (experiment-class 
                           number-of-interactions
                           number-of-series
                           &rest experiment-parameters)
  "Runs a series of experiments. Each time a new instance of experiment
   is created, saved once finished."
  (notify reset-monitors)
  (set-seed (assqv :seed experiment-paramenters))
  (loop for series from 1 to number-of-series
        for experiment = (apply 'make-instance
                                experiment-class
                                :series-number series                                
                                experiment-parameters)
        do (run-series experiment (+ 1 number-of-interactions))
           (cl-store:store 
                (merge-pathnames
                 (make-pathname :name (format nil "~a-~a" (assqv :exp-name experiment-parameters) series) :type #+lispworks "lw.store" #+ccl "ccl.store" #+sbcl "sbcl.store")
                 (babel-pathname :directory `("experiments"
                                              "crs-conventionality"
                                              "logging"
                                              ,(assqv :exp-top-dir experiment-parameters)
                                              ,(assqv :datasplit experiment-parameters)
                                              ,(assqv :exp-name experiment-parameters)))))
           (notify series-finished series))
  (notify batch-finished (symbol-name experiment-class)))