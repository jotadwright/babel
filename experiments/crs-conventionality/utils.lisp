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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            ;;
;;  Data loading and storing  ;;
;;                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-path (dataset-name dataset-split)
  "Creates path for concept emergence experiments."
  (merge-pathnames
   (make-pathname :directory `(:relative
                               "experiments"
                               "crs-conventionality"
                               "data"
                               ,dataset-name)
                  :name (format nil
                                "~a-~a"
                                dataset-name
                                dataset-split)
                  :type "jsonl")
   cl-user::*babel-path*))

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


(defun merge-batch (measure-names exp-top-dir datasplit exp-name)
  (let ((batch-directory (babel-pathname :directory `("experiments"
                                                      "crs-conventionality"
                                                      "logging"
                                                      ,exp-top-dir
                                                      ,datasplit
                                                      ,exp-name))))
    (loop for measure-name in measure-names
          for merged-data = (list (loop for dir in (uiop:subdirectories batch-directory)
                                        for path = (merge-pathnames (make-pathname :name measure-name :type "lisp") dir)
                                        if (probe-file path)
                                          collect (with-open-file (stream path)
                                                    (caar (read stream)))))
          do (with-open-file (file
                              (ensure-directories-exist (babel-pathname
                                                         :directory `("experiments"
                                                                      "crs-conventionality"
                                                                      "logging"
                                                                      ,exp-top-dir
                                                                      ,datasplit
                                                                      ,exp-name
                                                                      "merged-data")
                                                         :name measure-name
                                                         :type "lisp"))
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
               (write-sequence (format nil "~a" merged-data) file)))))

(defun restore-and-run-new-config (path new-configs &key
                                   (number-of-interactions 1000)
                                   (number-of-series 1)
                                   (monitors (list "log-every-x-interactions-in-output-browser"
                                                   "export-communicative-success"
                                                   "export-conventionalisation"
                                                   "export-construction-inventory-size")))
  (let ((experiment (restore-experiment path)))
         
    (set-configurations experiment new-configs)
    (let ((config (loop for k being the hash-key in (configuration experiment) using (hash-value v)
                       collect (cons k v))))
      (set-up-monitors monitors config)
      
      (loop for series from 1 to number-of-series
            do (run-series experiment (+ 1 number-of-interactions))
               (notify series-finished series))
      (notify batch-finished (symbol-name (type-of experiment))))))

(defun create-standalone-legend (file-name directory captions
                                &key (graphic-type "pdf") (fsize 10))
  "Creates a separate file containing just the legend"
  (with-open-stream (stream (monitors::pipe-to-gnuplot))
    (format stream "set terminal ~A font '~A,~D'" graphic-type "Helvetica" fsize)
    (format stream "~%set output '~A'"
            (namestring (babel-pathname :directory directory
                                      :name (format nil "~A-legend" file-name)
                                      :type graphic-type)))
    (format stream "~%set key horizontal center")
    (format stream "~%plot ")
    (loop for caption in captions
          for i from 1
          do (format stream "2 title '~A' with lines~A"
                    caption
                    (if (< i (length captions)) "," "")))
    (format stream "~%exit~%")
    (finish-output stream)))

(defun create-standalone-labels (file-name directory x-label y1-label y2-label
                                &key (graphic-type "pdf") (fsize 10))
  "Creates a separate file containing just the axis labels"
  (with-open-stream (stream (monitors::pipe-to-gnuplot))
    (format stream "set terminal ~A font '~A,~D'" graphic-type "Helvetica" fsize)
    (format stream "~%set output '~A'"
            (namestring (babel-pathname :directory directory
                                      :name (format nil "~A-labels" file-name)
                                      :type graphic-type)))
    (format stream "~%set xlabel '~A'" x-label)
    (when y1-label
      (format stream "~%set ylabel '~A'" y1-label))
    (when y2-label
      (format stream "~%set y2label '~A'" y2-label))
    (format stream "~%plot 2")
    (format stream "~%exit~%")
    (finish-output stream)))
    
        

