(setf cl-user::*automatically-start-web-interface* nil)
(setf test-framework::*dont-run-tests-when-loading-asdf-systems* t)

(ql:quickload :cle)
(in-package :cle)

(defun parse-config (args)
  (let ((config (loop for (a b) on args by #'cddr
                      collect (cons (parse-keyword a) (read-from-string b)))))
    (when (and (assoc :available-channels config) (keywordp (assqv :available-channels config)))
      (rplacd (assoc :available-channels config)
              (get-all-channels (assqv :available-channels config))))
    (loop for (key . val) in config
          when (find key (list :exp-name :dataset :dataset-split :exp-top-dir))
            do (rplacd (assoc key config)
                       (string-downcase (string (assqv key config)))))
    (when (assoc :stage-parameters config)
      (let ((stage-params (assqv :stage-parameters config)))
        (loop for stage-param in stage-params
              do (when (assoc :switch-dataset stage-param)
                   (rplacd (assoc :switch-dataset stage-param)
                           (string-downcase (string (assqv :switch-dataset stage-param)))))
              do (when (assoc :switch-dataset-split stage-param)
                   (rplacd (assoc :switch-dataset-split stage-param)
                           (string-downcase (string (assqv :switch-dataset-split stage-param)))))
              do (when (and (assoc :switch-available-channels stage-param) (keywordp (assqv :switch-available-channels stage-param)))
                   (rplacd (assoc :switch-available-channels stage-param)
                           (get-all-channels (assqv :switch-available-channels stage-param))))
                 )))
    config))

(defun fixed-config ()
  `(;; fixed in stone
    ;; --------------
    (:dot-interval . 5000)
    ;(:record-every-x-interactions . 100)
    (:usage-table-window . 5000)
    (:save-distribution-history . nil)
    (:interacting-agents-strategy . :standard)
    (:initial-cxn-entrenchement . 0.5)
    ;; concept representations
    (:concept-representation . :distribution)
    (:distribution . :gaussian-welford)
    (:M2 . 0.0001)))

(defun get-monitors ()
  (list "export-communicative-success"
        "export-lexicon-coherence"
        "export-unique-form-usage"
        "export-experiment-configurations"
        "export-experiment-store"
        "print-a-dot-for-each-interaction"))

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
                                                                  "concept-emergence2"
                                                                  "logging"
                                                                  ,(assqv :exp-top-dir config)
                                                                  ,(assqv :exp-name config))))))))

(defun run-experiment (args)
  (let* (;; parse command line arguments, append it to the fixed configuration
         (config (append (fixed-config) (parse-config args)))
         ;; generate a log-dir-name
         (log-dir-name (generate-log-dir-name (assqv :seed config))))
    ;; add log-dir-name to configuration
    (setf config (append config (list (cons :log-dir-name log-dir-name))))
    ;; adapt file-writing monitors so they output in the correct log-dir
    (set-up-monitors (get-monitors) config)

    ;; Run experiment
    (format t "~%~% == Running the experiment, log at 'logging/~a/~a/~a'.~%"
            (assqv :exp-top-dir config)
            (assqv :exp-name config)
            (assqv :log-dir-name config))
    (time
     (run-batch 'cle-experiment (assqv :nr-of-interactions config) 1
                :configuration (make-configuration :entries config))
     )
    (format t "~%~% == Completed experiment.~%~%")
    ))

(run-experiment #+sbcl (rest sb-ext:*posix-argv*))
