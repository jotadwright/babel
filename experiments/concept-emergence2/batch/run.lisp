(setf cl-user::*automatically-start-web-interface* nil)
(setf test-framework::*dont-run-tests-when-loading-asdf-systems* t)

(ql:quickload :cle)
(in-package :cle)

(defun fixed-config ()
  `(;; fixed in stone
    ;; --------------
    (:log-every-x-interactions . 5000)
    ;(:record-every-x-interactions . 100)
    (:usage-table-window . 5000)
    (:save-distribution-history . nil)
    (:initial-cxn-entrenchement . 0.5)
    ;; parameter for updating continuous distributions (gaussian-welford)
    (:M2 . 0.0001)))

(defun parse-config (args)
  (let ((config (loop for (a b) on args by #'cddr
                      collect (cons (parse-keyword a) (read-from-string b)))))
    ;; make sure that the keys required in paths are lowercase
    (loop for (key . val) in config
          when (find key (list :exp-name :dataset-split :exp-top-dir))
            do (rplacd (assoc key config)
                       (string-downcase (string (assqv key config)))))
    ;; lists of strings should also be downcase
    (loop for (key . val) in config
          when (find key (list :dataset))
            ;; loop through strings in val and downcase theme
            do (rplacd (assoc key config)
                       (mapcar #'string-downcase val)))

    ;; generate a unique log dir name
    (setf config (append config (list (cons :log-dir-name (generate-log-dir-name (assqv :seed config))))))
    ;; append the to the fixed configuration
    (setf config (append (fixed-config) config))
    ;; finally create a configuration object
    (make-configuration :entries config)))

(defun run-experiment (args)
  ;; parse command line arguments, 
  (let* ((configuration (parse-config args)))
    ;; adapt file-writing monitors so they output in the correct log-dir
    (set-up-monitors (list ;; record
                           "record-communicative-success"
                           "record-conventionalisation"
                           "record-construction-inventory-usage"
                           ;"record-partner-selection"
                           ;; export
                           "export-communicative-success"
                           "export-conventionalisation"
                           "export-construction-inventory-usage"
                           ;"export-partner-selection"
                           ;; export configs, experiments
                           "export-experiment-configurations"
                           "export-experiment-store"
                           ;; logger
                           "log-every-x-interactions-in-output-browser")
                     (list "train" "val")
                     configuration)

    ;; Run experiment
    (format t "~%~% == Running the experiment, log at 'logging/~a/~a/~a'.~%"
            (get-configuration configuration :exp-top-dir)
            (get-configuration configuration :exp-name)
            (get-configuration configuration :log-dir-name))
    
    (time
     (let ((experiment (make-instance 'cle-experiment :configuration configuration))
           (nr-of-interactions (get-configuration configuration :nr-of-interactions)))
       ;; create first interaction object
       (run-interaction experiment)
       (while (< (interaction-number (current-interaction experiment)) nr-of-interactions)
              do (run-interaction experiment))
       (notify run-series-finished experiment)
       (notify series-finished 1)
       (notify batch-finished (class-string experiment))))
    (format t "~%~% == Completed experiment.~%~%")))

(run-experiment #+sbcl (rest sb-ext:*posix-argv*))
(sb-ext:quit)