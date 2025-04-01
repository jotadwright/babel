(setf cl-user::*automatically-start-web-interface* nil)
(setf test-framework::*dont-run-tests-when-loading-asdf-systems* t)

(ql:quickload :cle)
(in-package :cle)

(defun fixed-config ()
  `(;; fixed in stone
    ;; --------------
    (:align . nil) ;; disable alignment
    (:usage-table-window . nil) ;; use full usage table
    ))

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
          when (find key (list :dataset :feature-set))
            ;; loop through strings in val and downcase theme
            do (rplacd (assoc key config)
                       (mapcar #'string-downcase val)))
    (when (assoc :stage-parameters config)
      (let ((stage-params (assqv :stage-parameters config)))
        (loop for stage-param in stage-params
              do (when (assoc :switch-dataset stage-param)
                   (rplacd (assoc :switch-dataset stage-param)
                           (string-downcase (string (assqv :switch-dataset stage-param)))))
              do (when (assoc :switch-dataset-split stage-param)
                   (rplacd (assoc :switch-dataset-split stage-param)
                           (string-downcase (string (assqv :switch-dataset-split stage-param)))))
              do (when (assoc :switch-feature-set stage-param)
                   (rplacd (assoc :switch-feature-set stage-param)
                           (string-downcase (string (assqv :switch-feature-set stage-param))))))))

    ;; append the to the fixed configuration
    (setf config (append (fixed-config) config))
    ;; finally create a configuration object
    (make-configuration :entries config)))

(defun test-experiment (args)
  (let* ((configuration (parse-config args))
         (fname (format nil "seed-~a" (get-configuration configuration :seed)))
         (experiment (cl-store:restore
                      (babel-pathname :directory `("experiments"
                                                   "concept-emergence2"
                                                   "storage"
                                                   ,(get-configuration configuration :exp-top-dir)
                                                   "stores"
                                                   ,(get-configuration configuration :exp-name)
                                                   )
                                      :name fname
                                      :type "store"))))
    (format t "~% EXP-NAME = ~a w/ seed ~a"
            (get-configuration configuration :exp-name)
            (get-configuration configuration :seed)) ;; log exp-name
    
    ;; set random seed
    (set-seed (get-configuration experiment :seed)) ;; set random state
    ;; ovewrite experiment config with parsed config
    (set-configuration experiment :scene-sampling (get-configuration configuration :scene-sampling))
    (set-configuration experiment :topic-sampling (get-configuration configuration :topic-sampling))
    (set-configuration experiment :dataset (get-configuration configuration :dataset))
    (set-configuration experiment :dataset-split (get-configuration configuration :dataset-split))
    (set-configuration experiment :feature-set (get-configuration configuration :feature-set))
    (set-configuration experiment :dataset-loader (get-configuration configuration :dataset-loader))
    (set-configuration experiment :min-context-size (get-configuration configuration :min-context-size))
    (set-configuration experiment :max-context-size (get-configuration configuration :max-context-size))
    (set-configuration experiment :nr-of-interactions (get-configuration configuration :nr-of-interactions))
    ;; overwrite experiment config with fixed config
    (set-configuration experiment :usage-table-window (get-configuration configuration :usage-table-window))
    (set-configuration experiment :align (get-configuration configuration :align))

    ;; reset usage tables
    (loop for agent in (agents experiment)
          do (setf (usage-table agent) (create-usage-table (get-configuration configuration :usage-table-window))))
    ;; initialise the world
    (initialise-world experiment)

    ;; set-up monitors
    (set-up-monitors (list "record-communicative-success"
                           "record-conventionalisation"
                           "export-communicative-success"
                           "export-conventionalisation"
                           "export-construction-inventory-window"
                           "export-experiment-configurations"
                           "log-every-x-interactions-in-output-browser")
                     (list "test")
                     experiment)

    ;; run experiment
    (time
     (let ((nr-of-interactions (get-configuration experiment :nr-of-interactions)))
       ;; create first interaction object
       (run-interaction experiment)
       (while (< (interaction-number (current-interaction experiment)) nr-of-interactions)
              do (run-interaction experiment))
       (notify run-series-finished experiment)
       (notify series-finished 1)
       (notify batch-finished (class-string experiment))))
    (format t "~%~% == Completed experiment.~%~%")))
      
(test-experiment #+sbcl (rest sb-ext:*posix-argv*))
