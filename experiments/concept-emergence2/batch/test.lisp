(setf cl-user::*automatically-start-web-interface* nil)
(setf test-framework::*dont-run-tests-when-loading-asdf-systems* t)

(ql:quickload :cle)
(in-package :cle)

(defun fixed-config ()
  `(;; fixed in stone
    ;; --------------
    (:align . nil) ;; disable alignment
    (:usage-tracker-window . nil) ;; use full usage table
    ))


(defun parse-config (args)
  (let ((config (loop for (a b) on args by #'cddr
                      collect (cons (parse-keyword a) (read-from-string b)))))
    (loop for (key . val) in config
          when (find key (list :exp-name :dataset-split :exp-top-dir :log-dir-name))
            do (rplacd (assoc key config)
                       (string-downcase (string (assqv key config)))))
    (loop for (key . val) in config
          when (find key (list :dataset :feature-set))
            ;; loop through strings in val and downcase theme
            do (rplacd (assoc key config)
                       (mapcar #'string-downcase val)))

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
    ;; set-up experiment
    (set-configuration experiment :scene-sampling (get-configuration configuration :scene-sampling))
    (set-configuration experiment :topic-sampling (get-configuration configuration :topic-sampling))
    (set-configuration experiment :dataset (get-configuration configuration :dataset))
    (set-configuration experiment :dataset-split (get-configuration configuration :dataset-split))
    (set-configuration experiment :feature-set (get-configuration configuration :feature-set))
    (set-configuration experiment :dataset-loader (get-configuration configuration :dataset-loader))
    (set-configuration experiment :min-context-size (get-configuration configuration :min-context-size))
    (set-configuration experiment :max-context-size (get-configuration configuration :max-context-size))
    ;; update experiment config for correct logging
    (set-configuration experiment :usage-tracker-window (get-configuration configuration :usage-tracker-window))
    (set-configuration experiment :align (get-configuration configuration :align))
    (set-configuration experiment :nr-of-interactions (get-configuration configuration :nr-of-interactions))

    ;; reset usage tables
    (loop for agent in (agents experiment)
          do (setf (usage-tracker agent) (create-usage-tracker (get-configuration configuration :usage-tracker-window))))
    
    ;; initialise the world
    (initialise-world experiment)

    ;; set-up monitors
    (set-up-monitors (list "export-communicative-success"
                           "export-conventionalisation"
                           "export-construction-inventory-usage-test"
                           "export-experiment-configurations"
                           "log-every-x-interactions-in-output-browser")
                     (entries experiment))

    ;; run experiment
    (time
     (progn
       (notify reset-monitors)
       (run-series experiment (+ 1 (get-configuration configuration :nr-of-interactions)))
       (notify series-finished 1)
       (notify batch-finished (class-string experiment))))
    (format t "~%~% == Completed experiment.~%~%")))

      
(test-experiment #+sbcl (rest sb-ext:*posix-argv*))
