(setf cl-user::*automatically-start-web-interface* nil)
(setf test-framework::*dont-run-tests-when-loading-asdf-systems* t)

(ql:quickload :cle)
(in-package :cle)

(defun parse-config (args)
  (let ((config (loop for (a b) on args by #'cddr
                      collect (cons (parse-keyword a) (read-from-string b)))))
    (loop for (key . val) in config
          when (find key (list :exp-name :dataset :dataset-split :exp-top-dir :feature-set :log-dir-name))
            do (rplacd (assoc key config)
                       (string-downcase (string (assqv key config)))))
    config))

(defun fixed-config ()
  `(;; fixed in stone
    ;; --------------
    (:align . nil) ;; disable alignment
    (:usage-table-window . nil) ;; use full usage table
    ))

(defun test-experiment (args)
  (let* ((config (append (fixed-config)
                         (parse-config args)))
         (fname (format nil "seed-~a" (assqv :seed config)))
         (experiment (cl-store:restore
                      (babel-pathname :directory `("experiments"
                                                   "concept-emergence2"
                                                   "storage"
                                                   ,(assqv :exp-top-dir config)
                                                   "stores"
                                                   ,(assqv :exp-name config)
                                                   )
                                      :name fname
                                      :type "store"))))
    (format t "~% EXP-NAME = ~a w/ seed ~a"
            (assqv :exp-name config)
            (assqv :seed config)) ;; log exp-name
    
    ;; set random seed
    (set-seed (get-configuration experiment :seed)) ;; set random state
    ;; set-up experiment
    (set-configuration experiment :scene-sampling (assqv :scene-sampling config))
    (set-configuration experiment :topic-sampling (assqv :topic-sampling config))
    (set-configuration experiment :dataset (assqv :dataset config))
    (set-configuration experiment :dataset-split (assqv :dataset-split config))
    (set-configuration experiment :feature-set (assqv :feature-set config))
    ;; update experiment config for correct logging
    (set-configuration experiment :usage-table-window (assqv :usage-table-window config))
    (set-configuration experiment :align (assqv :align config))
    (set-configuration experiment :nr-of-interactions (assqv :nr-of-interactions config))

    ;; reset usage tables
    (loop for agent in (agents experiment)
          do (setf (usage-table agent) (create-usage-table (assqv :usage-table-window config))))
    ;; initialise the world
    (initialise-world experiment)

    ;; set-up monitors
    (set-up-monitors (list "export-communicative-success"
                           "export-lexicon-coherence"
                           "export-lexicon-inventory-usage"
                           "export-experiment-configurations"
                           "print-a-dot-for-each-interaction")
                     (entries experiment))

    ;; run experiment
    (time
      (progn
      (notify reset-monitors)
      (run-series experiment (+ 1 (assqv :nr-of-interactions config)))
      (notify series-finished 1)
      (notify batch-finished (class-string experiment))))))
      
(test-experiment #+sbcl (rest sb-ext:*posix-argv*))
