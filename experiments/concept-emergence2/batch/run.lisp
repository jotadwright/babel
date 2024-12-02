(setf cl-user::*automatically-start-web-interface* nil)
(setf test-framework::*dont-run-tests-when-loading-asdf-systems* t)

(ql:quickload :cle)
(in-package :cle)

(defun parse-config (args)
  (let ((config (loop for (a b) on args by #'cddr
                      collect (cons (parse-keyword a) (read-from-string b)))))
    ;; make sure that the keys required in paths are lowercase
    (loop for (key . val) in config
          when (find key (list :exp-name :dataset :dataset-split :exp-top-dir :feature-set))
            do (rplacd (assoc key config)
                       (string-downcase (string (assqv key config)))))
    ;; make sure that the keys required in paths are lowercase
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

(defun run-experiment (args)
  (let* (;; parse command line arguments, append it to the fixed configuration
         (config (append (fixed-config) (parse-config args)))
         ;; generate a log-dir-name
         (log-dir-name (generate-log-dir-name (assqv :seed config))))
    ;; add log-dir-name to configuration
    (setf config (append config (list (cons :log-dir-name log-dir-name))))
    ;; adapt file-writing monitors so they output in the correct log-dir
    (set-up-monitors (list "export-communicative-success"
                           "export-communicative-success-given-conceptualisation"
                           "export-lexicon-coherence"
                           "export-unique-form-usage"
                           "export-experiment-configurations"
                           "export-experiment-store"
                           "print-a-dot-for-each-interaction")
                     config)

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
(sb-ext:quit)