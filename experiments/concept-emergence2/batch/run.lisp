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
          when (find key (list :exp-name :dataset :dataset-split))
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

(defun run-parallel (args)
  (let* ((config (append (fixed-config)
                         (parse-config args))))
    (time
     (run-parallel-batch-for-grid-search
      :asdf-system "cle"
      :package "cle"
      :experiment-class "cle-experiment"
      :number-of-interactions (assqv :nr-of-interactions config)
      :number-of-series (assqv :nr-of-series config)
      :monitors (list "export-communicative-success"
                      "export-lexicon-coherence"
                      "export-unique-form-usage"
                      "export-experiment-configurations"
                      "export-experiment-store"
                      "print-a-dot-for-each-interaction")
      ;; actual configuration
      :shared-configuration config
      ;; configuration is empty as we are not grid searching
      :configurations `()
      ;; output directory
      :output-dir (babel-pathname :directory `("experiments"
                                               "concept-emergence2"
                                               "logging"
                                               ,(assqv :exp-name config)))
      :heap-size 60000))))

(run-parallel #+sbcl (rest sb-ext:*posix-argv*))
