(setf cl-user::*automatically-start-web-interface* nil)
(setf test-framework::*dont-run-tests-when-loading-asdf-systems* t)

(ql:quickload :cle)
(in-package :cle)

(defun parse-config (args)
  (let ((config (loop for (a b) on args by #'cddr
                      collect (cons (parse-keyword a) (read-from-string b)))))
    (when (assoc :available-channels config)
      (rplacd (assoc :available-channels config)
              (get-all-channels (assqv :available-channels config))))
    (loop for (key . val) in config
          when (find key (list :exp-name :dataset :dataset-split))
            do (rplacd (assoc key config)
                       (string-downcase (string (assqv key config)))))
    config))

(defun fixed-config ()
  `(;; fixed in stone
    ;; --------------
    (:dot-interval . 1000)
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
      :heap-size 12248))))

(run-parallel #+sbcl (rest sb-ext:*posix-argv*))
