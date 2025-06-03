(in-package :cle)

;; ------------
;; + Monitors +
;; ------------

;; -------------------------------------------------------
;; + Log information to output-browser (standard output) +
;; -------------------------------------------------------
(defvar *start-time* nil)

(defun log-every-x-interactions-in-output-browser (experiment interaction split)
  (let ((log-every-x-interactions (get-configuration experiment :log-every-x-interactions)))
    (when (string= split "VAL")
      (setf log-every-x-interactions (floor (/ log-every-x-interactions 5))))
    (cond ((or (= (interaction-number interaction) 1) (not *start-time*))
           (setf *start-time* (get-universal-time)))
          ((= (mod (interaction-number interaction) log-every-x-interactions)
              0)
           (let ((communicative-success (caaar (monitors::get-average-values (monitors::get-monitor (intern (format nil "RECORD-COMMUNICATIVE-SUCCESS-~a" split))))))
                 (conventionalisation (caaar (monitors::get-average-values (monitors::get-monitor (intern (format nil "RECORD-CONVENTIONALISATION-~a" split)))))))
             (multiple-value-bind (h m s) (seconds-to-hours-minutes-seconds (- (get-universal-time) *start-time*))
               (format t
                       ". (~a / ~a / ~a / ~ah ~am ~as)~%"
                       (interaction-number interaction)
                       (if communicative-success
                         (format nil "~,vf%" 1 (* 100 (float communicative-success)))
                         "NIL")
                       (if conventionalisation
                         (format nil "~,vf%" 1 (* 100 (float conventionalisation)))
                         "NIL")
                       h m s)))
           (setf *start-time* (get-universal-time))))))

(define-monitor log-every-x-interactions-in-output-browser
                :documentation "Logs measures every x interactions in the output-browser")

(define-event-handler (log-every-x-interactions-in-output-browser interaction-finished)
  (let ((dataset-split (get-configuration experiment :dataset-split)))
    (log-every-x-interactions-in-output-browser experiment interaction (string-upcase dataset-split))))

;; -------------------
;; + Helper function +
;; -------------------

(defun retrieve-monitor (experiment name)
  (let* ((dataset-split (get-configuration experiment :dataset-split))
         (monitor (monitors::get-monitor (intern (string-upcase (format nil "~a-~a" name dataset-split))))))
    monitor))

;; -------------------------
;; + Communicative success +
;; -------------------------

(defun record-communicative-success (experiment interaction)
  (let ((monitor (retrieve-monitor experiment "record-communicative-success")))
    (record-value monitor (if (communicated-successfully interaction) 1 0))))

;; train
(define-monitor record-communicative-success-train
                :class 'data-recorder
                :average-window 1000
                :documentation "Records the game outcome of each game (1 or 0).")

(define-monitor export-communicative-success-train
                :class 'csv-data-file-writer
                :documentation "Exports communicative success."
                :data-sources '(record-communicative-success-train)
                :file-name (babel-pathname :name "communicative-success-train" :type "csv"
                                           :directory '("experiments" "concept-emergence2" "logging"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-communicative-success-train interaction-finished)
  (record-communicative-success experiment interaction))

;; val
(define-monitor record-communicative-success-val
                :class 'data-recorder
                :average-window 1000
                :documentation "Records the game outcome of each game (1 or 0).")

(define-monitor export-communicative-success-val
                :class 'csv-data-file-writer
                :documentation "Exports communicative success."
                :data-sources '(record-communicative-success-val)
                :file-name (babel-pathname :name "communicative-success-val" :type "csv"
                                           :directory '("experiments" "concept-emergence2" "logging"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-communicative-success-val interaction-finished)
  (record-communicative-success experiment interaction))

;; test
(define-monitor record-communicative-success-test
                :class 'data-recorder
                :average-window 1000
                :documentation "Records the game outcome of each game (1 or 0).")

(define-monitor export-communicative-success-test
                :class 'csv-data-file-writer
                :documentation "Exports communicative success."
                :data-sources '(record-communicative-success-test)
                :file-name (babel-pathname :name "communicative-success-test" :type "csv"
                                           :directory '("experiments" "concept-emergence2" "logging"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-communicative-success-test interaction-finished)
  (record-communicative-success experiment interaction))

;; -------------------------------
;; + Construction Inventory Size +
;; -------------------------------

(defun record-construction-inventory-size (experiment interaction)
  (let ((monitor (retrieve-monitor experiment "record-construction-inventory-size")))
    (record-value monitor (let* ((agent (second (agents (experiment interaction))))
                                 (fast-inventory (get-inventory (lexicon agent) :fast))
                                 (trash-inventory (get-inventory (lexicon agent) :trash))
                                 (total (+ (hash-table-count fast-inventory)
                                           (hash-table-count trash-inventory))))
                            total))))

;; train
(define-monitor record-construction-inventory-size-train
                :class 'data-recorder
                :average-window 1000
                :documentation "Records the average construction inventory size.")

(define-monitor export-construction-inventory-size-train
                :class 'csv-data-file-writer
                :documentation "Exports construction inventory size."
                :data-sources '(record-construction-inventory-size-train)
                :file-name (babel-pathname :name "construction-inventory-size-train" :type "csv"
                                           :directory '("experiments" "concept-emergence2" "logging"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-construction-inventory-size-train interaction-finished)
  (record-construction-inventory-size experiment interaction))

;; val
(define-monitor record-construction-inventory-size-val
                :class 'data-recorder
                :average-window 1000
                :documentation "Records the average construction inventory size.")

(define-monitor export-construction-inventory-size-val
                :class 'csv-data-file-writer
                :documentation "Exports construction inventory size."
                :data-sources '(record-construction-inventory-size-val)
                :file-name (babel-pathname :name "construction-inventory-size-val" :type "csv"
                                           :directory '("experiments" "concept-emergence2" "logging"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-construction-inventory-size-val interaction-finished)
  (record-construction-inventory-size experiment interaction))

;; test
(define-monitor record-construction-inventory-size-test
                :class 'data-recorder
                :average-window 1000
                :documentation "Records the average construction inventory size.")

(define-monitor export-construction-inventory-size-test
                :class 'csv-data-file-writer
                :documentation "Exports construction inventory size."
                :data-sources '(record-construction-inventory-size-test)
                :file-name (babel-pathname :name "train" :type "csv"
                                           :directory '("experiments" "concept-emergence2" "logging"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-construction-inventory-size-test interaction-finished)
  (record-construction-inventory-size experiment interaction))

;; ---------------------------------
;; + Degree of conventionalisation +
;; ---------------------------------

(defun record-conventionalisation (experiment interaction)
  (let ((monitor (retrieve-monitor experiment "record-conventionalisation")))
    (record-value monitor (if (find-data interaction 'lexicon-coherence) 1 0))))

;; train
(define-monitor record-conventionalisation-train
                :class 'data-recorder
                :average-window 1000
                :documentation "Records the degree of conventionalisation.")

(define-monitor export-conventionalisation-train
                :class 'csv-data-file-writer
                :documentation "Exports the degree of conventionalisation."
                :data-sources '(record-conventionalisation-train)
                :file-name (babel-pathname :name "lexicon-coherence-train" :type "csv"
                                           :directory '("experiments" "concept-emergence2" "logging"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-conventionalisation-train interaction-finished)
  (record-conventionalisation experiment interaction))

;; val
(define-monitor record-conventionalisation-val
                :class 'data-recorder
                :average-window 1000
                :documentation "Records the degree of conventionalisation.")

(define-monitor export-conventionalisation-val
                :class 'csv-data-file-writer
                :documentation "Exports the degree of conventionalisation."
                :data-sources '(record-conventionalisation-val)
                :file-name (babel-pathname :name "lexicon-coherence-val" :type "csv"
                                           :directory '("experiments" "concept-emergence2" "logging"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-conventionalisation-val interaction-finished)
  (record-conventionalisation experiment interaction))

;; test
(define-monitor record-conventionalisation-test
                :class 'data-recorder
                :average-window 1000
                :documentation "Records the degree of conventionalisation.")

(define-monitor export-conventionalisation-test
                :class 'csv-data-file-writer
                :documentation "Exports the degree of conventionalisation."
                :data-sources '(record-conventionalisation-test)
                :file-name (babel-pathname :name "lexicon-coherence-test" :type "csv"
                                           :directory '("experiments" "concept-emergence2" "logging"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-conventionalisation-test interaction-finished)
  (record-conventionalisation experiment interaction))

;; -------------------------------------------------------------
;; + Construction inventory usage (during training/validation) +
;; -------------------------------------------------------------

(defun record-construction-inventory-usage (experiment interaction)
  (let ((monitor (retrieve-monitor experiment "record-construction-inventory-usage")))
    (record-value monitor (loop for agent in (agents (experiment interaction))
                                sum (unique-forms-in-window agent) into total-sum
                                finally (return (round (/ total-sum (length (agents (experiment interaction))))))))))
  
;; train
(define-monitor record-construction-inventory-usage-train
                :class 'data-recorder
                ;; the window is kept track manually in the `usage-table-window` of an agent
                ;; when average-window is set to 0, the data-recorders skips the averaging step
                :average-window 0
                :documentation "Records the amount of unique forms observed in the past x interactions.")

(define-monitor export-construction-inventory-usage-train
                :class 'csv-data-file-writer
                :documentation "Exports the amount of unique forms observed in the past x interactions."
                :data-sources '(record-construction-inventory-usage-train)
                :file-name (babel-pathname :name "unique-form-usage-train" :type "csv"
                                           :directory '("experiments" "concept-emergence2" "logging"))
                :add-time-and-experiment-to-file-name nil)

;; val
(define-monitor record-construction-inventory-usage-val
                :class 'data-recorder
                ;; the window is kept track manually in the `usage-table-window` of an agent
                ;; when average-window is set to 0, the data-recorders skips the averaging step
                :average-window 0
                :documentation "Records the amount of unique forms observed in the past x interactions.")

(define-monitor export-construction-inventory-usage-val
                :class 'csv-data-file-writer
                :documentation "Exports the amount of unique forms observed in the past x interactions."
                :data-sources '(record-construction-inventory-usage-val)
                :file-name (babel-pathname :name "unique-form-usage-val" :type "csv"
                                           :directory '("experiments" "concept-emergence2" "logging"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-construction-inventory-usage-train interaction-finished)
  (record-construction-inventory-usage experiment interaction))

(define-event-handler (record-construction-inventory-usage-val interaction-finished)
  (record-construction-inventory-usage experiment interaction))

;; -------------------------------------------------
;; + Construction inventory usage (during testing) +
;; -------------------------------------------------
(define-monitor export-construction-inventory-window
                :documentation "Exports the word usage across all interactions (i.e. the window size is set to infinite).")

(define-event-handler (export-construction-inventory-window run-series-finished)
  (let* ((exp-top-dir (get-configuration experiment :exp-top-dir))
         (dataset-split (get-configuration experiment :dataset-split))
         (exp-name (get-configuration experiment :exp-name))
         (log-dir-name (get-configuration experiment :log-dir-name))
         (path (babel-pathname
                :directory `("experiments"
                             "concept-emergence2"
                             "logging"
                             ,exp-top-dir   ;; directory name that groups a set of related of experiments together
                             ,dataset-split ;; store experiments by split (more convenient)
                             ,exp-name      ;; directory name that groups different runs of an experiment
                             ,log-dir-name  ;; directory name for a single run
                             )
                :name "lexicon-inventory-usage" 
                :type "json"))
         (tables (loop with agents-ht = (make-hash-table :test #'equalp) 
                       for agent in (agents experiment)
                       for ht = (make-hash-table :test #'equalp)
                       do (setf (gethash "fast-inventory" ht) (hash-keys (get-inventory (lexicon agent) :fast)))
                       do (setf (gethash "trash-inventory" ht) (hash-keys (get-inventory (lexicon agent) :trash)))
                       do (setf (gethash "usage-count" ht) (usage-counts (usage-table agent)))
                       do (setf (gethash (id agent) agents-ht) ht)
                       finally (return agents-ht))))
    (ensure-directories-exist path)
    (with-open-file (stream path :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string (jzon::stringify tables)
                    stream))))

;; -------------------------------
;; + Partner selection over time +
;; -------------------------------
(define-monitor record-partner-selection
                :class 'data-recorder
                :average-window 0
                :documentation "Records the q-values for selecting partners.")

(define-monitor export-partner-selection
                :class 'csv-data-file-writer
                :documentation "Exports the q-values for selecting partners."
                :data-sources '(record-partner-selection)
                :file-name (babel-pathname :name "partner-selection" :type "csv"
                                           :directory '("experiments" "concept-emergence2" "logging"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-partner-selection interaction-finished)
  (record-value monitor (let* ((agent (first (agents (experiment interaction)))))
                          #|(loop for agent-id being the hash-keys of (neighbor-q-values agent)
                                                    using (hash-value q-value)
                                                  collect q-value)|#
                          (format nil "{\"~a\": ~a}"
                                  (string-downcase (symbol-name (id agent)))
                                  (jzon::stringify (hash-values (neighbor-q-values agent)))))))

;; -----------------
;; + Export CONFIG +
;; -----------------
(define-monitor export-experiment-configurations
                :documentation "Export the configurations of the experiment at the end of the series.")

(define-event-handler (export-experiment-configurations run-series-finished)
                        (let* ((exp-top-dir (get-configuration experiment :exp-top-dir))
                               (dataset-split (get-configuration experiment :dataset-split))
                               (exp-name (get-configuration experiment :exp-name))
                               (log-dir-name (get-configuration experiment :log-dir-name))
                               (path (babel-pathname
                                      :directory `("experiments"
                                                   "concept-emergence2"
                                                   "logging"
                                                   ,exp-top-dir   ;; directory name that groups a set of related of experiments together
                                                   ,dataset-split ;; store experiments by split (more convenient)
                                                   ,exp-name      ;; directory name that groups different runs of an experiment
                                                   ,log-dir-name) ;; directory name for a single run
                                      :name "experiment-configurations" 
                                      :type "json"))
                              (config (configuration experiment)))
                          ;; add the git hash to the configuration
                          (setf (gethash :HASH config) (first (exec-and-return "git" "rev-parse" "HEAD")))
                          (ensure-directories-exist path)
                          (with-open-file (stream path :direction :output
                                                  :if-exists :overwrite
                                                  :if-does-not-exist :create)
                            (write-string (jzon::stringify config) stream))))

(define-monitor export-experiment-store
                :documentation "Exports the entire experiment to a .store file")

(define-event-handler (export-experiment-store run-series-finished)
  (store-experiment experiment))