(in-package :cle)

;; ------------
;; + Monitors +
;; ------------

;; when a game finished
(define-event interaction-finished-train (experiment t) (interaction t) (interaction-number number))
(define-event interaction-finished-val (experiment t) (interaction t) (interaction-number number))
(define-event interaction-finished-test (experiment t) (interaction t) (interaction-number number))

;; -------------------------------------------------------
;; + Log information to output-browser (standard output) +
;; -------------------------------------------------------
(defvar *start-time* nil)

(defun log-every-x-interactions-in-output-browser (experiment interaction split)
  (cond ((or (= (interaction-number interaction) 1) (not *start-time*))
                            (setf *start-time* (get-universal-time)))
                          ((= (mod (interaction-number interaction)
                                    (get-configuration experiment :log-every-x-interactions))
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
                            (setf *start-time* (get-universal-time)))))

(define-monitor log-every-x-interactions-in-output-browser-train
                :documentation "Logs measures every x interactions in the output-browser")
  
(define-event-handler (log-every-x-interactions-in-output-browser-train interaction-finished-train)
                      (log-every-x-interactions-in-output-browser experiment interaction "TRAIN"))

(define-monitor log-every-x-interactions-in-output-browser-val
                :documentation "Logs measures every x interactions in the output-browser")
  
(define-event-handler (log-every-x-interactions-in-output-browser-val interaction-finished-val)
                      (log-every-x-interactions-in-output-browser experiment interaction "VAL"))

(define-monitor log-every-x-interactions-in-output-browser-test
                :documentation "Logs measures every x interactions in the output-browser")
  
(define-event-handler (log-every-x-interactions-in-output-browser-test interaction-finished-test)
                      (log-every-x-interactions-in-output-browser experiment interaction "TEST"))


;; -------------------------
;; + Communicative success +
;; -------------------------

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

(defun test (monitor interaction)
  (record-value monitor (if (communicated-successfully interaction) 1 0)))

(define-event-handler (record-communicative-success-train interaction-finished-train)
  (test monitor interaction))

;; validation
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

(define-event-handler (record-communicative-success-val interaction-finished-val)
  (record-value monitor (if (communicated-successfully interaction) 1 0)))

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

(define-event-handler (record-communicative-success-test interaction-finished-test)
  (record-value monitor (if (communicated-successfully interaction) 1 0)))

;; ---------------------------------
;; + Degree of conventionalisation +
;; ---------------------------------

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

(define-event-handler (record-conventionalisation-train interaction-finished-train)
  (record-value monitor (if (find-data interaction 'lexicon-coherence) 1 0)))

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

(define-event-handler (record-conventionalisation-val interaction-finished-val)
  (record-value monitor (if (find-data interaction 'lexicon-coherence) 1 0)))

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

(define-event-handler (record-conventionalisation-test interaction-finished-test)
  (record-value monitor (if (find-data interaction 'lexicon-coherence) 1 0)))

;; -------------------------------------------------------------
;; + Construction inventory usage (during training/validation) +
;; -------------------------------------------------------------

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

(define-event-handler (record-construction-inventory-usage-train interaction-finished-train)
  (record-value monitor (loop for agent in (agents (experiment interaction))
                              sum (unique-forms-in-window agent) into total-sum
                              finally (return (round (/ total-sum (length (agents (experiment interaction)))))))))

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

(define-event-handler (record-construction-inventory-usage-val interaction-finished-val)
  (record-value monitor (loop for agent in (agents (experiment interaction))
                              sum (unique-forms-in-window agent) into total-sum
                              finally (return (round (/ total-sum (length (agents (experiment interaction)))))))))

;; -------------------------------------------------
;; + Construction inventory usage (during testing) +
;; -------------------------------------------------
(define-monitor export-construction-inventory-usage-test
                :documentation "Exports the word usage across all interactions (i.e. the window size is set to infinite).")

(define-event-handler (export-construction-inventory-usage-test run-series-finished)
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