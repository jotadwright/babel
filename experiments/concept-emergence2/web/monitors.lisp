(in-package :cle)

;; ------------
;; + Monitors +
;; ------------

;; -------------------------------------------------------
;; + Log information to output-browser (standard output) +
;; -------------------------------------------------------
(defvar *start-time* nil)

(define-monitor log-every-x-interactions-in-output-browser
                :documentation "Logs measures every x interactions in the output-browser")
  
(define-event-handler (log-every-x-interactions-in-output-browser interaction-finished)
                      (cond ((or (= (interaction-number interaction) 1) (not *start-time*))
                             (setf *start-time* (get-universal-time)))
                            ((= (mod (interaction-number interaction)
                                     (get-configuration experiment :log-every-x-interactions))
                                0)
                             (let ((communicative-success (caaar (monitors::get-average-values (monitors::get-monitor 'record-communicative-success))))
                                   (conventionalisation (caaar (monitors::get-average-values (monitors::get-monitor 'record-conventionalisation)))))
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

;; -------------------------
;; + Communicative success +
;; -------------------------
(define-monitor record-communicative-success
                :class 'data-recorder
                :average-window 1000
                :documentation "Records the game outcome of each game (1 or 0).")

(define-monitor export-communicative-success
                :class 'csv-data-file-writer
                :documentation "Exports communicative success."
                :data-sources '(record-communicative-success)
                :file-name (babel-pathname :name "communicative-success" :type "csv"
                                           :directory '("experiments" "concept-emergence2" "logging"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-communicative-success interaction-finished)
  (record-value monitor (if (communicated-successfully interaction) 1 0)))

;; ---------------------------------
;; + Degree of conventionalisation +
;; ---------------------------------
(define-monitor record-conventionalisation
                :class 'data-recorder
                :average-window 1000
                :documentation "Records the degree of conventionalisation.")

(define-monitor export-conventionalisation
                :class 'csv-data-file-writer
                :documentation "Exports the degree of conventionalisation."
                :data-sources '(record-conventionalisation)
                :file-name (babel-pathname :name "lexicon-coherence" :type "csv"
                                           :directory '("experiments" "concept-emergence2" "logging"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-conventionalisation interaction-finished)
  (record-value monitor (if (find-data interaction 'lexicon-coherence) 1 0)))

;; --------------------------------------------------
;; + Construction inventory usage (during training) +
;; --------------------------------------------------
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
                :file-name (babel-pathname :name "unique-form-usage" :type "csv"
                                           :directory '("experiments" "concept-emergence2" "logging"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-construction-inventory-usage-train interaction-finished)
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


(defun print-trimmed-float (float)
  (let ((rounded (format nil "~,3F" float)))
    (string-right-trim "0" (if (find #\. rounded) rounded (concatenate 'string rounded ".")))))

(define-event-handler (record-partner-selection interaction-finished)
  (record-value monitor (let* ((agent (first (agents (experiment interaction)))))
                          #|(loop for agent-id being the hash-keys of (partner-preferences agent)
                                                    using (hash-value q-value)
                                                  collect q-value)|#
                          (format nil "{\"~a\": ~a}"
                                  (string-downcase (symbol-name (id agent)))
                                  (jzon::stringify (hash-values (partner-preferences agent)))))))


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