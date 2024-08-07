(in-package :cle)

;; ------------
;; + Monitors +
;; ------------

;; -----------------
;; + Printing dots +
;; -----------------
(defvar *start-time* nil)

(define-monitor print-a-dot-for-each-interaction
                :documentation "Prints a '.' for each interaction
                 and prints the number after :dot-interval")
  
(define-event-handler (print-a-dot-for-each-interaction interaction-finished)
                      (cond ((or (= (interaction-number interaction) 1) (not *start-time*))
                             (setf *start-time* (get-universal-time)))
                            ((= (mod (interaction-number interaction)
                                     (get-configuration experiment :dot-interval))
                                0)
                             (let ((comm-success (caaar (monitors::get-average-values (monitors::get-monitor 'record-communicative-success))))
                                   (coherence (caaar (monitors::get-average-values (monitors::get-monitor 'record-lexicon-coherence)))))
                               (multiple-value-bind (h m s) (seconds-to-hours-minutes-seconds (- (get-universal-time) *start-time*))
                                 (format t
                                         ". (~a / ~a / ~a / ~ah ~am ~as)~%"
                                         (interaction-number interaction)
                                         (if comm-success
                                           (format nil "~,vf%" 1 (* 100 (float comm-success)))
                                           "NIL")
                                         (if coherence
                                           (format nil "~,vf%" 1 (* 100 (float coherence)))
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
                :class 'lisp-data-file-writer
                :documentation "Exports communicative success."
                :data-sources '(record-communicative-success)
                :file-name (babel-pathname :name "communicative-success" :type "lisp"
                                           :directory '("experiments" "concept-emergence2" "logging"))
                :add-time-and-experiment-to-file-name nil
                :column-separator " "
                :comment-string "#")

(define-event-handler (record-communicative-success interaction-finished)
  (record-value monitor (if (communicated-successfully interaction) 1 0)))

;; ---------------------
;; + Lexicon Coherence +
;; ---------------------
(define-monitor record-lexicon-coherence
                :class 'data-recorder
                :average-window 1000
                :documentation "Records the lexicon coherence.")

(define-monitor export-lexicon-coherence
                :class 'lisp-data-file-writer
                :documentation "Exports lexicon size."
                :data-sources '(record-lexicon-coherence)
                :file-name (babel-pathname :name "lexicon-coherence" :type "lisp"
                                           :directory '("experiments" "concept-emergence2" "logging"))
                :add-time-and-experiment-to-file-name nil
                :column-separator " "
                :comment-string "#")

(define-event-handler (record-lexicon-coherence interaction-finished)
  (record-value monitor (if (find-data interaction 'lexicon-coherence) 1 0)))

;; ---------------------
;; + Unique form usage +
;; ---------------------
(define-monitor record-unique-form-usage
                :class 'data-recorder
                :average-window 0
                :documentation "Records the unique form usage.")

(define-monitor export-unique-form-usage
                :class 'lisp-data-file-writer
                :documentation "Exports the unique form usage"
                :data-sources '(record-unique-form-usage)
                :file-name (babel-pathname :name "unique-form-usage" :type "lisp"
                                           :directory '("experiments" "concept-emergence2" "logging"))
                :add-time-and-experiment-to-file-name nil
                :column-separator " "
                :comment-string "#")

(define-event-handler (record-unique-form-usage interaction-finished)
  (record-value monitor (loop for agent in (agents (experiment interaction))
                              sum (unique-forms-in-window agent) into total-sum
                              finally (return (round (/ total-sum (length (agents (experiment interaction)))))))))

;; -----------------
;; + Export CONFIG +
;; -----------------
;;;; Export the configurations of the experiment at the end of the first series
(define-monitor export-experiment-configurations)
(define-event-handler (export-experiment-configurations run-series-finished)
                      (when (= (series-number experiment) 1)
                        (let* ((exp-top-dir (get-configuration experiment :exp-top-dir))
                               (log-dir-name (get-configuration experiment :log-dir-name))
                               (exp-name (get-configuration experiment :exp-name))
                               (path (babel-pathname
                                      :directory `("experiments"
                                                   "concept-emergence2"
                                                   "logging"
                                                   ,exp-top-dir
                                                   ,exp-name
                                                   ,log-dir-name)
                                      :name "experiment-configurations" 
                                      :type "lisp"))
                               (config (append (entries experiment) 
                                               (list (cons :HASH (first (exec-and-return "git" "rev-parse" "HEAD")))))))
                          (ensure-directories-exist path)
                          (with-open-file (stream path :direction :output
                                                  :if-exists :overwrite
                                                  :if-does-not-exist :create)
                            (write config :stream stream)))))

(define-monitor export-experiment-store)
(define-event-handler (export-experiment-store run-series-finished)
  (let* ((exp-top-dir (get-configuration experiment :exp-top-dir))
         (log-dir-name (get-configuration experiment :log-dir-name))
         (exp-name (get-configuration experiment :exp-name))
         (path (babel-pathname
                :directory `("experiments" 
                             "concept-emergence2" 
                             "logging" 
                             ,exp-top-dir
                             ,exp-name
                             ,log-dir-name
                             "stores")
                :name (list-of-strings->string (list (write-to-string (series-number experiment)) "history") :separator "-") 
                :type "store")))
    (setf (world experiment) nil)
    (ensure-directories-exist path)
    (cl-store:store experiment path)))
