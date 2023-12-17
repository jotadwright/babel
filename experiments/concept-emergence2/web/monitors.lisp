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
                        (let* ((experiment-name (get-configuration experiment :experiment-name))
                               (output-dir (get-configuration experiment :output-dir))
                               (path (babel-pathname
                                      :directory `("experiments" "concept-emergence2" "logging" ,(downcase output-dir) ,(downcase experiment-name))
                                      :name "experiment-configurations" :type "lisp"))
                               (config (append (entries experiment)  (list (cons :HASH (first (exec-and-return "git" "rev-parse" "HEAD"))))))
                               (clean-config (remove :CURRENT-SCENE-IDX (remove :SCENE-IDS config :key #'car) :key #'car)))
                          (ensure-directories-exist path)
                          (with-open-file (stream path :direction :output
                                                  :if-exists :overwrite
                                                  :if-does-not-exist :create)
                            (write clean-config :stream stream)))))

(define-monitor export-experiment-store)
(define-event-handler (export-experiment-store run-series-finished)
  (let* ((experiment-name (get-configuration experiment :experiment-name))
          (output-dir (get-configuration experiment :output-dir))
          (path (babel-pathname
                :directory `("experiments" "concept-emergence2" "logging" ,(downcase output-dir) ,(downcase experiment-name) "stores")
                :name (list-of-strings->string (list (write-to-string (series-number experiment)) "history") :separator "-") :type "store")))
    (setf (world experiment) nil)
    (ensure-directories-exist path)
    (cl-store:store experiment path)))

#|;; ---------------------
;; + Invention rate +
;; ---------------------
(define-monitor record-invention-rate
                :class 'data-recorder
                :average-window 1
                :documentation "Records the invention rate.")

(define-monitor export-invention-rate
                :class 'lisp-data-file-writer
                :documentation "Exports invention rate."
                :data-sources '(record-invention-rate)
                :file-name (babel-pathname :name "invention-rate" :type "lisp"
                                           :directory '("experiments" "concept-emergence" "logging"))
                :add-time-and-experiment-to-file-name nil
                :column-separator " "
                :comment-string "#")

(define-event-handler (record-invention-rate interaction-finished)
                      (record-value monitor (if (invented-or-adopted (speaker interaction)) 1 0)))

;; ---------------------
;; + Adoption rate +
;; ---------------------
(define-monitor record-adoption-rate
                :class 'data-recorder
                :average-window 100
                :documentation "Records the adoption rate.")

(define-monitor export-adoption-rate
                :class 'lisp-data-file-writer
                :documentation "Exports adoption rate."
                :data-sources '(record-invention-rate)
                :file-name (babel-pathname :name "adoption-rate" :type "lisp"
                                           :directory '("experiments" "concept-emergence2" "logging"))
                :add-time-and-experiment-to-file-name nil
                :column-separator " "
                :comment-string "#")

(define-event-handler (record-adoption-rate interaction-finished)
                      (record-value monitor (if (invented-or-adopted (hearer interaction)) 1 0)))

;; -----------------------
;; + LIVE gnuplot display +
;; -----------------------
(define-monitor display-communicative-success
                :class 'gnuplot-display
                :documentation "Plots the communicative success."
                :data-sources '((average record-communicative-success)
                                (average record-lexicon-coherence)
                                record-invention-rate
                                (average record-adoption-rate)
                                )
                :update-interval 100
                :caption '("communicative success"
                           "lexicon coherence"
                           "invention-rate"
                           "adoption-rate"
                           )
                :x-label "# Games"
                :use-y-axis '(1 1 1 1)
                :y1-label "Communicative Success/Lexicon Coherence" 
                :y1-max 1.0 :y1-min 0
                ;:y2-label "Lexicon Size"
                ;:y2-min 0
                :draw-y1-grid t
                :error-bars nil)
|#


;; ------------------
;; + Question Types +
;; ------------------
#|(define-monitor record-attribute-type
                :class 'data-recorder
                :average-window 500
                :documentation "Records the success for each attribute type.")

(define-monitor export-attribute-type
                :class 'lisp-data-file-writer
                :documentation "Exports attribute type."
                :data-sources '(record-attribute-type)
                :file-name (babel-pathname :name "attribute-type" :type "lisp"
                                           :directory '("experiments" "concept-emergence" "logging"))
                :add-time-and-experiment-to-file-name nil
                :column-separator " "
                :comment-string "#")

(define-event-handler (record-attribute-type interaction-finished)
                      (record-value monitor (find-data interaction 'attribute-type)))|#

;; -----------------------
;; + Store final lexicon +
;; -----------------------

#|(define-monitor export-hearer-concepts-to-pdf)

(define-event-handler (export-hearer-concepts-to-pdf run-series-finished)
                      (loop for agent in (agents experiment)
                            do (lexicon->pdf agent :serie (series-number experiment))))|#

