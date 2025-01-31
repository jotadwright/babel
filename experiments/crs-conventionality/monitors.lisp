(in-package :crs-conventionality)

;;;;;;;;;;;;;;
;; Monitors ;;
;;;;;;;;;;;;;;

;; ------------------------------
;; + Printing dots at intervals +
;; ------------------------------
(defvar *start-time* nil)

(define-monitor print-a-dot-for-each-interaction
                :documentation "Prints a '.' for each interaction
                 and prints the number after :dot-interval")

(define-event-handler (print-a-dot-for-each-interaction interaction-finished)
  (ensure-directories-exist (babel-pathname :directory `("experiments"
                                                         "crs-conventionality"
                                                         "raw-data")))
  (cond ((or (= (interaction-number interaction) 1) (not *start-time*))
         (setf *start-time* (get-universal-time)))
        ((= (mod (interaction-number interaction)
                 (get-configuration experiment :dot-interval))
            0)
         (let ((comm-success (caaar (monitors::get-average-values (monitors::get-monitor 'record-communicative-success))))
               (coherence (caaar (monitors::get-average-values (monitors::get-monitor 'record-conventionalisation)))))
           (multiple-value-bind (h m s) (seconds-to-hours-minutes-seconds (- (get-universal-time) *start-time*))
             (format t
                     ". (~a / ~a / ~a / ~ah ~am ~as)~%"
                     (interaction-number interaction)
                     (if comm-success
                       (format nil "~,vf% Comm. success" 1 (* 100 (float comm-success)))
                       "NIL")
                     (if coherence
                       (format nil "~,vf% Conven." 1 (* 100 (float coherence)))
                       "NIL")
                     h m s)))
         ;; reset the timer
         (setf *start-time* (get-universal-time)))))

;; -------------------------
;; + Communicative success +
;; -------------------------
(define-monitor record-communicative-success
                :class 'data-recorder
                :average-window 100
                :documentation "Records the game outcome of each game (1 or 0).")

(define-monitor export-communicative-success
                :class 'lisp-data-file-writer
                :documentation "Exports communicative success."
                :data-sources '(record-communicative-success)
                :file-name (babel-pathname :name "communicative-success" :type "lisp"
                                           :directory '("experiments" "crs-conventionality" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-communicative-success interaction-finished)
  (record-value monitor (if (communicated-successfully interaction) 1 0)))

;; ---------------------
;; + Lexicon Coherence +
;; ---------------------
(define-monitor record-conventionalisation
                :class 'data-recorder
                :average-window 100
                :documentation "Records the degree of conventionalisation.")

(define-monitor export-conventionalisation
                :class 'lisp-data-file-writer
                :documentation "Exports the degree of conventionalisation."
                :data-sources '(record-conventionalisation)
                :file-name (babel-pathname :name "conventionalisation" :type "lisp"
                                           :directory '("experiments" "crs-conventionality" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-conventionalisation interaction-finished)
  (record-value monitor (if (coherence interaction) 1 0)))

;; -------------------------------
;; + Construction inventory size +
;; -------------------------------
(define-monitor record-construction-inventory-size
                :class 'data-recorder
                :average-window 1
                :documentation "Records the construction inventory size.")

(define-monitor export-construction-inventory-size
                :class 'lisp-data-file-writer
                :documentation "Exports the construction inventory size."
                :data-sources '(record-construction-inventory-size)
                :file-name (babel-pathname :name "construction-inventory-size" :type "lisp"
                                           :directory '("experiments" "crs-conventionality" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(defun non-zero-cxn-p (cxn) ;; func should be moved to grammar
  "Checks if the entrenchment score of a cxn is positive."
  (> (attr-val cxn :score) 0))

(define-event-handler (record-construction-inventory-size interaction-finished)
  (record-value monitor (loop for agent in (agents (population (experiment interaction)))
                              for grammar-size = (count-if #'non-zero-cxn-p (constructions (grammar agent)))
                              sum grammar-size into total-sum
                              finally (return (round (/ total-sum (length (agents (population (experiment interaction))))))))))

;; ---------------------
;; + Real-time display +
;; ---------------------

;; Gnuplot Display monitor
(define-monitor display-metrics
                :class 'gnuplot-display
                :data-sources '((average record-communicative-success)
                                (average record-conventionalisation)
                                (average record-construction-inventory-size))
                :update-interval 100
                :caption '("communicative success"
                           "degree of conventionalisation"
                           "construction inventory size")
                :x-label "# Games"
                :use-y-axis '(1 1 2)
                :y1-label "Communicative Success/Conventionalisation" 
                :y1-max 1.0
                :y1-min 0
                :y2-label "Construction inventory size"
                :y2-min 0
                :draw-y1-grid t
                :error-bars nil)