(in-package :crs-conventionality)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  ;;
;;  Defines the different monitors  ;;
;;                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --------------------------------------
;; + Logs information in output-browser +
;; --------------------------------------
(defvar *start-time* nil)

(define-monitor log-every-x-interactions-in-output-browser
                :documentation "Logs measures every x interactions in the output-browser")
  
(define-event-handler (log-every-x-interactions-in-output-browser interaction-finished)
  #|(ensure-directories-exist (babel-pathname :directory `("experiments"
                                                         "crs-conventionality"
                                                         "raw-data")))|#
  (cond (;; initialise the start time
         (or (= (interaction-number interaction) 1) (not *start-time*))
         (setf *start-time* (get-universal-time)))
        ;; every x interactions -> log to output browser
        ((= (mod (interaction-number interaction)
                 (get-configuration experiment :log-every-x-interactions))
            0)
         (let ((comm-success (caaar (monitors::get-average-values (monitors::get-monitor 'record-communicative-success))))
               (coherence-interacting-agents (caaar (monitors::get-average-values (monitors::get-monitor 'record-coherence-interacting-agents))))
               (coherence-population (caaar (monitors::get-average-values (monitors::get-monitor 'record-coherence-population))))
               )
           (multiple-value-bind (h m s) (seconds-to-hours-minutes-seconds (- (get-universal-time) *start-time*))
             (format t
                     ". (~a | ~a / ~a / ~a | ~ah ~am ~as)~%"
                     (interaction-number interaction)
                     (if comm-success
                       (format nil "~,vf% Comm. success" 1 (* 100 (float comm-success)))
                       "NIL")
                     (if coherence-interacting-agents
                       (format nil "~,vf% Conven." 1 (* 100 (float coherence-interacting-agents)))
                       "NIL")
                     (if coherence-population
                       (format nil "~,vf% Population-wide Conven." 1 (* 100 (float coherence-population)))
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

;; --------------------------------------------------
;; + Degree of coherence between interacting agents +
;; --------------------------------------------------
(define-monitor record-coherence-interacting-agents
                :class 'data-recorder
                :average-window 100
                :documentation "Records the coherence between interacting agents, which serves as a proxy for conventionalisation.")

(define-monitor export-coherence-interacting-agents
                :class 'lisp-data-file-writer
                :documentation "Exports the degree of coherence between interacting agents."
                :data-sources '(record-coherence-interacting-agents)
                :file-name (babel-pathname :name "coherence-interacting-agents" :type "lisp"
                                           :directory '("experiments" "crs-conventionality" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-coherence-interacting-agents interaction-finished)
  (record-value monitor (if (coherence-interacting-agents interaction) 1 0)))



;; ---------------------------------------
;; + Degree of population-wide coherence +
;; ---------------------------------------
(define-monitor record-coherence-population
                :class 'data-recorder
                :average-window 100
                :documentation "Records the population-wide coherence, which serves as a proxy for conventionalisation.")

(define-monitor export-coherence-population
                :class 'lisp-data-file-writer
                :documentation "Exports the degree of population-wide coherence."
                :data-sources '(record-coherence-population)
                :file-name (babel-pathname :name "coherence-population" :type "lisp"
                                           :directory '("experiments" "crs-conventionality" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-coherence-population interaction-finished)
  (record-value monitor (coherence-population interaction)))



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
  (> (attr-val cxn :score) 0.0001))

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
                                (average record-coherence-interacting-agents)
                                (average record-coherence-population)
                                (average record-construction-inventory-size))
                :update-interval 100
                :caption '("communicative success"
                           "degree of coherence between interacting agents"
                           "degree of population-wide coherence"
                           "construction inventory size")
                :x-label "# Games"
                :use-y-axis '(1 1 1 2)
                :y1-label "Communicative Success/Coherence" 
                :y1-max 1.0
                :y1-min 0
                :y2-label "Construction inventory size"
                :y2-min 0
                :draw-y1-grid t
                :error-bars nil)

