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

;; ---------------------------------------
;; + Communicative success of New Agents +
;; ---------------------------------------
(define-monitor record-communicative-success-of-new-agents
                :class 'data-recorder
                :average-window 100
                :documentation "Records the game outcome of each game (1 or 0) in which an agent introduced at a later stage of the experiment participated.")

(define-monitor export-communicative-success-of-new-agents
                :class 'lisp-data-file-writer
                :documentation "Records the game outcome of each game (1 or 0) in which an agent introduced at a later stage of the experiment participated."
                :data-sources '(record-communicative-success-of-new-agents)
                :file-name (babel-pathname :name "communicative-success-of-new-agents" :type "lisp"
                                           :directory '("experiments" "crs-conventionality" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-communicative-success-of-new-agents interaction-finished)
  (let ((new-agents (remove-if (lambda (agent) (= 0 (introduced-in-game agent)))
                               (agents (population (experiment interaction))))))
    (if (or (member (speaker interaction) new-agents)
            (member (hearer interaction) new-agents))
      (record-value monitor (if (communicated-successfully interaction) 1 0))
      (record-value monitor NIL))))

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


;; -----------------------------------
;; + Lexicon Coherence of New Agents +
;; -----------------------------------
(define-monitor record-conventionalisation-of-new-agents
                :class 'data-recorder
                :average-window 100
                :documentation "Records the degree of conventionalisation at interactions in which an  agents introduced at a later stage of the experiment participated.")

(define-monitor export-conventionalisation-of-new-agents
                :class 'lisp-data-file-writer
                :documentation "Exports the degree of conventionalisation."
                :data-sources '(record-conventionalisation-of-new-agents)
                :file-name (babel-pathname :name "conventionalisation-of-new-agents" :type "lisp"
                                           :directory '("experiments" "crs-conventionality" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-conventionalisation-of-new-agents interaction-finished)
  (let ((new-agents (remove-if (lambda (agent) (= 0 (introduced-in-game agent)))
                               (agents (population (experiment interaction))))))
    (if (or (member (speaker interaction) new-agents)
            (member (hearer interaction) new-agents))
      (record-value monitor (if (coherence interaction) 1 0))
      (record-value monitor NIL))))

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
  (> (attr-val cxn :score) 0.01))

(define-event-handler (record-construction-inventory-size interaction-finished)
  (record-value monitor (loop for agent in (agents (population (experiment interaction)))
                              for grammar-size = (count-if #'non-zero-cxn-p (constructions (grammar agent)))
                              sum grammar-size into total-sum
                              finally (return (round (/ total-sum (length (agents (population (experiment interaction))))))))))

;; ---------------------------------------------
;; + Construction Inventory Size of New Agents +
;; ---------------------------------------------
(define-monitor record-construction-inventory-size-of-new-agents
                :class 'data-recorder
                :average-window 1
                :documentation "Records the construction inventory size of agents that were introduced at a later stage of the experiment.")

(define-monitor export-construction-inventory-size-of-new-agents
                :class 'lisp-data-file-writer
                :documentation "Exports the construction inventory size of agents that were introced at a later stage of the experiment."
                :data-sources '(record-construction-inventory-size-of-new-agents)
                :file-name (babel-pathname :name "construction-inventory-size-of-new-agents" :type "lisp"
                                           :directory '("experiments" "crs-conventionality" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-construction-inventory-size-of-new-agents interaction-finished)
  (let ((new-agents (remove-if (lambda (agent) (= 0 (introduced-in-game agent)))
                              (agents (population (experiment interaction))))))
    (if (or (member (speaker interaction) new-agents)
              (member (hearer interaction) new-agents))
      (record-value monitor (loop for agent in new-agents
                                  for grammar-size = (count-if #'non-zero-cxn-p (constructions (grammar agent)))
                                  sum grammar-size into total-sum
                                  finally (return (round (/ total-sum (length new-agents))))))
      (record-value monitor NIL))))
    
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

;; ---------------------
;; + Store experiment  +
;; ---------------------

(define-monitor store-every-x-interactions
                :documentation "Stores experiment every x interactions")
  
(define-event-handler (store-every-x-interactions interaction-finished)
  (when (get-configuration experiment :store-every-x-interactions)
    ;; every x interactions -> store experiment 
    (when (= (mod (interaction-number interaction)
                  (get-configuration experiment :store-every-x-interactions))
             0)
      (store-experiment experiment interaction))))
