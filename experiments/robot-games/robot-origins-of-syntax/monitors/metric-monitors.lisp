;;;; /monitors/metric-monitors.lisp

(in-package :roos)

;; --------------------------------------------
;; + Record and Display Communicative success +
;; --------------------------------------------

(define-monitor record-communicative-success
                :class 'data-recorder
                :average-window 3
                :documentation "records the game outcome of each game (1 or 0).")

(define-monitor display-communicative-success
                :class 'gnuplot-display
                :documentation "Plots the communicative success."
                :data-sources '((average record-communicative-success))
                :update-interval 1
                :caption '("communicative success")
                :x-label "games" 
                :y1-label "communicative success" 
                :y1-max 1.0 :y1-min 0 
                :draw-y1-grid t)

(define-monitor export-communicative-success
                :class 'lisp-data-file-writer
                :documentation "Exports communicative success"
                :data-sources '(record-communicative-success)
                :file-name (babel-pathname :name "communicative-success" :type "lisp"
                                           :directory '("experiments" "robot-games" "robot-origins-of-syntax" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :column-separator " "
                :comment-string "#")

(define-event-handler (record-communicative-success interaction-finished)
                      (record-value monitor (if (communicated-successfully interaction)
                                              1
                                              0)))

;; -----------------
;; + Ontology Size +
;; -----------------

(define-monitor record-ontology-size
                :class 'data-recorder
                :average-window 3
                :documentation "Records the average ontology size across all agents")

(define-monitor display-ontology-size
                :class 'gnuplot-display
                :documentation "Plots the average ontology size"
                :data-sources '(record-ontology-size)
                :update-interval 1
                :caption '("ontology size")
                :x-label "games"
                :y1-label "ontology-size"
                :y1-max 1.0 :y1-min 0
                :draw-y1-grid t)

(define-monitor export-ontology-size
                :class 'lisp-data-file-writer
                :documentation "Export ontology size"
                :data-sources '(record-ontology-size)
                :file-name (babel-pathname :name "ontology-size" :type "lisp"
                                           :directory '("experiments" "robot-games" "robot-origins-of-syntax" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :column-separator " "
                :comment-string "#")

(define-event-handler (record-ontology-size interaction-finished)
                      (record-value monitor (let ((categories 0))
                                              (loop for agent in (population experiment)
                                                    do (loop for feature in (get-configuration experiment :features)
                                                             for feature-categories = (find-data (ontology agent) feature)
                                                             do (incf categories (length feature-categories))))
                                              (float (/ categories (length (population experiment)))))))

;; ----------------
;; + Lexicon size +
;; ----------------

(define-monitor record-lexicon-size
                :class 'data-recorder
                :average-window 3
                :documentation "Records the average lexicon size across all agents")

(define-monitor display-lexicon-size
                :class 'gnuplot-display
                :documentation "Plots the average lexicon size"
                :data-sources '(record-lexicon-size)
                :update-interval 1
                :caption '("lexicon size")
                :x-label "games"
                :y1-label "lexicon-size"
                :y1-max 1.0 :y1-min 0
                :draw-y1-grid t)

(define-monitor export-lexicon-size
                :class 'lisp-data-file-writer
                :documentation "Export lexicon size"
                :data-sources '(record-lexicon-size)
                :file-name (babel-pathname :name "lexicon-size" :type "lisp"
                                           :directory '("experiments" "robot-games" "robot-origins-of-syntax" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :column-separator " "
                :comment-string "#")

(define-event-handler (record-lexicon-size interaction-finished)
                      (record-value monitor (let ((cxns 0))
                                              (loop for agent in (population experiment)
                                                    do (incf cxns (length (constructions (grammar agent)))))
                                              (float (/ cxns (length (population experiment)))))))
;; ----------------------------------
;; + Display communicative success, +
;; + ontology size and lexicon size +
;; ----------------------------------

(define-monitor display-metrics
                :class 'gnuplot-display
                :documentation "Shows communicative success, lexicon size and ontology size"
                :data-sources '((average record-communicative-success)
                                record-ontology-size
                                record-lexicon-size)
                :caption '("communicative success"
                           "ontology size"
                           "lexicon size")
                :update-interval 1
                :x-label "games"
                :y1-label "success"
                :y2-label "ontology/lexicon size"
                :y1-max 1.0 :y1-min 0.0
                :y2-min 0.0
                :use-y-axis '(1 2 2))