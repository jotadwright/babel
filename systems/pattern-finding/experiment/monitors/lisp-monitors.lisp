;;;; monitors.lisp

(in-package :pf)

;;;; Communicative success             
(define-monitor record-communicative-success
                :class 'data-recorder
                :average-window 100
                :documentation "records the game outcome of each game (1 or 0).")

(define-monitor export-communicative-success
                :class 'lisp-data-file-writer
                :documentation "Exports communicative success"
                :data-sources '((average record-communicative-success))
                :file-name (babel-pathname :name "communicative-success" :type "lisp"
                                           :directory '("systems" "pattern-finding" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-communicative-success interaction-finished)
  (record-value monitor (if (communicated-successfully interaction) 1 0)))




;;;; Grammar size
(define-monitor record-lexicon-size
                :class 'data-recorder
                :documentation "records the avg grammar size.")

(define-monitor export-lexicon-size
                :class 'lisp-data-file-writer
                :documentation "Exports lexicon size"
                :data-sources '(record-lexicon-size)
                :file-name (babel-pathname :name "grammar-size" :type "lisp"
                                           :directory '("systems" "pattern-finding" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-lexicon-size interaction-finished)
  (record-value monitor (length (routine-non-zero-cxns (learner experiment)))))





;;;; Gnuplot Display monitor
(define-monitor display-metrics
                :class 'gnuplot-display
                :documentation "Plots the communicative success."
                :data-sources '((average record-communicative-success)
                                (average record-lexicon-size))
                :update-interval 100
                :caption '("communicative success"
                           "grammar size")
                :x-label "# Games"
                :use-y-axis '(1 2)
                :y1-label "Communicative Success" 
                :y1-max 1.0 :y1-min 0
                :y2-label "Grammar Size"
                :y2-min 0
                :draw-y1-grid t
                :error-bars nil)





;;;; Avg cxn score
(define-monitor record-avg-cxn-score
                :class 'data-recorder
                :average-window 100
                :documentation "record the avg cxn score")

(define-monitor export-avg-cxn-score
                :class 'lisp-data-file-writer
                :documentation "exports avg cxn score"
                :data-sources '((average record-avg-cxn-score))
                :file-name (babel-pathname :name "avg-cxn-score" :type "lisp"
                                           :directory '("systems" "pattern-finding" "raw-data"))
                :add-time-and-experiment-to-file-name nil)
                
(define-event-handler (record-avg-cxn-score interaction-finished)
  (record-value monitor (average (mapcar #'cxn-score (routine-non-zero-cxns (learner experiment))))))





;;;; lexicon size per cxn type 
(define-monitor record-number-of-holistic-cxns
                :class 'data-recorder)

(define-monitor export-number-of-holostic-cxns
                :class 'lisp-data-file-writer
                :data-sources '(record-number-of-holistic-cxns)
                :file-name (babel-pathname :name "number-of-holostic-cxns" :type "lisp"
                                           :directory '("systems" "pattern-finding" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-number-of-holistic-cxns interaction-finished)
  (let* ((cxns (routine-non-zero-cxns (learner experiment)))
         (holistic-cxns (remove-if-not #'holistic-cxn-p cxns)))
    (record-value monitor (length holistic-cxns))))

(define-monitor record-number-of-item-based-cxns
                :class 'data-recorder)

(define-monitor export-number-of-item-based-cxns
                :class 'lisp-data-file-writer
                :data-sources '(record-number-of-item-based-cxns)
                :file-name (babel-pathname :name "number-of-item-based-cxns" :type "lisp"
                                           :directory '("systems" "pattern-finding" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-number-of-item-based-cxns interaction-finished)
  (let* ((cxns (routine-non-zero-cxns (learner experiment)))
         (item-based-cxns (remove-if #'holistic-cxn-p cxns)))
    (record-value monitor (length item-based-cxns))))






;;;; avg cxn score per cxn type
(define-monitor record-avg-holostic-cxn-score
                :class 'data-recorder :average-window 100)

(define-monitor export-avg-holostic-cxn-score
                :class 'lisp-data-file-writer
                :data-sources '((average record-avg-holostic-cxn-score))
                :file-name (babel-pathname :name "avg-holophrase-cxn-score" :type "lisp"
                                           :directory '("systems" "pattern-finding" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-avg-holostic-cxn-score interaction-finished)
  (let* ((cxns (routine-non-zero-cxns (learner experiment)))
         (holistic-cxns (remove-if-not #'holistic-cxn-p cxns)))
    (record-value monitor (average (mapcar #'cxn-score holistic-cxns)))))

(define-monitor record-avg-item-based-cxn-score
                :class 'data-recorder :average-window 100)

(define-monitor export-avg-item-based-cxn-score
                :class 'lisp-data-file-writer
                :data-sources '((average record-avg-item-based-cxn-score))
                :file-name (babel-pathname :name "avg-item-based-cxn-score" :type "lisp"
                                           :directory '("systems" "pattern-finding" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-avg-item-based-cxn-score interaction-finished)
  (let* ((cxns (routine-non-zero-cxns (learner experiment)))
         (item-based-cxns (remove-if #'holistic-cxn-p cxns)))
    (record-value monitor (average (mapcar #'cxn-score item-based-cxns)))))


;; repair monitors
(define-monitor record-repair-usage-add-cxn
                :class 'data-recorder)

(define-monitor export-repair-usage-add-cxn
                :class 'lisp-data-file-writer
                :data-sources '(record-repair-usage-add-cxn)
                :file-name (babel-pathname :name "repair-usage-add-cxn" :type "lisp"
                                           :directory '("systems" "pattern-finding" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-repair-usage-add-cxn interaction-finished)
  (record-value monitor (if (string= (find-data interaction :applied-repair) "h") 1 0)))


(define-monitor record-repair-usage-anti-unify-cxns
                :class 'data-recorder)

(define-monitor export-repair-usage-anti-unify-cxns
                :class 'lisp-data-file-writer
                :data-sources '(record-repair-usage-anti-unify-cxns)
                :file-name (babel-pathname :name "repair-usage-anti-unify-cxns" :type "lisp"
                                           :directory '("systems" "pattern-finding" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-repair-usage-anti-unify-cxns interaction-finished)
  (record-value monitor (if (string= (find-data interaction :applied-repair) "a") 1 0)))


(define-monitor record-repair-usage-anti-unify-cipn
                :class 'data-recorder)

(define-monitor export-repair-usage-anti-unify-cipn
                :class 'lisp-data-file-writer
                :data-sources '(record-repair-usage-anti-unify-cipn)
                :file-name (babel-pathname :name "repair-usage-anti-unify-cipn" :type "lisp"
                                           :directory '("systems" "pattern-finding" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-repair-usage-anti-unify-cipn interaction-finished)
  (record-value monitor (if (string= (find-data interaction :applied-repair) "p") 1 0)))


(define-monitor record-repair-usage-add-categorial-links
                :class 'data-recorder)

(define-monitor export-repair-usage-add-categorial-links
                :class 'lisp-data-file-writer
                :data-sources '(record-repair-usage-add-categorial-links)
                :file-name (babel-pathname :name "repair-usage-add-categorial-links" :type "lisp"
                                           :directory '("systems" "pattern-finding" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-repair-usage-add-categorial-links interaction-finished)
  (record-value monitor (if (string= (find-data interaction :applied-repair) "l") 1 0)))


;;;; Number of nodes in the type hierarchy
(define-monitor record-number-of-nodes
                :class 'data-recorder
                :average-window 1
                :documentation "records the number of nodes in the type hierarchy")

(define-monitor export-number-of-nodes
                :class 'lisp-data-file-writer
                :documentation "exports the number of nodes in the type hierarchy"
                :data-sources '(record-number-of-nodes)
                :file-name (babel-pathname :name "number-of-th-nodes" :type "lisp"
                                           :directory '("systems" "pattern-finding" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-number-of-nodes interaction-finished)
  (record-value monitor (length (categories (grammar (learner experiment))))))


;;;; Number of edges in the type hierarchy
(define-monitor record-number-of-edges
                :class 'data-recorder
                :average-window 1
                :documentation "records the number of edges in the type hierarchy")

(define-monitor export-number-of-edges
                :class 'lisp-data-file-writer
                :documentation "exports the number of edges in the type hierarchy"
                :data-sources '(record-number-of-edges)
                :file-name (babel-pathname :name "number-of-th-edges" :type "lisp"
                                           :directory '("systems" "pattern-finding" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-number-of-edges interaction-finished)
  (record-value monitor (length (links (grammar (learner experiment))))))


  
;; utilty function to get all of them
(defun get-all-lisp-monitors ()
  '("export-communicative-success"
    "export-lexicon-size"
    "export-avg-cxn-score"
    "export-number-of-nodes"
    "export-number-of-edges"
    "export-repair-usage-add-cxn"
    "export-repair-usage-add-categorial-links"
    "export-repair-usage-anti-unify-cxns"
    "export-repair-usage-anti-unify-cipn"
    "export-number-of-holostic-cxns"
    "export-number-of-item-based-cxns"
    "export-avg-holostic-cxn-score"
    "export-avg-item-based-cxn-score"))
