;;;; monitors.lisp

(in-package :clevr-grammar-learning)

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
                                           :directory '("experiments" "clevr-grammar-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-communicative-success interaction-finished)
  (record-value monitor (if (communicated-successfully interaction) 1 0)))

;;;; Lexicon size
(define-monitor record-lexicon-size
                :class 'data-recorder
                :documentation "records the avg lexicon size.")

(define-monitor export-lexicon-size
                :class 'lisp-data-file-writer
                :documentation "Exports lexicon size"
                :data-sources '(record-lexicon-size)
                :file-name (babel-pathname :name "lexicon-size" :type "lisp"
                                           :directory '("experiments" "clevr-grammar-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-lexicon-size interaction-finished)
  (record-value monitor (count-if #'non-zero-cxn-p (get-cxns-of-type (learner experiment) 'all))))

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
                                           :directory '("experiments" "clevr-grammar-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)
                
(define-event-handler (record-avg-cxn-score interaction-finished)
  (record-value monitor (average (mapcar #'cxn-score (get-cxns-of-type (learner experiment) 'all)))))

;;;; lexicon size per cxn type (alist monitor)
(define-monitor record-lexicon-size-per-type
                :class 'alist-recorder
                :average-window 1)

(defun debug-record-lexicon-size-per-type (experiment monitor)
  (let ((all-constructions
         (constructions-list (grammar (learner experiment)))))
    (loop for cxn-type in '(gl::holophrase gl::lexical gl::item-based)
          for all-cxns-of-type = (find-all cxn-type all-constructions
                                           :key #'get-cxn-type)
          when all-cxns-of-type
          do (set-value-for-symbol monitor cxn-type (length all-cxns-of-type)))))

(define-event-handler (record-lexicon-size-per-type interaction-finished)
  (debug-record-lexicon-size-per-type experiment monitor))

(define-monitor plot-lexicon-size-per-type
    :class 'alist-gnuplot-graphic-generator
    :recorder 'record-lexicon-size-per-type
    :draw-y-grid t :y-min 0
    :y-label "Number of constructions"
    :x-label "Total number of interactions"
    :file-name (babel-pathname :name "num-cxns-per-type" :type "pdf"
                               :directory '("experiments" "clevr-grammar-learning" "graphs"))
    :graphic-type "pdf" :error-bars '(:percentile 5 95)
    :add-time-and-experiment-to-file-name nil)

;;;; avg cxn score per cxn type (alist monitor)
(define-monitor record-cxn-score-per-type
                :class 'alist-recorder
                :average-window 100)

(define-event-handler (record-cxn-score-per-type interaction-finished)
  (let ((all-constructions
         (constructions-list (grammar (learner experiment)))))
    (loop for cxn-type in '(gl::holophrase gl::lexical gl::item-based)
          for all-cxns-of-type = (find-all cxn-type all-constructions
                                           :key #'get-cxn-type)
          for cxn-scores = (mapcar #'cxn-score all-cxns-of-type)
          when all-cxns-of-type
          do (set-value-for-symbol monitor cxn-type (average cxn-scores)))))

(define-monitor plot-cxn-score-per-type
    :class 'alist-gnuplot-graphic-generator
    :recorder 'record-cxn-score-per-type
    :draw-y-grid t
    :y-min 0 :y-max 1
    :y-label "Construction Score"
    :x-label "Total number of interactions"
    :file-name (babel-pathname :name "avg-cxn-score-per-type" :type "pdf"
                               :directory '("experiments" "clevr-grammar-learning" "graphs"))
    :graphic-type "pdf" :error-bars '(:percentile 5 95)
    :add-time-and-experiment-to-file-name nil)

;; cxn usage per type (alist monitor)
(define-monitor record-cxn-usage-per-type
                :class 'alist-recorder
                :average-window 100)

(define-event-handler (record-cxn-usage-per-type constructions-chosen)
  (if (find 'holophrase constructions :key #'get-cxn-type)
    (progn (set-value-for-symbol monitor 'holophrase 1)
      (set-value-for-symbol monitor 'item-based+lexical 0))
    (progn (set-value-for-symbol monitor 'holophrase 0)
      (set-value-for-symbol monitor 'item-based+lexical 1))))
  
(define-monitor plot-cxn-usage-per-type
        :class 'alist-gnuplot-graphic-generator
        :recorder 'record-cxn-usage-per-type
        :draw-y-grid t
        :y-min 0 :y-max 1
        :y-label "Usage"
        :x-label "Total number of interactions"
        :file-name (babel-pathname :name "cxn-usage-per-type" :type "pdf"
                                   :directory '("experiments" "clevr-grammar-learning" "graphs"))
        :graphic-type "pdf" :error-bars '(:percentile 5 95)
        :add-time-and-experiment-to-file-name nil)

;; nr of item-based cxns with slots (alist monitor)
(define-monitor record-nr-of-slots
                :class 'alist-recorder
                :average-window 1)

(define-event-handler (record-nr-of-slots interaction-finished)
  (let* ((all-constructions
          (constructions-list (grammar (learner experiment))))
         (item-based-cxns
          (find-all 'gl::item-based all-constructions :key #'get-cxn-type)))
    (loop with counts = nil
          for cxn in item-based-cxns
          for nr-of-slots = (item-based-number-of-slots cxn)
          if (assoc nr-of-slots counts :test #'=)
          do (incf (cdr (assoc nr-of-slots counts)))
          else do (push (cons nr-of-slots 1) counts)
          finally (loop for (nr . count) in counts
                        for sym = (internal-symb (upcase (format nil "~r" nr)))
                        do (set-value-for-symbol monitor sym count)))))

(define-monitor plot-nr-of-slots
        :class 'alist-gnuplot-graphic-generator
        :recorder 'record-nr-of-slots
        :draw-y-grid t :y-min 0
        :y-label "Number of constructions"
        :x-label "Total number of interactions"
        :file-name (babel-pathname :name "nr-of-item-based-cxns-with-slots" :type "pdf"
                                   :directory '("experiments" "clevr-grammar-learning" "graphs"))
        :graphic-type "pdf" :error-bars '(:percentile 5 95)
        :add-time-and-experiment-to-file-name nil)



;; utilty function to get all of them
(defun get-all-lisp-monitors ()
  '("export-communicative-success"
    "export-lexicon-size"
    "export-avg-cxn-score"
    "plot-lexicon-size-per-type"
    "plot-cxn-score-per-type"
    "plot-cxn-usage-per-type"
    "plot-nr-of-slots"))
