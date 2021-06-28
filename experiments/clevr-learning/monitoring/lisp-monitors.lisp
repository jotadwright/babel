;;;; monitors.lisp

(in-package :clevr-learning)

;;;; Communicative success             
(define-monitor record-communicative-success
                :class 'data-recorder :average-window 100)

(define-monitor export-communicative-success
                :class 'lisp-data-file-writer
                :data-sources '((average record-communicative-success))
                :file-name (babel-pathname :name "communicative-success" :type "lisp"
                                           :directory '("experiments" "clevr-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-communicative-success interaction-finished)
  (record-value monitor (if (communicated-successfully interaction) 1 0)))

;;;; Lexicon size
(define-monitor record-lexicon-size
                :class 'data-recorder
                :documentation "records the avg lexicon size.")

(define-monitor export-lexicon-size
                :class 'lisp-data-file-writer
                :data-sources '(record-lexicon-size)
                :file-name (babel-pathname :name "lexicon-size" :type "lisp"
                                           :directory '("experiments" "clevr-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-lexicon-size interaction-finished)
  (record-value monitor (length (get-cxns-of-type (learner experiment) 'all))))

;;;; Gnuplot Display monitor
(define-monitor display-metrics
                :class 'gnuplot-display
                :data-sources '((average record-communicative-success)
                                (average record-lexicon-size))
                :update-interval 100
                :caption '("communicative success"
                           "lexicon size")
                :x-label "# Games"
                :use-y-axis '(1 2)
                :y1-label "Communicative Success" 
                :y1-max 1.0 :y1-min 0
                :y2-label "Lexicon Size"
                :y2-min 0
                :draw-y1-grid t
                :error-bars nil)

;;;; # meanings per form for lexical cxns
(define-monitor record-lexical-meanings-per-form
                :class 'data-recorder)

(define-monitor export-lexical-meanings-per-form
                :class 'lisp-data-file-writer
                :data-sources '(record-lexical-meanings-per-form)
                :file-name (babel-pathname :name "lexical-meanings-per-form" :type "lisp"
                                           :directory '("experiments" "clevr-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(defun compute-nr-of-lexical-meanings-per-form (agent)
  (loop with form-count = nil
        with lexical-cxns = (find-all 'lexical (constructions-list (grammar agent))
                                      :key #'get-cxn-type)
        for cxn in lexical-cxns
        for cxn-form = (list-of-strings->string
                        (render (extract-form-predicates cxn)
                                (get-configuration (grammar agent) :render-mode)))
        for found = (assoc cxn-form form-count :test #'string=)
        if found do (incf (cdr found))
        else do (push (cons cxn-form 1) form-count)
        finally
        (return (average (mapcar #'cdr form-count)))))

(define-event-handler (record-lexical-meanings-per-form interaction-finished)
   (record-value monitor
                 (compute-nr-of-lexical-meanings-per-form
                  (learner experiment))))

;;;; # forms per meaning for lexical cxns
(define-monitor record-lexical-forms-per-meaning
                :class 'data-recorder)

(define-monitor export-lexical-forms-per-meaning
                :class 'lisp-data-file-writer
                :data-sources '(record-lexical-forms-per-meaning)
                :file-name (babel-pathname :name "lexical-forms-per-meaning" :type "lisp"
                                           :directory '("experiments" "clevr-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(defun compute-nr-of-lexical-forms-per-meaning (agent)
  (loop with meaning-count = nil
        with lexical-cxns = (find-all 'lexical (constructions-list (grammar agent))
                                      :key #'get-cxn-type)
        for cxn in lexical-cxns
        for cxn-meaning = (last-elt (first (extract-meaning-predicates cxn)))
        for found = (assoc cxn-meaning meaning-count)
        if found do (incf (cdr found))
        else do (push (cons cxn-meaning 1) meaning-count)
        finally
        (return (average (mapcar #'cdr meaning-count)))))

(define-event-handler (record-lexical-forms-per-meaning interaction-finished)
   (record-value monitor
                 (compute-nr-of-lexical-forms-per-meaning
                  (learner experiment))))

;;;; Avg cxn score
(define-monitor record-avg-cxn-score
                :class 'data-recorder :average-window 100)

(define-monitor export-avg-cxn-score
                :class 'lisp-data-file-writer
                :data-sources '((average record-avg-cxn-score))
                :file-name (babel-pathname :name "avg-cxn-score" :type "lisp"
                                           :directory '("experiments" "clevr-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)
                
(define-event-handler (record-avg-cxn-score interaction-finished)
  (record-value monitor (average (mapcar #'cxn-score (get-cxns-of-type (learner experiment) 'all)))))

;;;; lexicon size per cxn type (alist monitor)
(define-monitor record-number-of-holophrase-cxns
                :class 'data-recorder)

(define-monitor export-number-of-holophrase-cxns
                :class 'lisp-data-file-writer
                :data-sources '(record-number-of-holophrase-cxns)
                :file-name (babel-pathname :name "number-of-holophrase-cxns" :type "lisp"
                                           :directory '("experiments" "clevr-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-number-of-holophrase-cxns interaction-finished)
  (record-value
   monitor
   (count 'holophrase (constructions-list (grammar (learner experiment)))
          :key #'get-cxn-type)))

(define-monitor record-number-of-item-based-cxns
                :class 'data-recorder)

(define-monitor export-number-of-item-based-cxns
                :class 'lisp-data-file-writer
                :data-sources '(record-number-of-item-based-cxns)
                :file-name (babel-pathname :name "number-of-item-based-cxns" :type "lisp"
                                           :directory '("experiments" "clevr-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-number-of-item-based-cxns interaction-finished)
  (record-value
   monitor
   (count 'item-based (constructions-list (grammar (learner experiment)))
          :key #'get-cxn-type)))

(define-monitor record-number-of-lexical-cxns
                :class 'data-recorder)

(define-monitor export-number-of-lexical-cxns
                :class 'lisp-data-file-writer
                :data-sources '(record-number-of-lexical-cxns)
                :file-name (babel-pathname :name "number-of-lexical-cxns" :type "lisp"
                                           :directory '("experiments" "clevr-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-number-of-lexical-cxns interaction-finished)
  (record-value
   monitor
   (count 'lexical (constructions-list (grammar (learner experiment)))
          :key #'get-cxn-type)))

#|
(define-monitor record-lexicon-size-per-type
                :class 'alist-recorder
                :average-window 1)

(define-event-handler (record-lexicon-size-per-type interaction-finished)
  (let ((all-constructions
         (constructions-list (grammar (learner experiment)))))
    (loop for cxn-type in '(holophrase lexical item-based)
          for all-cxns-of-type = (find-all cxn-type all-constructions
                                           :key #'get-cxn-type)
          when all-cxns-of-type
          do (set-value-for-symbol monitor cxn-type (length all-cxns-of-type)))))

(define-monitor plot-lexicon-size-per-type
    :class 'alist-gnuplot-graphic-generator
    :recorder 'record-lexicon-size-per-type
    :draw-y-grid t :y-min 0
    :y-label "Number of constructions"
    :x-label "Total number of interactions"
    :file-name (babel-pathname :name "num-cxns-per-type" :type "pdf"
                               :directory '("experiments" "clevr-learning" "graphs"))
    :graphic-type "pdf" :error-bars '(:percentile 5 95)
    :add-time-and-experiment-to-file-name nil)
|#

;;;; avg cxn score per cxn type (alist monitor)
(define-monitor record-avg-holophrase-cxn-score
                :class 'data-recorder :average-window 100)

(define-monitor export-avg-holophrase-cxn-score
                :class 'lisp-data-file-writer
                :data-sources '((average record-avg-holophrase-cxn-score))
                :file-name (babel-pathname :name "avg-holophrase-cxn-score" :type "lisp"
                                           :directory '("experiments" "clevr-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-avg-holophrase-cxn-score interaction-finished)
  (record-value
   monitor
   (average
    (mapcar #'cxn-score
            (find-all 'holophrase (constructions-list (grammar (learner experiment)))
                      :key #'get-cxn-type)))))

(define-monitor record-avg-item-based-cxn-score
                :class 'data-recorder :average-window 100)

(define-monitor export-avg-item-based-cxn-score
                :class 'lisp-data-file-writer
                :data-sources '((average record-avg-item-based-cxn-score))
                :file-name (babel-pathname :name "avg-item-based-cxn-score" :type "lisp"
                                           :directory '("experiments" "clevr-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-avg-item-based-cxn-score interaction-finished)
  (record-value
   monitor
   (average
    (mapcar #'cxn-score
            (find-all 'item-based (constructions-list (grammar (learner experiment)))
                      :key #'get-cxn-type)))))

(define-monitor record-avg-lexical-cxn-score
                :class 'data-recorder :average-window 100)

(define-monitor export-avg-lexical-cxn-score
                :class 'lisp-data-file-writer
                :data-sources '((average record-avg-lexical-cxn-score))
                :file-name (babel-pathname :name "avg-lexical-cxn-score" :type "lisp"
                                           :directory '("experiments" "clevr-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-avg-lexical-cxn-score interaction-finished)
  (record-value
   monitor
   (average
    (mapcar #'cxn-score
            (find-all 'lexical (constructions-list (grammar (learner experiment)))
                      :key #'get-cxn-type)))))

#|
(define-monitor record-cxn-score-per-type
                :class 'alist-recorder
                :average-window 100)

(define-event-handler (record-cxn-score-per-type interaction-finished)
  (let ((all-constructions
         (constructions-list (grammar (learner experiment)))))
    (loop for cxn-type in '(holophrase lexical item-based)
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
                               :directory '("experiments" "clevr-learning" "graphs"))
    :graphic-type "pdf" :error-bars '(:percentile 5 95)
    :add-time-and-experiment-to-file-name nil)
|#

;; nr of item-based cxns with slots (alist monitor)
(define-monitor record-num-item-based-1
                :class 'data-recorder)

(define-monitor export-num-item-based-1
                :class 'lisp-data-file-writer
                :data-sources '(record-num-item-based-1)
                :file-name (babel-pathname :name "num-item-based-1" :type "lisp"
                                           :directory '("experiments" "clevr-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-num-item-based-1 interaction-finished)
  (record-value
   monitor
   (count 1 (find-all 'item-based (constructions-list (grammar (learner experiment)))
                      :key #'get-cxn-type)
          :key #'item-based-number-of-slots :test #'=)))

(define-monitor record-num-item-based-2
                :class 'data-recorder)

(define-monitor export-num-item-based-2
                :class 'lisp-data-file-writer
                :data-sources '(record-num-item-based-2)
                :file-name (babel-pathname :name "num-item-based-2" :type "lisp"
                                           :directory '("experiments" "clevr-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-num-item-based-2 interaction-finished)
  (record-value
   monitor
   (count 2 (find-all 'item-based (constructions-list (grammar (learner experiment)))
                      :key #'get-cxn-type)
          :key #'item-based-number-of-slots :test #'=)))

(define-monitor record-num-item-based-3
                :class 'data-recorder)

(define-monitor export-num-item-based-3
                :class 'lisp-data-file-writer
                :data-sources '(record-num-item-based-3)
                :file-name (babel-pathname :name "num-item-based-3" :type "lisp"
                                           :directory '("experiments" "clevr-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-num-item-based-3 interaction-finished)
  (record-value
   monitor
   (count 3 (find-all 'item-based (constructions-list (grammar (learner experiment)))
                      :key #'get-cxn-type)
          :key #'item-based-number-of-slots :test #'=)))

(define-monitor record-num-item-based-4
                :class 'data-recorder)

(define-monitor export-num-item-based-4
                :class 'lisp-data-file-writer
                :data-sources '(record-num-item-based-4)
                :file-name (babel-pathname :name "num-item-based-4" :type "lisp"
                                           :directory '("experiments" "clevr-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-num-item-based-4 interaction-finished)
  (record-value
   monitor
   (count 4 (find-all 'item-based (constructions-list (grammar (learner experiment)))
                      :key #'get-cxn-type)
          :key #'item-based-number-of-slots :test #'=)))

(define-monitor record-num-item-based-5
                :class 'data-recorder)

(define-monitor export-num-item-based-5
                :class 'lisp-data-file-writer
                :data-sources '(record-num-item-based-5)
                :file-name (babel-pathname :name "num-item-based-5" :type "lisp"
                                           :directory '("experiments" "clevr-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-num-item-based-5 interaction-finished)
  (record-value
   monitor
   (count 5 (find-all 'item-based (constructions-list (grammar (learner experiment)))
                      :key #'get-cxn-type)
          :key #'item-based-number-of-slots :test #'=)))

(define-monitor record-num-item-based-6
                :class 'data-recorder)

(define-monitor export-num-item-based-6
                :class 'lisp-data-file-writer
                :data-sources '(record-num-item-based-6)
                :file-name (babel-pathname :name "num-item-based-6" :type "lisp"
                                           :directory '("experiments" "clevr-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-num-item-based-6 interaction-finished)
  (record-value
   monitor
   (count 6 (find-all 'item-based (constructions-list (grammar (learner experiment)))
                      :key #'get-cxn-type)
          :key #'item-based-number-of-slots :test #'=)))

#|
(define-monitor record-nr-of-slots
                :class 'alist-recorder
                :average-window 1)

(define-event-handler (record-nr-of-slots interaction-finished)
  (let* ((all-constructions
          (constructions-list (grammar (learner experiment))))
         (item-based-cxns
          (find-all 'item-based all-constructions :key #'get-cxn-type)))
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
                                   :directory '("experiments" "clevr-learning" "graphs"))
        :graphic-type "pdf" :error-bars '(:percentile 5 95)
        :add-time-and-experiment-to-file-name nil)
|#

;;;; number of unseen questions       
(define-monitor record-unseen-questions
                :class 'data-recorder
                :average-window 1) 

(define-monitor export-unseen-questions
                :class 'lisp-data-file-writer
                :documentation "Exports communicative success"
                :data-sources '(record-unseen-questions)
                :file-name (babel-pathname :name "number-of-unseen-questions" :type "lisp"
                                           :directory '("experiments" "clevr-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-unseen-questions log-unseen-questions)
  (record-value monitor n))

;;;; Number of chunks
(define-monitor record-number-of-chunks
                :class 'data-recorder
                :documentation "records the number of chunks.")

(define-monitor export-number-of-chunks
                :class 'lisp-data-file-writer
                :documentation "Exports the number of chunks."
                :data-sources '(record-number-of-chunks)
                :file-name (babel-pathname :name "number-of-chunks" :type "lisp"
                                           :directory '("experiments" "clevr-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-number-of-chunks interaction-finished)
  (record-value monitor (length (composer-chunks (learner experiment)))))

;;;; Composer search space size
(define-monitor record-composer-search-space-size
                :class 'data-recorder
                :average-window 1
                :documentation "records the size of the composer search space")

(define-monitor export-composer-search-space-size
                :class 'lisp-data-file-writer
                :documentation "exports the size of the composer search space"
                :data-sources '(record-composer-search-space-size)
                :file-name (babel-pathname :name "composer-search-space" :type "lisp"
                                           :directory '("experiments" "clevr-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-composer-search-space-size composer-solution-found)
  (record-value monitor (float
                         (/ (irl::node-counter composer)
                            (irl::node-depth (irl::node solution))))))


;;;; Number of nodes in the type hierarchy
(define-monitor record-number-of-nodes-in-th
                :class 'data-recorder
                :average-window 1
                :documentation "records the number of nodes in the type hierarchy")

(define-monitor export-number-of-nodes-in-th
                :class 'lisp-data-file-writer
                :documentation "exports the number of nodes in the type hierarchy"
                :data-sources '(record-number-of-nodes-in-th)
                :file-name (babel-pathname :name "number-of-th-nodes" :type "lisp"
                                           :directory '("experiments" "clevr-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-number-of-nodes-in-th interaction-finished)
  (record-value monitor
                (graph-utils::node-count
                 (type-hierarchies::graph
                  (get-type-hierarchy
                   (grammar
                    (learner experiment)))))))


;;;; Number of edges in the type hierarchy
(define-monitor record-number-of-edges-in-th
                :class 'data-recorder
                :average-window 1
                :documentation "records the number of edgesd in the type hierarchy")

(define-monitor export-number-of-edges-in-th
                :class 'lisp-data-file-writer
                :documentation "exports the number of edges in the type hierarchy"
                :data-sources '(record-number-of-edges-in-th)
                :file-name (babel-pathname :name "number-of-th-edges" :type "lisp"
                                           :directory '("experiments" "clevr-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-number-of-edges-in-th interaction-finished)
  (record-value monitor
                (graph-utils::edge-count
                 (type-hierarchies::graph
                  (get-type-hierarchy
                   (grammar
                    (learner experiment)))))))
                           

;; utilty function to get all of them
(defun get-all-lisp-monitors ()
  '("export-communicative-success"
    "export-lexicon-size"
    "export-lexical-meanings-per-form"
    "export-lexical-forms-per-meaning"
    "export-avg-cxn-score"
    ;"export-unseen-questions"
    ;"export-number-of-chunks"
    "export-composer-search-space-size"
    "export-number-of-nodes-in-th"
    "export-number-of-edges-in-th"
    "export-number-of-holophrase-cxns"
    "export-number-of-item-based-cxns"
    "export-number-of-lexical-cxns"
    "export-avg-holophrase-cxn-score"
    "export-avg-item-based-cxn-score"
    "export-avg-lexical-cxn-score"
    "export-num-item-based-1"
    "export-num-item-based-2"
    "export-num-item-based-3"
    "export-num-item-based-4"
    "export-num-item-based-5"
    "export-num-item-based-6"))
