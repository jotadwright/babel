;;;; monitors.lisp

(in-package :clevr-learning)

;;;; Helper functions

(defun get-cxns-of-type (agent type)
  (if (eql type 'all)
    (constructions-list (grammar agent))
    (find-all type (constructions-list (grammar agent))
              :key #'get-cxn-type)))

(defun cxn-score (cxn)
  (attr-val cxn :score))

;;;; Printing dots
(define-monitor print-a-dot-for-each-interaction
                :documentation "Prints a '.' for each interaction
                 and prints the number after :dot-interval")

(define-event-handler (print-a-dot-for-each-interaction interaction-finished)
  (cond ((= (interaction-number interaction) 1)
         (format t "~%."))
        ((= (mod (interaction-number interaction)
                 (get-configuration experiment :dot-interval)) 0)
         (format t ". (~a)~%" (interaction-number interaction))
         ;(wi:clear-page)
         )
        (t (format t "."))))

;;;; Communicative success
(define-monitor stream-communicative-success
                :class 'stream-monitor
                :file-name (babel-pathname :name "communicative-success" :type "csv"
                                           :directory '("experiments" "clevr-learning" "raw-data")))

(define-event-handler (stream-communicative-success interaction-finished)
  (record-value monitor (if (communicated-successfully interaction) 1 0)))
                

(define-monitor record-communicative-success
                :class 'data-recorder
                :average-window 100
                :documentation "records the game outcome of each game (1 or 0).")

(define-monitor display-communicative-success
                :class 'gnuplot-display
                :documentation "Plots the communicative success."
                :data-sources '((average record-communicative-success))
                :update-interval 1000
                :caption '("communicative success")
                :x-label "# Games" 
                :y1-label "Communicative Success" 
                :y1-max 1.0 :y1-min 0 
                :draw-y1-grid t)

(define-monitor export-communicative-success
                :class 'lisp-data-file-writer
                :documentation "Exports communicative success"
                :data-sources '((average record-communicative-success))
                :file-name (babel-pathname :name "communicative-success" :type "lisp"
                                           :directory '("experiments" "clevr-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :column-separator " "
                :comment-string "#")

(define-event-handler (record-communicative-success interaction-finished)
  (record-value monitor (if (communicated-successfully interaction) 1 0)))

;;;; Lexicon size
(define-monitor record-lexicon-size
                :class 'data-recorder
                :average-window 1
                :documentation "records the avg lexicon size.")

(define-monitor export-lexicon-size
                :class 'lisp-data-file-writer
                :documentation "Exports lexicon size"
                :data-sources '(record-lexicon-size)
                :file-name (babel-pathname :name "lexicon-size" :type "lisp"
                                           :directory '("experiments" "clevr-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :column-separator " "
                :comment-string "#")

(define-event-handler (record-lexicon-size interaction-finished)
  (record-value monitor (length (get-cxns-of-type (learner experiment) 'all))))

;;;; # meanings per form for lexical cxns
(define-monitor record-lexical-meanings-per-form
                :class 'data-recorder
                :average-window 1
                :documentation "records avg nr of meanings per form")

(define-monitor export-lexical-meanings-per-form
                :class 'lisp-data-file-writer
                :documentation "Exports nr of meanings per form for lexical cxns"
                :data-sources '(record-lexical-meanings-per-form)
                :file-name (babel-pathname :name "lexical-meanings-per-form" :type "lisp"
                                           :directory '("experiments" "clevr-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :column-separator " "
                :comment-string "#")

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
                :class 'data-recorder
                :average-window 1
                :documentation "records avg nr of forms per meaning")

(define-monitor export-lexical-forms-per-meaning
                :class 'lisp-data-file-writer
                :documentation "Exports nr of forms per meaning for lexical cxns"
                :data-sources '(record-lexical-forms-per-meaning)
                :file-name (babel-pathname :name "lexical-forms-per-meaning" :type "lisp"
                                           :directory '("experiments" "clevr-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :column-separator " "
                :comment-string "#")

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

;;;; Frequency of lexicon change
(define-monitor record-lexicon-change
                :class 'data-recorder
                :average-window 1000
                :documentation "records how often the lexicon changes")

(define-monitor export-lexicon-change
                :class 'lisp-data-file-writer
                :documentation "Exports how often the lexicon changes"
                :data-sources '(record-lexicon-change)
                :file-name (babel-pathname :name "lexicon-change" :type "lisp"
                                           :directory '("experiments" "clevr-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :column-separator " "
                :comment-string "#")

(define-event-handler (record-lexicon-change lexicon-changed)
  (record-value monitor 1))

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
                                           :directory '("experiments" "clevr-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :column-separator " "
                :comment-string "#")

(define-event-handler (record-avg-cxn-score interaction-finished)
  (record-value monitor (average (mapcar #'cxn-score (get-cxns-of-type (learner experiment) 'all)))))

;;;; learner confidence level
(define-monitor record-confidence-level
                :class 'data-recorder
                :average-window 100
                :documentation "record the confidence level")

(define-monitor export-confidence-level
                :class 'lisp-data-file-writer
                :documentation "exports confidence level"
                :data-sources '((average record-confidence-level))
                :file-name (babel-pathname :name "confidence-level" :type "lisp"
                                           :directory '("experiments" "clevr-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :column-separator " "
                :comment-string "#")

(define-event-handler (record-confidence-level interaction-finished)
  (record-value monitor (average (confidence-buffer experiment))))

;;;; lexicon size per cxn type (alist monitor)
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

(define-monitor plot-num-cxns-per-type
    :class 'alist-gnuplot-graphic-generator
    :recorder 'record-lexicon-size-per-type
    :average-windows 1
    :draw-y-grid t
    :y-label "Number of constructions"
    :x-label "Total number of interactions"
    :file-name (babel-pathname :name "num-cxns-per-type" :type "pdf"
                               :directory '("experiments" "clevr-learning" "graphs"))
    :graphic-type "pdf")

;;;; avg cxn score per cxn type (alist monitor)
(define-monitor record-cxn-score-per-type
                :class 'alist-recorder
                :average-window 100)

(define-event-handler (record-cxn-score-per-type interaction-finished)
  (let ((all-constructions
         (constructions-list (grammar (learner experiment)))))
    (loop for cxn-type in '(holophrase lexical item-based)
          for all-cxns-of-type = (find-all cxn-type all-constructions
                                           :key #'get-cxn-type)
          for cxn-scores = (mapcar #'(lambda (cxn) (attr-val cxn :score)) all-cxns-of-type)
          when all-cxns-of-type
          do (set-value-for-symbol monitor cxn-type (average cxn-scores)))))

(define-monitor plot-cxn-score-per-type
    :class 'alist-gnuplot-graphic-generator
    :recorder 'record-cxn-score-per-type
    :average-windows 100
    :draw-y-grid t
    :y-min 0 :y-max 1
    :y-label "Construction Score"
    :x-label "Total number of interactions"
    :file-name (babel-pathname :name "avg-cxn-score-per-type" :type "pdf"
                               :directory '("experiments" "clevr-learning" "graphs"))
    :graphic-type "pdf")

;; cxn usage per type (alist monitor)
(define-monitor record-cxn-usage-per-type
                :class 'alist-recorder
                :average-window 100
                :keep-previous-values t)

(define-event-handler (record-cxn-usage-per-type interaction-finished)
  (let ((applied-cxns (find-data (task-result (learner experiment)) 'applied-cxns)))
    (when applied-cxns
      (if (find 'holophrase applied-cxns :key #'get-cxn-type)
        (progn (set-value-for-symbol monitor 'holophrase 1)
          (set-value-for-symbol monitor 'item-based+lexical 0))
        (progn (set-value-for-symbol monitor 'holophrase 0)
          (set-value-for-symbol monitor 'item-based+lexical 1))))))
  
(define-monitor plot-cxn-usage-per-type
        :class 'alist-gnuplot-graphic-generator
        :recorder 'record-cxn-usage-per-type
        :average-windows 100
        :draw-y-grid t
        :y-min 0 :y-max 1
        :y-label "Usage"
        :x-label "Total number of interactions"
        :file-name (babel-pathname :name "cxn-usage-per-type" :type "pdf"
                                   :directory '("experiments" "clevr-learning" "graphs"))
        :graphic-type "pdf")

;; nr of item-based cxns with slots (alist monitor)
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
        :draw-y-grid t
        :y-label "Number of constructions"
        :x-label "Total number of interactions"
        :file-name (babel-pathname :name "nr-of-item-based-cxns-with-slots" :type "pdf"
                                   :directory '("experiments" "clevr-learning" "graphs"))
        :graphic-type "pdf")

;; export type hierarchy after series
(define-monitor export-type-hierarchy)

(defun export-type-hierarchy (agent)
  (let ((th (get-type-hierarchy (grammar agent)))
        (filename (pathname-name
                   (parse-namestring
                    (make-file-name-with-time "learned-type-hierarchy-~a")))))
    (type-hierarchy->image th :render-program "circo" :weights? t
                           :path (babel-pathname :directory '("experiments" "clevr-learning" "graphs"))
                           :file-name filename :format "pdf")))

(define-event-handler (export-type-hierarchy run-series-finished)
  (export-type-hierarchy (learner experiment)))

;; export type hierarchy every nth interaction
(define-monitor export-type-hierarchy-every-nth-interaction)

(define-event-handler (export-type-hierarchy-every-nth-interaction interaction-finished)
  (let ((interaction-nr (interaction-number (current-interaction experiment)))
        (n (get-configuration-or-default experiment :export-interval 100)))
    (when (= (mod interaction-nr n) 0)
      (let ((th (get-type-hierarchy (grammar (learner experiment))))
            (filename (pathname-name
                       (parse-namestring
                        (make-file-name-with-time
                         (format nil "learned-type-hierarchy-~a"
                                 interaction-nr))))))
        (type-hierarchy-components->images
         ;type-hierarchy->image
         th :render-program "circo" :weights? t
         :path (babel-pathname :directory '("experiments" "clevr-learning" "graphs"))
         :file-name filename :format "pdf"
         :minimum-component-size 1
         )))))

;; export grammar after series
(define-monitor export-learner-grammar)

(defun export-grammar (cxn-inventory pathname)
  (let ((path (make-file-name-with-time pathname)))
    (cl-store:store cxn-inventory path)))

(define-event-handler (export-learner-grammar run-series-finished)
  (export-grammar (babel-pathname :directory '("experiments" "clevr-learning" "raw-data")
                                      :name "learner-grammar" :type "store")
                  (grammar (learner experiment))))

;; export grammar every nth interaction
(define-monitor export-learner-grammar-every-nth-interaction)

(define-event-handler (export-learner-grammar-every-nth-interaction interaction-finished)
  (let ((interaction-nr (interaction-number (current-interaction experiment)))
        (n (get-configuration-or-default experiment :export-interval 100)))
    (when (= (mod interaction-nr n) 0)
      (let ((pathname (babel-pathname :directory '("experiments" "clevr-learning" "raw-data")
                                      :name (format nil "learner-grammar-~a" interaction-nr)
                                      :type "store")))
        (export-grammar (grammar (learner experiment))
                        pathname)))))
    
