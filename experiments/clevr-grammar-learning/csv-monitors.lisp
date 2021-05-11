;;;; monitors.lisp

(in-package :clevr-grammar-learning)

;;;; Communicative success             
(define-monitor record-csv-communicative-success
                :class 'data-recorder
                :documentation "records the game outcome of each game (1 or 0).")

(define-monitor export-csv-communicative-success
                :class 'csv-data-file-writer
                :documentation "Exports communicative success"
                :data-sources '(record-csv-communicative-success)
                :file-name (babel-pathname :name "communicative-success" :type "csv"
                                           :directory '("experiments" "clevr-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :comment-string "#"
                :column-separator ",")

(define-event-handler (record-csv-communicative-success interaction-finished)
  (record-value monitor (if (communicated-successfully interaction) 1 0)))

;;;; Lexicon size
(define-monitor record-csv-lexicon-size
                :class 'data-recorder
                :documentation "records the avg lexicon size.")

(define-monitor export-csv-lexicon-size
                :class 'csv-data-file-writer
                :documentation "Exports lexicon size"
                :data-sources '(record-csv-lexicon-size)
                :file-name (babel-pathname :name "lexicon-size" :type "csv"
                                           :directory '("experiments" "clevr-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :comment-string "#"
                :column-separator ",")

(define-event-handler (record-csv-lexicon-size interaction-finished)
  (record-value monitor (length (get-cxns-of-type (learner experiment) 'all))))

;;;; # meanings per form for lexical cxns
(define-monitor record-csv-lexical-meanings-per-form
                :class 'data-recorder
                :documentation "records avg nr of meanings per form")

(define-monitor export-csv-lexical-meanings-per-form
                :class 'csv-data-file-writer
                :documentation "Exports nr of meanings per form for lexical cxns"
                :data-sources '(record-csv-lexical-meanings-per-form)
                :file-name (babel-pathname :name "lexical-meanings-per-form" :type "csv"
                                           :directory '("experiments" "clevr-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :comment-string "#"
                :column-separator ",")

(define-event-handler (record-csv-lexical-meanings-per-form interaction-finished)
   (record-value monitor
                 (compute-nr-of-lexical-meanings-per-form
                  (learner experiment))))

;;;; # forms per meaning for lexical cxns
(define-monitor record-csv-lexical-forms-per-meaning
                :class 'data-recorder
                :documentation "records avg nr of forms per meaning")

(define-monitor export-csv-lexical-forms-per-meaning
                :class 'csv-data-file-writer
                :documentation "Exports nr of forms per meaning for lexical cxns"
                :data-sources '(record-csv-lexical-forms-per-meaning)
                :file-name (babel-pathname :name "lexical-forms-per-meaning" :type "csv"
                                           :directory '("experiments" "clevr-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :comment-string "#"
                :column-separator ",")

(define-event-handler (record-csv-lexical-forms-per-meaning interaction-finished)
   (record-value monitor
                 (compute-nr-of-lexical-forms-per-meaning
                  (learner experiment))))

;;;; Avg cxn score
(define-monitor record-csv-avg-cxn-score
                :class 'data-recorder
                :documentation "record the avg cxn score")

(define-monitor export-csv-avg-cxn-score
                :class 'csv-data-file-writer
                :documentation "exports avg cxn score"
                :data-sources '(record-csv-avg-cxn-score)
                :file-name (babel-pathname :name "avg-cxn-score" :type "csv"
                                           :directory '("experiments" "clevr-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :comment-string "#"
                :column-separator ",")   

(define-event-handler (record-csv-avg-cxn-score interaction-finished)
  (record-value monitor (average (mapcar #'cxn-score (get-cxns-of-type (learner experiment) 'all)))))

;;;; lexicon size per cxn type (alist monitor)
(define-monitor record-csv-lexicon-size-per-type
                :class 'alist-recorder
                :average-window 1)

(define-monitor export-csv-lexicon-size-per-type
                :class 'alist-csv-file-writer
                :documentation "Exports lexicon size per type"
                :recorder 'record-csv-lexicon-size-per-type
                :file-name (babel-pathname :name "lexicon-size-per-type" :type "csv"
                                           :directory '("experiments" "clevr-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :comment-string "#" :column-separator ",")

(define-event-handler (record-csv-lexicon-size-per-type interaction-finished)
  (let ((all-constructions
         (constructions-list (grammar (learner experiment)))))
    (loop for cxn-type in '(holophrase lexical item-based)
          for all-cxns-of-type = (find-all cxn-type all-constructions
                                           :key #'get-cxn-type)
          when all-cxns-of-type
          do (set-value-for-symbol monitor cxn-type (length all-cxns-of-type)))))

;;;; avg cxn score per cxn type (alist monitor)
(define-monitor record-csv-cxn-score-per-type
                :class 'alist-recorder
                :average-window 1)

(define-monitor export-csv-cxn-score-per-type
                :class 'alist-csv-file-writer
                :documentation "Exports cxn score per type"
                :recorder 'record-csv-cxn-score-per-type
                :file-name (babel-pathname :name "cxn-score-per-type" :type "csv"
                                           :directory '("experiments" "clevr-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :comment-string "#" :column-separator ",")

(define-event-handler (record-csv-cxn-score-per-type interaction-finished)
  (let ((all-constructions
         (constructions-list (grammar (learner experiment)))))
    (loop for cxn-type in '(holophrase lexical item-based)
          for all-cxns-of-type = (find-all cxn-type all-constructions
                                           :key #'get-cxn-type)
          for cxn-scores = (mapcar #'cxn-score all-cxns-of-type)
          when all-cxns-of-type
          do (set-value-for-symbol monitor cxn-type (average cxn-scores)))))

;; cxn usage per type (alist monitor)
(define-monitor record-csv-cxn-usage-per-type
                :class 'alist-recorder
                :average-window 1)

(define-monitor export-csv-cxn-usage-per-type
                :class 'alist-csv-file-writer
                :documentation "Exports cxn usage per type"
                :recorder 'record-csv-cxn-usage-per-type
                :file-name (babel-pathname :name "cxn-usage-per-type" :type "csv"
                                           :directory '("experiments" "clevr-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :comment-string "#" :column-separator ",")

(define-event-handler (record-csv-cxn-usage-per-type constructions-chosen)
  (if (find 'holophrase constructions :key #'get-cxn-type)
    (progn (set-value-for-symbol monitor 'holophrase 1)
      (set-value-for-symbol monitor 'item-based+lexical 0))
    (progn (set-value-for-symbol monitor 'holophrase 0)
      (set-value-for-symbol monitor 'item-based+lexical 1))))

;; nr of item-based cxns with slots (alist monitor)
(define-monitor record-csv-nr-of-slots
                :class 'alist-recorder
                :average-window 1)

(define-monitor export-csv-nr-of-slots
                :class 'alist-csv-file-writer
                :documentation "Exports cxn usage per type"
                :recorder 'record-csv-nr-of-slots
                :file-name (babel-pathname :name "item-based-nr-of-slots" :type "csv"
                                           :directory '("experiments" "clevr-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :comment-string "#" :column-separator ",")

(define-event-handler (record-csv-nr-of-slots interaction-finished)
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


;; utilty function to get all of them
(defun get-all-csv-monitors ()
  '("export-csv-communicative-success"
    "export-csv-lexicon-size"
    "export-csv-lexical-meanings-per-form"
    "export-csv-lexical-forms-per-meaning"
    "export-csv-avg-cxn-score"
    "export-csv-lexicon-size-per-type"
    "export-csv-cxn-score-per-type"
    "export-csv-cxn-usage-per-type"
    "export-csv-nr-of-slots"))
