;;;; monitors.lisp

(in-package :grammar-learning)

;;;; Communicative success             
(define-monitor record-csv-communicative-success
                :class 'data-recorder
                :documentation "records the game outcome of each game (1 or 0).")

(define-monitor export-csv-communicative-success
                :class 'csv-data-file-writer
                :documentation "Exports communicative success"
                :data-sources '(record-csv-communicative-success)
                :file-name (babel-pathname :name "communicative-success" :type "csv"
                                           :directory '("experiments" "clevr-grammar-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :comment-string #\#
                :column-separator #\,)

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
                :file-name (babel-pathname :name "grammar-size" :type "csv"
                                           :directory '("experiments" "clevr-grammar-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :comment-string #\#
                :column-separator #\,)

(define-event-handler (record-csv-lexicon-size interaction-finished)
  (record-value monitor (length (get-cxns-of-type (learner experiment) 'all))))

;;;; Type hierarchy size
(define-monitor record-csv-th-size
                :class 'data-recorder
                :documentation "records the type hierarchy size.")

(define-monitor export-csv-th-size
                :class 'csv-data-file-writer
                :documentation "Exports type hierarchy size as number of edges"
                :data-sources '(record-csv-th-size)
                :file-name (babel-pathname :name "th-size" :type "csv"
                                           :directory '("experiments" "clevr-grammar-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :comment-string #\#
                :column-separator #\,)

(define-event-handler (record-csv-th-size interaction-finished)
  (record-value monitor (nr-of-links (grammar (learner experiment)))))

;;;; Avg cxn score
(define-monitor record-csv-avg-cxn-score
                :class 'data-recorder
                :documentation "record the avg cxn score")

(define-monitor export-csv-avg-cxn-score
                :class 'csv-data-file-writer
                :documentation "exports avg cxn score"
                :data-sources '(record-csv-avg-cxn-score)
                :file-name (babel-pathname :name "avg-cxn-score" :type "csv"
                                           :directory '("experiments" "clevr-grammar-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :comment-string #\#
                :column-separator #\,)

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
                :file-name (babel-pathname :name "grammar-size-per-type" :type "csv"
                                           :directory '("experiments" "clevr-grammar-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :comment-string "#"
                :column-separator ",")

(define-event-handler (record-csv-lexicon-size-per-type interaction-finished)
  (loop for cxn-type in '(gl::holophrase gl::lexical gl::item-based)
        for all-cxns-of-type = (get-cxns-of-type (learner experiment) cxn-type)
        when all-cxns-of-type
        do (set-value-for-symbol monitor cxn-type (length all-cxns-of-type))))

;;;; avg cxn score per cxn type (alist monitor)
(define-monitor record-csv-cxn-score-per-type
                :class 'alist-recorder
                :average-window 1)

(define-monitor export-csv-cxn-score-per-type
                :class 'alist-csv-file-writer
                :documentation "Exports cxn score per type"
                :recorder 'record-csv-cxn-score-per-type
                :file-name (babel-pathname :name "cxn-score-per-type" :type "csv"
                                           :directory '("experiments" "clevr-grammar-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :comment-string "#"
                :column-separator ",")

(define-event-handler (record-csv-cxn-score-per-type interaction-finished)
  (loop for cxn-type in '(gl::holophrase gl::lexical gl::item-based)
        for all-cxns-of-type = (get-cxns-of-type (learner experiment) cxn-type)
        for cxn-scores = (mapcar #'cxn-score all-cxns-of-type)
        when all-cxns-of-type
        do (set-value-for-symbol monitor cxn-type (average cxn-scores))))

;; cxn usage per type (alist monitor)
(define-monitor record-csv-cxn-usage-per-type
                :class 'alist-recorder
                :average-window 1)

(define-monitor export-csv-cxn-usage-per-type
                :class 'alist-csv-file-writer
                :documentation "Exports cxn usage per type"
                :recorder 'record-csv-cxn-usage-per-type
                :file-name (babel-pathname :name "cxn-usage-per-type" :type "csv"
                                           :directory '("experiments" "clevr-grammar-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :comment-string "#"
                :column-separator ",")

(define-event-handler (record-csv-cxn-usage-per-type constructions-chosen)
  (if (find 'gl::holophrase constructions :key #'get-cxn-type)
    (progn (set-value-for-symbol monitor 'holophrase 1)
      (set-value-for-symbol monitor 'item-based+lexical 0))
    (progn (set-value-for-symbol monitor 'holophrase 0)
      (set-value-for-symbol monitor 'item-based+lexical 1))))

;; repair per type (alist monitor)
(define-monitor record-csv-repair-per-type
                :class 'alist-recorder
                :average-window 1)

(define-event-handler (record-csv-repair-per-type interaction-finished)
  (let ((repair-symbol (last-elt (repair-buffer experiment))))
    (cond ((string= repair-symbol "h")
           (progn
             (set-value-for-symbol monitor 'nothing->holophrase 1)
             (set-value-for-symbol monitor 'lexical->item-based 0)
             (set-value-for-symbol monitor 'item-based->lexical 0)
             (set-value-for-symbol monitor 'holophrase->item-based+lexical+lexical--substitution 0)
             (set-value-for-symbol monitor 'holophrase->item-based+lexical+lexical--addition 0)
             (set-value-for-symbol monitor 'holophrase->item-based+lexical+lexical--deletion 0)
             (set-value-for-symbol monitor 'add-categorial-links 0)))
          ((string= repair-symbol "i")
           (progn
             (set-value-for-symbol monitor 'nothing->holophrase 0)
             (set-value-for-symbol monitor 'lexical->item-based 1)
             (set-value-for-symbol monitor 'item-based->lexical 0)
             (set-value-for-symbol monitor 'holophrase->item-based+lexical+lexical--substitution 0)
             (set-value-for-symbol monitor 'holophrase->item-based+lexical+lexical--addition 0)
             (set-value-for-symbol monitor 'holophrase->item-based+lexical+lexical--deletion 0)
             (set-value-for-symbol monitor 'add-categorial-links 0)))
          ((string= repair-symbol "l")
           (progn
             (set-value-for-symbol monitor 'nothing->holophrase 0)
             (set-value-for-symbol monitor 'lexical->item-based 0)
             (set-value-for-symbol monitor 'item-based->lexical 1)
             (set-value-for-symbol monitor 'holophrase->item-based+lexical+lexical--substitution 0)
             (set-value-for-symbol monitor 'holophrase->item-based+lexical+lexical--addition 0)
             (set-value-for-symbol monitor 'holophrase->item-based+lexical+lexical--deletion 0)
             (set-value-for-symbol monitor 'add-categorial-links 0)))
          ((string= repair-symbol "s")
           (progn
             (set-value-for-symbol monitor 'nothing->holophrase 0)
             (set-value-for-symbol monitor 'lexical->item-based 0)
             (set-value-for-symbol monitor 'item-based->lexical 0)
             (set-value-for-symbol monitor 'holophrase->item-based+lexical+lexical--substitution 1)
             (set-value-for-symbol monitor 'holophrase->item-based+lexical+lexical--addition 0)
             (set-value-for-symbol monitor 'holophrase->item-based+lexical+lexical--deletion 0)
             (set-value-for-symbol monitor 'add-categorial-links 0)))
          ((string= repair-symbol "a")
           (progn
             (set-value-for-symbol monitor 'nothing->holophrase 0)
             (set-value-for-symbol monitor 'lexical->item-based 0)
             (set-value-for-symbol monitor 'item-based->lexical 0)
             (set-value-for-symbol monitor 'holophrase->item-based+lexical+lexical--substitution 0)
             (set-value-for-symbol monitor 'holophrase->item-based+lexical+lexical--addition 1)
             (set-value-for-symbol monitor 'holophrase->item-based+lexical+lexical--deletion 0)
             (set-value-for-symbol monitor 'add-categorial-links 0)))
          ((string= repair-symbol "d")
           (progn
             (set-value-for-symbol monitor 'nothing->holophrase 0)
             (set-value-for-symbol monitor 'lexical->item-based 0)
             (set-value-for-symbol monitor 'item-based->lexical 0)
             (set-value-for-symbol monitor 'holophrase->item-based+lexical+lexical--substitution 0)
             (set-value-for-symbol monitor 'holophrase->item-based+lexical+lexical--addition 0)
             (set-value-for-symbol monitor 'holophrase->item-based+lexical+lexical--deletion 1)
             (set-value-for-symbol monitor 'add-categorial-links 0)))
          ((string= repair-symbol "t")
           (progn
             (set-value-for-symbol monitor 'nothing->holophrase 0)
             (set-value-for-symbol monitor 'lexical->item-based 0)
             (set-value-for-symbol monitor 'item-based->lexical 0)
             (set-value-for-symbol monitor 'holophrase->item-based+lexical+lexical--substitution 0)
             (set-value-for-symbol monitor 'holophrase->item-based+lexical+lexical--addition 0)
             (set-value-for-symbol monitor 'holophrase->item-based+lexical+lexical--deletion 0)
             (set-value-for-symbol monitor 'add-categorial-links 1))))))

(define-monitor export-csv-repair-per-type
        :class 'alist-csv-file-writer
                :documentation "Exports repair usage per type"
                :recorder 'record-csv-repair-per-type
                :file-name (babel-pathname :name "repair-per-type" :type "csv"
                                           :directory '("experiments" "clevr-grammar-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :comment-string "#"
                :column-separator ",")

;; nr of item-based cxns with slots (alist monitor)
(define-monitor record-csv-nr-of-slots
                :class 'alist-recorder
                :average-window 1)

(define-monitor export-csv-nr-of-slots
                :class 'alist-csv-file-writer
                :documentation "Exports cxn usage per type"
                :recorder 'record-csv-nr-of-slots
                :file-name (babel-pathname :name "item-based-nr-of-slots" :type "csv"
                                           :directory '("experiments" "clevr-grammar-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :comment-string "#"
                :column-separator ",")

(define-event-handler (record-csv-nr-of-slots interaction-finished)
  (let* ((item-based-cxns
          (get-cxns-of-type (learner experiment) 'gl::item-based)))
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
    "export-csv-th-size"
    "export-csv-avg-cxn-score"
    "export-csv-lexicon-size-per-type"
    "export-csv-cxn-score-per-type"
    "export-csv-cxn-usage-per-type"
    "export-csv-repair-per-type"
    "export-csv-nr-of-slots"))
