;;;; monitors.lisp

(in-package :clevr-learning)

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
         (wi:clear-page))
        (t (format t "."))))

;;;; Communicative success
(define-monitor record-communicative-success
                :class 'data-recorder
                :average-window 100
                :documentation "records the game outcome of each game (1 or 0).")

(define-monitor export-communicative-success
                :class 'lisp-data-file-writer
                :documentation "Exports communicative success"
                :data-sources '(record-communicative-success)
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
                :average-window 100
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

(defun get-lexicon-size (agent)
  (length (constructions-list (grammar agent))))

(define-event-handler (record-lexicon-size interaction-finished)
  (record-value monitor (get-lexicon-size (learner experiment))))

;;;; Number of meanings per form
(define-monitor record-meanings-per-form
                :class 'data-recorder
                :average-window 100
                :documentation "records avg nr of meanings per form")

(define-monitor export-meanings-per-form
                :class 'lisp-data-file-writer
                :documentation "Exports nr of meanings per form"
                :data-sources '(record-meanings-per-form)
                :file-name (babel-pathname :name "meanings-per-form" :type "lisp"
                                           :directory '("experiments" "clevr-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :column-separator " "
                :comment-string "#")

(defun compute-nr-of-meanings-per-form (agent)
  (loop with form-count = nil
        for cxn in (constructions-list (grammar agent))
        for cxn-form = (list-of-strings->string
                        (render (extract-form-predicates cxn)
                                (get-configuration (grammar agent) :render-mode)))
        for found = (assoc cxn-form form-count :test #'string=)
        if found do (incf (cdr found))
        else do (push (cons cxn-form 1) form-count)
        finally
        (return (average (mapcar #'cdr form-count)))))

(define-event-handler (record-meanings-per-form interaction-finished)
   (record-value monitor
                 (compute-nr-of-meanings-per-form
                  (learner experiment))))

;;;; Frequency of lexicon change
(define-monitor record-lexicon-change
                :class 'data-recorder
                :average-window 100
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
                :data-sources '(record-avg-cxn-score)
                :file-name (babel-pathname :name "avg-cxn-score" :type "lisp"
                                           :directory '("experiments" "clevr-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :column-separator " "
                :comment-string "#")

(defun compute-avg-cxn-score (agent)
  (average
   (mapcar #'(lambda (cxn)
               (attr-val cxn :score))
           (constructions-list (grammar agent)))))

(define-event-handler (record-avg-cxn-score interaction-finished)
  (record-value monitor (compute-avg-cxn-score (learner experiment))))

;;;; learner confidence level
(define-monitor record-confidence-level
                :class 'data-recorder
                :average-window 100
                :documentation "record the confidence level")

(define-monitor export-confidence-level
                :class 'lisp-data-file-writer
                :documentation "exports confidence level"
                :data-sources '(record-confidence-level)
                :file-name (babel-pathname :name "confidence-level" :type "lisp"
                                           :directory '("experiments" "clevr-learning" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :column-separator " "
                :comment-string "#")

(define-event-handler (record-confidence-level interaction-finished)
  (record-value monitor (average (confidence-buffer experiment))))

;;;; lexicon size per cxn type
(define-monitor record-lexicon-size-per-type
                :class 'alist-recorder
                :average-window 1)

(define-event-handler (record-lexicon-size-per-type interaction-finished)
  (let ((all-constructions
         (constructions-list (grammar (learner experiment)))))
    (loop with processed-cxn-types = nil
          for cxn in all-constructions
          for cxn-type = (attr-val cxn :cxn-type)
          for all-cxns-of-type = (find-all cxn-type all-constructions
                                           :key #'(lambda (cxn) (attr-val cxn :cxn-type)))
          unless (find cxn-type processed-cxn-types)
          do (push cxn-type processed-cxn-types)
          (set-value-for-symbol monitor cxn-type (length all-cxns-of-type)))))

(define-monitor plot-num-cxns-per-type
    :class 'alist-gnuplot-graphic-generator
    :recorder 'record-lexicon-size-per-type
    :average-windows 1
    :draw-y-1-grid t
    :y-label "Number of constructions"
    :x-label "Total number of interactions"
    :file-name (babel-pathname :name "num-cxns-per-type" :directory '("experiments" "clevr-learning" "graphs"))
    :graphic-type "pdf")




;;;; avg cxn score per cxn type
(define-monitor record-cxn-score-per-type
                :class 'alist-recorder
                :average-window 1)

(define-event-handler (record-cxn-score-per-type interaction-finished)
  (let ((all-constructions
         (constructions-list (grammar (learner experiment)))))
    (loop with processed-cxn-types = nil
          for cxn in all-constructions
          for cxn-type = (attr-val cxn :cxn-type)
          for all-cxns-of-type = (find-all cxn-type all-constructions
                                           :key #'(lambda (cxn) (attr-val cxn :cxn-type)))
          for cxn-scores = (mapcar #'(lambda (cxn) (attr-val cxn :score)) all-cxns-of-type)
          unless (find cxn-type processed-cxn-types)
          do (push cxn-type processed-cxn-types)
          (set-value-for-symbol monitor cxn-type (average cxn-scores)))))

(define-monitor plot-cxn-score-per-type
    :class 'alist-gnuplot-graphic-generator
    :recorder 'record-cxn-score-per-type
    :average-windows 1
    :draw-y-1-grid t
    :y-label "Construction Score"
    :x-label "Total number of interactions"
    :file-name (babel-pathname :name "avg-cxn-score-per-type" :directory '("experiments" "clevr-learning" "graphs"))
    :graphic-type "pdf")

