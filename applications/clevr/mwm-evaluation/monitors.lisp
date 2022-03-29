(in-package :mwm-evaluation)

(defun get-all-monitors ()
  '("print-a-dot-for-each-interaction"
   "log-mwm-evaluation"
   "export-count!-primitive"
   "export-equal?-primitive"
   "export-equal-integer-primitive"
   "export-less-than-primitive"
   "export-greater-than-primitive"
   "export-exist-primitive"
   "export-filter-primitive"
   "export-intersect-primitive"
   "export-query-primitive"
   "export-relate-primitive"
   "export-same-primitive"
   "export-union!-primitive"
   "export-unique-primitive"))
       

;;;; print a dot for each interaction

(define-monitor print-a-dot-for-each-interaction
                :documentation "Prints a '.' for each interaction
                 and prints the number after :dot-interval")

(define-event-handler (print-a-dot-for-each-interaction interaction-finished)
  (cond ((= interaction-number 1) (format t "~%."))
        ((= (mod interaction-number
                 (get-configuration experiment :dot-interval)) 0)
         (format t ". (~a)~%" interaction-number))
        (t (format t "."))))


;;;; log the evaluation to a stream

(define-monitor log-mwm-evaluation
                :class 'stream-monitor
                :file-name (babel-pathname :directory '("applications" "clevr" "mwm-evaluation")
                                           :name "mwm-evaluation" :type "csv")
                :interaction-separator #\linefeed
                :series-separator #\space)

(define-event-handler (log-mwm-evaluation question-evaluation)
  ;; scene-name question answer computed-answer result
  (let ((csv-line
         (format nil "~a, ~a, ~a, ~a, ~a"
                 scene-name question answer
                 computed-answer result)))
    (record-value monitor csv-line)))


;;;; collect data per primitive
;; count!
(define-monitor record-count!-primitive
                :class 'data-recorder)

(define-event-handler (record-count!-primitive question-evaluation)
  (record-value
   monitor
   (when (find 'count! irl-program :key #'first)
     result)))

(define-monitor export-count!-primitive
                :class 'lisp-data-file-writer
                :file-name (babel-pathname :directory '("applications" "clevr" "mwm-evaluation")
                                           :name "count!" :type "log")
                :data-sources '(record-count!-primitive))

;; equal?
(define-monitor record-equal?-primitive
                :class 'data-recorder)

(define-event-handler (record-equal?-primitive question-evaluation)
  (record-value
   monitor
   (when (find 'equal? irl-program :key #'first)
     result)))

(define-monitor export-equal?-primitive
                :class 'lisp-data-file-writer
                :file-name (babel-pathname :directory '("applications" "clevr" "mwm-evaluation")
                                           :name "equal?" :type "log")
                :data-sources '(record-equal?-primitive))

;; equal-integer
(define-monitor record-equal-integer-primitive
                :class 'data-recorder)

(define-event-handler (record-equal-integer-primitive question-evaluation)
  (record-value
   monitor
   (when (find 'equal-integer irl-program :key #'first)
     result)))

(define-monitor export-equal-integer-primitive
                :class 'lisp-data-file-writer
                :file-name (babel-pathname :directory '("applications" "clevr" "mwm-evaluation")
                                           :name "equal-integer" :type "log")
                :data-sources '(record-equal-integer-primitive))

;; less-than
(define-monitor record-less-than-primitive
                :class 'data-recorder)

(define-event-handler (record-less-than-primitive question-evaluation)
  (record-value
   monitor
   (when (find 'less-than irl-program :key #'first)
     result)))

(define-monitor export-less-than-primitive
                :class 'lisp-data-file-writer
                :file-name (babel-pathname :directory '("applications" "clevr" "mwm-evaluation")
                                           :name "less-than" :type "log")
                :data-sources '(record-less-than-primitive))

;; greater-than
(define-monitor record-greater-than-primitive
                :class 'data-recorder)

(define-event-handler (record-greater-than-primitive question-evaluation)
  (record-value
   monitor
   (when (find 'greater-than irl-program :key #'first)
     result)))

(define-monitor export-greater-than-primitive
                :class 'lisp-data-file-writer
                :file-name (babel-pathname :directory '("applications" "clevr" "mwm-evaluation")
                                           :name "greater-than" :type "log")
                :data-sources '(record-greater-than-primitive))

;; exist
(define-monitor record-exist-primitive
                :class 'data-recorder)

(define-event-handler (record-exist-primitive question-evaluation)
  (record-value
   monitor
   (when (find 'exist irl-program :key #'first)
     result)))

(define-monitor export-exist-primitive
                :class 'lisp-data-file-writer
                :file-name (babel-pathname :directory '("applications" "clevr" "mwm-evaluation")
                                           :name "exist" :type "log")
                :data-sources '(record-exist-primitive))

;; filter
(define-monitor record-filter-primitive
                :class 'data-recorder)

(define-event-handler (record-filter-primitive question-evaluation)
  (record-value
   monitor
   (when (find 'filter irl-program :key #'first)
     result)))

(define-monitor export-filter-primitive
                :class 'lisp-data-file-writer
                :file-name (babel-pathname :directory '("applications" "clevr" "mwm-evaluation")
                                           :name "filter" :type "log")
                :data-sources '(record-filter-primitive))

;; intersect
(define-monitor record-intersect-primitive
                :class 'data-recorder)

(define-event-handler (record-intersect-primitive question-evaluation)
  (record-value
   monitor
   (when (find 'intersect irl-program :key #'first)
     result)))

(define-monitor export-intersect-primitive
                :class 'lisp-data-file-writer
                :file-name (babel-pathname :directory '("applications" "clevr" "mwm-evaluation")
                                           :name "intersect" :type "log")
                :data-sources '(record-intersect-primitive))

;; query
(define-monitor record-query-primitive
                :class 'data-recorder)

(define-event-handler (record-query-primitive question-evaluation)
  (record-value
   monitor
   (when (find 'query irl-program :key #'first)
     result)))

(define-monitor export-query-primitive
                :class 'lisp-data-file-writer
                :file-name (babel-pathname :directory '("applications" "clevr" "mwm-evaluation")
                                           :name "query" :type "log")
                :data-sources '(record-query-primitive))

;; relate
(define-monitor record-relate-primitive
                :class 'data-recorder)

(define-event-handler (record-relate-primitive question-evaluation)
  (record-value
   monitor
   (when (find 'relate irl-program :key #'first)
     result)))

(define-monitor export-relate-primitive
                :class 'lisp-data-file-writer
                :file-name (babel-pathname :directory '("applications" "clevr" "mwm-evaluation")
                                           :name "relate" :type "log")
                :data-sources '(record-relate-primitive))

;; same
(define-monitor record-same-primitive
                :class 'data-recorder)

(define-event-handler (record-same-primitive question-evaluation)
  (record-value
   monitor
   (when (find 'same irl-program :key #'first)
     result)))

(define-monitor export-same-primitive
                :class 'lisp-data-file-writer
                :file-name (babel-pathname :directory '("applications" "clevr" "mwm-evaluation")
                                           :name "same" :type "log")
                :data-sources '(record-same-primitive))

;; union!
(define-monitor record-union!-primitive
                :class 'data-recorder)

(define-event-handler (record-union!-primitive question-evaluation)
  (record-value
   monitor
   (when (find 'union! irl-program :key #'first)
     result)))

(define-monitor export-union!-primitive
                :class 'lisp-data-file-writer
                :file-name (babel-pathname :directory '("applications" "clevr" "mwm-evaluation")
                                           :name "union!" :type "log")
                :data-sources '(record-union!-primitive))

;; unique
(define-monitor record-unique-primitive
                :class 'data-recorder)

(define-event-handler (record-unique-primitive question-evaluation)
  (record-value
   monitor
   (when (find 'unique irl-program :key #'first)
     result)))

(define-monitor export-unique-primitive
                :class 'lisp-data-file-writer
                :file-name (babel-pathname :directory '("applications" "clevr" "mwm-evaluation")
                                           :name "unique" :type "log")
                :data-sources '(record-unique-primitive))
       
    




  
                
