(ql:quickload :propbank-grammar)
(in-package :propbank-grammar)


;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(get-predictions-info "/Users/ehai-guest/Projects/babel/grammars/propbank-grammar/cleaning-and-evaluation/parameter-evaluation/predictions-parameter" "sentence_predictions_info.csv" "macro_predictions_info.csv")

(defun get-predictions-info (directory-path output-file-micro output-file-macro
                              &key (batch-size 1)
                                   (core-roles-only nil)
                                   (selected-rolesets nil)
                                   (include-word-sense t)
                                   (include-timed-out-sentences nil)
                                   (excluded-rolesets nil)
                                   (include-sentences-with-incomplete-role-constituent-mapping t))
  (let ((micro-predictions-info (get-evaluation-data-batches directory-path
                                                        :batch-size batch-size
                                                        :core-roles-only core-roles-only
                                                        :selected-rolesets selected-rolesets
                                                        :include-word-sense include-word-sense
                                                        :include-timed-out-sentences include-timed-out-sentences
                                                        :excluded-rolesets excluded-rolesets
                                                        :include-sentences-with-incomplete-role-constituent-mapping include-sentences-with-incomplete-role-constituent-mapping))
        (macro-predictions-info (get-evaluation-data-macro directory-path output-file-macro
                                                        :batch-size batch-size
                                                        :core-roles-only core-roles-only
                                                        :selected-rolesets selected-rolesets
                                                        :include-word-sense include-word-sense
                                                        :include-timed-out-sentences include-timed-out-sentences
                                                        :excluded-rolesets excluded-rolesets
                                                        :include-sentences-with-incomplete-role-constituent-mapping include-sentences-with-incomplete-role-constituent-mapping)))
    (save-predictions-info-csv output-file-micro micro-predictions-info)))


(defstruct sentence-evaluation-result
  prediction-nr
  sentence-stats
  evaluation-result)

(defun save-predictions-info-csv (file-path evaluation-data)
  "Saves the evaluation data to a CSV file."
  (with-open-file (out file-path
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    ;; Write the headers
    (format out "prediction_nr,sentence_id,tokens,frames,aux_frames,roles,core_roles,non_core_roles,elapsed_time,precision,recall,f1_score,nr_of_correct_predictions,nr_of_predictions,nr_of_gold_standard_predictions~%")
    ;; Write the data
    (dolist (result evaluation-data)
      (let* ((prediction-nr (sentence-evaluation-result-prediction-nr result))
             (sentence-stats (sentence-evaluation-result-sentence-stats result))
             (evaluation-result (sentence-evaluation-result-evaluation-result result))
             (sentence-stats-csv (prepare-sentence-stats-for-csv sentence-stats))
             (evaluation-result-csv (prepare-evaluation-result-for-csv evaluation-result)))
        (format out "~A,~A,~A~%" prediction-nr sentence-stats-csv evaluation-result-csv)))))

(defun prepare-sentence-stats-for-csv (stats)
  "Prepares the output of the collect-stats function for CSV export."
  (mapcar (lambda (stat)
            (format nil "~A,~A,~A,~A,~A,~A,~A,~A"
                    (getf stat :sentence-id)
                    (getf stat :tokens)
                    (getf stat :frames)
                    (getf stat :aux-frames)
                    (getf stat :roles)
                    (getf stat :core-roles)
                    (getf stat :non-core-roles)
                    (getf stat :elapsed-time)))
          stats))

(defun prepare-evaluation-result-for-csv (evaluation-result)
  "Prepares the output of the evaluate-predictions function for CSV export."
  (format nil "~A,~A,~A,~A,~A,~A"
          (cdr (assoc :precision evaluation-result))
          (cdr (assoc :recall evaluation-result))
          (cdr (assoc :f1-score evaluation-result))
          (cdr (assoc :nr-of-correct-predictions evaluation-result))
          (cdr (assoc :nr-of-predictions evaluation-result))
          (cdr (assoc :nr-of-gold-standard-predictions evaluation-result))))


(defun get-evaluation-data-batches (directory-path &key (batch-size 1) (core-roles-only t) (selected-rolesets nil) (include-word-sense t) (include-timed-out-sentences t) (excluded-rolesets nil) (include-sentences-with-incomplete-role-constituent-mapping t))
  "Process all files in a directory using a specified function and perform another action on the results."
  (let ((directory (uiop:directory-files directory-path))
        (results '()))
    (dolist (file directory)
      (let ((file-path-string (namestring file))
            (predictions-number ""))
        ;; Extract the predictions number from the file name
        (cl-ppcre:register-groups-bind (config-number-string)
            ("(\\d{4})\\.store" file-path-string)
          (setq predictions-number (parse-integer config-number-string)))
        ;; Evaluate predictions and save results
        (let ((predictions (restore-predictions file)))
          (let ((batch-results (evaluate-predictions-batch predictions :batch-size batch-size :core-roles-only core-roles-only :selected-rolesets selected-rolesets :include-word-sense include-word-sense :include-timed-out-sentences include-timed-out-sentences :excluded-rolesets excluded-rolesets :include-sentences-with-incomplete-role-constituent-mapping include-sentences-with-incomplete-role-constituent-mapping)))
            (dolist (result batch-results)
              (push (make-sentence-evaluation-result :prediction-nr predictions-number
                                            :sentence-stats (sentence-evaluation-result-sentence-stats result)
                                            :evaluation-result (sentence-evaluation-result-evaluation-result result))
                    results))))))
    (nreverse results)))

(defun evaluate-predictions-batch (predictions &key (batch-size 1) (core-roles-only t) (selected-rolesets nil) (include-word-sense t) (include-timed-out-sentences t) (excluded-rolesets nil) (include-sentences-with-incomplete-role-constituent-mapping t))
  "Evaluates predictions in batches of specified size."
  (let ((batch-results '())
        (batch-count 0)
        (prediction-nr 0))
    (loop for i from 0 to (- (length predictions) 1) by batch-size
          do (let ((batch (subseq predictions i (min (+ i batch-size) (length predictions))))
                   (batch-name (format nil "Batch-~A" (+ batch-count 1))))
               (format t "~&~A: ~%" batch-name)
               (let* ((evaluation-result (evaluate-predictions batch :core-roles-only core-roles-only :selected-rolesets selected-rolesets :include-word-sense include-word-sense :include-timed-out-sentences include-timed-out-sentences :excluded-rolesets excluded-rolesets :include-sentences-with-incomplete-role-constituent-mapping include-sentences-with-incomplete-role-constituent-mapping))
                      (sentence-stats (collect-stats batch)))
                 (push (make-sentence-evaluation-result :prediction-nr prediction-nr
                                               :sentence-stats sentence-stats
                                               :evaluation-result evaluation-result)
                       batch-results))
               (incf batch-count)))
    (nreverse batch-results)))


(defun save-batch-results (batch-results)
  "Stores the F1 scores and sentence stats for all batches in a text file. Appends the information for new batches and does not overwrite."
  (let* ((grammar-name (caar batch-results))
         (output-strings '())
         (file-name (format nil "f1-scores-batches-all-grammars"))
         (file-path (babel-pathname :directory '("grammars" "propbank-grammar" "cleaning-and-evaluation" "parameter-evaluation" "predictions-parameter")
                                    :name file-name
                                    :type "txt")))
    ;; loop over each batch result and format it as a string
    (dolist (batch-result batch-results)
      (let ((evaluation-result (car (first (cdr batch-result))))
            (sentence-stats (rest batch-result)))
        (dolist (stats sentence-stats)
          (let ((output-string (format nil "~a,~{~A~^,~}~%" grammar-name (mapcar #'second (append evaluation-result stats)))))
            (push output-string output-strings))))
      ;; write the strings to the output file
      (with-open-file (out-stream file-path :direction :output :if-does-not-exist :create :if-exists :append)
        (dolist (output-string (nreverse output-strings))
          (write-string output-string out-stream))))))


(defun get-evaluation-data-macro (directory-path file-name &key (batch-size 1) (core-roles-only t) (selected-rolesets nil) (include-word-sense t) (include-timed-out-sentences t) (excluded-rolesets nil) (include-sentences-with-incomplete-role-constituent-mapping t))
  "Process all files in a directory using a specified function and perform another action on the results."
  (let ((directory (uiop:directory-files directory-path))
        (results '()))
    (dolist (file directory)
      (let* ((predictions (restore-predictions file))
             (file-path-string (namestring file))
             (predictions-number ""))
        ;; Extract the predictions number from the file name
        (cl-ppcre:register-groups-bind (config-number-string)
            ("(\\d{4})\\.store" file-path-string)
          (setq predictions-number (parse-integer config-number-string)))
        (let ((evaluation-result (evaluate-predictions predictions :core-roles-only core-roles-only :selected-rolesets selected-rolesets :include-word-sense include-word-sense :include-timed-out-sentences include-timed-out-sentences :excluded-rolesets excluded-rolesets :include-sentences-with-incomplete-role-constituent-mapping include-sentences-with-incomplete-role-constituent-mapping)))
          (push (cons predictions-number evaluation-result) results))))
    (save-evaluation-results-to-csv results file-name))) 


(defun collect-stats (predictions)
  "Collects statistics from a list of predictions, where each prediction is a list containing a conll-sentence, annotation, solution, and elapsed time. Returns a list of stats for each individual sentence."
  (let ((aux-roles '("be.01" "be.02" "be.03"))
        stats)
    (dolist (prediction predictions)
      (let ((sentence (first prediction))
            (elapsed-time (nth 3 prediction))
            (sentence-tokens 0)
            (sentence-frames 0)
            (sentence-roles 0)
            (aux-frames 0)
            (core-roles 0)
            (non-core-roles 0))
        (setf sentence-tokens (length (tokens sentence)))
        (setf sentence-frames (length (propbank-frames sentence)))
        (dolist (frame (propbank-frames sentence))
          (incf sentence-roles (length (frame-roles frame)))
          (when (find (frame-name frame) aux-roles :test #'equalp)
            (incf aux-frames))
          (dolist (role (frame-roles frame))
            (if (core-role-p role)
              (incf core-roles)
              (incf non-core-roles))))
        ;; add elapsed time to the stats list
        (push (list :sentence-id (sentence-id sentence)
                    :tokens sentence-tokens
                    :frames sentence-frames
                    :aux-frames aux-frames
                    :roles sentence-roles
                    :core-roles core-roles
                    :non-core-roles non-core-roles
                    :elapsed-time elapsed-time)
              stats)))
    (nreverse stats)))

(defun save-evaluation-results-to-csv (evaluation-results file-name)
  "Save evaluation results to a CSV file."
  (with-open-file (out-stream file-name :direction :output
                              :if-exists :supersede :if-does-not-exist :create)
    (format out-stream "grammar_id,precision,recall,f1_score,nr_of_correct_predictions,nr_of_predictions,nr_of_gold_standard_predictions~%")
    (dolist (result evaluation-results)
      (format out-stream "~a,~a,~a,~a,~a,~a,~a~%"
              (car result)
              (get-alist-value (cdr result) :precision)
              (get-alist-value (cdr result) :recall)
              (get-alist-value (cdr result) :f1-score)
              (get-alist-value (cdr result) :nr-of-correct-predictions)
              (get-alist-value (cdr result) :nr-of-predictions)
              (get-alist-value (cdr result) :nr-of-gold-standard-predictions)))))

(defun restore-predictions (filename)
  "Restore variables from a binary file containing multiple CL-STORE objects."
  (with-open-file (in-stream filename :element-type '(unsigned-byte 8))
    (let ((objects '())
          (done nil))
      (loop until done
            do (let ((object (ignore-errors (cl-store:restore in-stream))))
                 (if object
                     (push object objects)
                     (setf done t))))
      (nreverse (apply #'append objects)))))

(defun get-alist-value (alist key)
  "Retrieve the value associated with a key in an alist."
  (cdr (assoc key alist)))