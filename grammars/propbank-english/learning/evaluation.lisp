(in-package :propbank-english)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                              ;;
;; Evaluating propbank-english grammars.                        ;;
;;                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun comprehend-and-evaluate (list-of-propbank-sentences cxn-inventory &key (timeout 60) (core-roles-only t)
                                                           (selected-rolesets nil) (include-word-sense t) (include-timed-out-sentences t)
                                                           (silent nil))
  (let ((output-file (babel-pathname :directory '(".tmp")
                                     :name "results"
                                     :type "store")))

    (evaluate-propbank-corpus list-of-propbank-sentences cxn-inventory :output-file output-file :timeout timeout :silent silent)

    (let ((predictions (restore (babel-pathname :directory '(".tmp")
                                                :name "results"
                                                :type "store"))))
    
      (evaluate-predictions predictions
                            :core-roles-only core-roles-only
                            :selected-rolesets selected-rolesets
                            :include-word-sense include-word-sense 
                            :include-timed-out-sentences include-timed-out-sentences))))
 

(defun evaluate-propbank-corpus (list-of-propbank-sentences cxn-inventory &key (output-file nil) (timeout 60) (silent t))
  "Runs FCG comprehend on a corpus of sentences and stores the solutions to an external file."
  (let ((output-file (or output-file
                         (babel-pathname :directory '(".tmp")
                                         :name (format nil "~a~a" (multiple-value-bind (sec min hour day month year)
                                                               (decode-universal-time (get-universal-time))
                                                             (format nil "~d-~2,'0d-~2,'0d-~2,'0d-~2,'0d-~2,'0d-"
                                                                     year month day hour min sec)) "evaluation")
                                         :type "store"))))
    (loop for sentence in list-of-propbank-sentences
          for sentence-number from 1
          do (format t "~%Sentence ~a: ~a" sentence-number (sentence-string sentence))
          collect (let* ((cipn (second (multiple-value-list (comprehend-and-extract-frames sentence :cxn-inventory cxn-inventory :silent silent :timeout timeout))))
                         (utterance (sentence-string sentence))
                         (annotation (propbank-frames sentence)))
                    (if (eql cipn 'time-out)
                      (progn (format t " --> timed out .~%")
                        (list utterance annotation 'time-out))
                      (let ((solution (remove-if-not #'frame-with-name (frames (extract-frames (car-resulting-cfs (cipn-car cipn)))))))
                        (format t " --> done .~%")
                        (list utterance annotation solution))))
          into evaluation
          finally (cl-store:store evaluation output-file))))

(defun evaluate-predictions (predictions &key (core-roles-only t) (selected-rolesets nil) (include-word-sense t) (include-timed-out-sentences t))
  "Computes precision, recall and F1 score for a given list of predictions."
  (loop for (nil annotation solution) in predictions
        when (or include-timed-out-sentences
                 (not (eql solution 'time-out)))
        ;;gold standard predictions
        sum (loop for frame in annotation
                  for frame-name = (if include-word-sense
                                     (frame-name frame)
                                     (truncate-frame-name (frame-name frame)))
                  if (or (null selected-rolesets)
                         (find frame-name selected-rolesets :test #'equalp))
                  sum (loop for role in (frame-roles frame)
                            if core-roles-only
                            sum (if (core-role-p role)
                                  (length (indices role)) 0)
                            else sum (length (indices role))))
        into number-of-gold-standard-predictions
        ;;grammar predictions
        unless (eql solution 'time-out)
        sum (loop for predicted-frame in solution
                  for frame-name = (if include-word-sense
                                     (symbol-name (frame-name predicted-frame))
                                     (truncate-frame-name (symbol-name (frame-name predicted-frame))))
                  when (and frame-name
                            (or (null selected-rolesets)
                                (find frame-name selected-rolesets :test #'equalp))
                           ; (if selected-rolesets
                              (find (truncate-frame-name frame-name) annotation
                                    :key #'(lambda (frame)
                                             (truncate-frame-name (frame-name frame)))
                                    :test #'equalp))
                             ; t))
                  sum (+ (loop for role in (frame-elements predicted-frame)
                               if core-roles-only
                               sum (if (core-role-p role)
                                     (length (indices role)) 0)
                               else sum (length (indices role)))
                         1)) ;;FEE
        into number-of-grammar-predictions
        ;;correct predictions
        unless (eql solution 'time-out)
        sum (loop for predicted-frame in solution
                  for frame-name = (if include-word-sense
                                     (symbol-name (frame-name predicted-frame))
                                     (truncate-frame-name (symbol-name (frame-name predicted-frame))))
                  when (and frame-name
                            (or (null selected-rolesets)
                                (find frame-name selected-rolesets :test #'equalp)))
                  sum (+ (loop for predicted-frame-element in (frame-elements predicted-frame) ;;frame elements
                            for predicted-indices = (indices predicted-frame-element)
                            if core-roles-only
                            sum (if (core-role-p predicted-frame-element)
                                  (loop for index in predicted-indices
                                        when (correctly-predicted-index-p index predicted-frame-element predicted-frame
                                                                           annotation include-word-sense)
                                         sum 1)
                                  0)
                            else sum (loop for index in predicted-indices
                                           when (correctly-predicted-index-p index predicted-frame-element predicted-frame
                                                                             annotation include-word-sense)
                                           sum 1))
                         (if (correctly-predicted-fee-index-p (indices (frame-evoking-element predicted-frame)) ;;FEE
                                                              predicted-frame annotation include-word-sense)
                           1 0)))
        into number-of-correct-predictions
        finally (let ((evaluation-result `((:precision . ,(compute-precision number-of-correct-predictions number-of-grammar-predictions))
                                           (:recall . ,(compute-recall number-of-correct-predictions number-of-gold-standard-predictions))
                                           (:f1-score . ,(compute-f1-score number-of-correct-predictions number-of-grammar-predictions number-of-gold-standard-predictions))
                                           (:nr-of-correct-predictions . ,number-of-correct-predictions)
                                           (:nr-of-predictions . ,number-of-grammar-predictions)
                                           (:nr-of-gold-standard-predictions . ,number-of-gold-standard-predictions))))
                  (format t "~%~%~%############## EVALUATION RESULTS ##############~%")
                  (format t "~a" evaluation-result)
                  (return evaluation-result))))
      
  
(defun correctly-predicted-index-p (index predicted-frame-element predicted-frame gold-frames include-word-sense)
  "Returns t if the index form the predicted-frame occurs in the same role of the same frame in the gold-standard annotation."
  (let ((predicted-frame-name (if include-word-sense
                                (frame-name predicted-frame)
                                (truncate-frame-name (frame-name predicted-frame))))
        (predicted-role (fe-role predicted-frame-element)))
    (loop for gold-frame in gold-frames
          for gold-frame-name = (if include-word-sense
                                  (frame-name gold-frame)
                                  (truncate-frame-name (frame-name gold-frame)))
          if (when (and (equalp gold-frame-name (symbol-name predicted-frame-name))
                        (equalp (indices (frame-evoking-element predicted-frame))
                             (indices (find "V" (frame-roles gold-frame) :key #'role-type :test #'equalp))))
               (loop for gold-role in (find-all (symbol-name predicted-role) (frame-roles gold-frame) :key #'role-type :test #'equalp)
                     if (find index (indices gold-role))
                     return t))
          do
          (return t))))

(defun correctly-predicted-fee-index-p (indices predicted-frame gold-frames include-word-sense)
  "Returns t if the index form the frame-evoking-element occurs in the same role of the same frame in the gold-standard annotation."
  (let ((predicted-frame-name (if include-word-sense
                                (frame-name predicted-frame)
                                (truncate-frame-name (frame-name predicted-frame)))))
    (loop for gold-frame in gold-frames
          for gold-frame-name = (if include-word-sense
                                  (frame-name gold-frame)
                                  (truncate-frame-name (frame-name gold-frame)))
          if (and (equalp gold-frame-name (symbol-name predicted-frame-name))
                  (find "V" (frame-roles gold-frame) :key #'role-type :test #'equalp)
                  (equalp indices (indices (find "V" (frame-roles gold-frame) :key #'role-type :test #'equalp))))
          do
          (return t))))

(defmethod frame-with-name ((frame frame))
  "Return t if the frame has a non-variable name."
  (not (equalp "?" (subseq (symbol-name (frame-name frame)) 0 1))))

(defmethod core-role-p ((role propbank-frame-role))
  (unless (search "ARGM" (role-type role) :test #'equalp)
    t))
 
(defmethod core-role-p ((role frame-element))
  (unless (search "ARGM" (symbol-name (fe-role role)) :test #'equalp)
    t))


;; Precision, Recall and F1-score ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compute-precision (nr-of-correct-predictions total-nr-of-predictions)
  "Computes Precision."
  (when (> total-nr-of-predictions 0)
    (float (/ nr-of-correct-predictions total-nr-of-predictions))))

(defun compute-recall (nr-of-correct-predictions nr-of-gold-standard-predictions)
  "Computes Recall."
  (when (> nr-of-gold-standard-predictions 0)
    (float (/ nr-of-correct-predictions nr-of-gold-standard-predictions))))

(defun compute-f1-score (nr-of-correct-predictions total-nr-of-predictions nr-of-gold-standard-predictions)
  "Computes F1-Score."
  (let ((precision (compute-precision nr-of-correct-predictions total-nr-of-predictions))
        (recall (compute-recall nr-of-correct-predictions nr-of-gold-standard-predictions)))
    (when (and precision recall (> (+ precision recall) 0.0))
      (float (* 2 (/ (* precision recall) (+ precision recall)))))))
