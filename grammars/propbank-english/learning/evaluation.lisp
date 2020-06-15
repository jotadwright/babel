(in-package :propbank-english)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                              ;;
;; Evaluating propbank-english grammars.                        ;;
;;                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun evaluate-propbank-sentences (list-of-propbank-sentences cxn-inventory &key
                                                               (selected-rolesets nil) (silent nil) (print-to-standard-output t))
  "Returns a.o. precision, recall, F1 score for evaluation of list-of-propbank-sentences."
  ;; Precision = (#correct-predictions / #predictions)
  ;; Recall = (#correct-predictions / #gold-standard-predictions)
  ;; F1-score = 2 * ((precision * recall) / (precision + recall))
  (let ((evaluation-result (loop with number-of-correct-predictions = 0
                                 with number-of-predictions = 0
                                 with number-of-gold-standard-predictions = 0
                                 for sentence in list-of-propbank-sentences
                                 for sentence-number from 1
                                 for sentence-evaluation-result = (evaluate-propbank-sentence sentence cxn-inventory
                                                                                              :selected-rolesets selected-rolesets
                                                                                              :silent silent)
                                 
                                 do
                                 (when print-to-standard-output
                                   (format t "~%Sentence ~a: ~a~%" sentence-number (sentence-string sentence)))
                                 (setf number-of-correct-predictions (+ number-of-correct-predictions
                                                                        (cdr (assoc :nr-of-correct-predictions sentence-evaluation-result))))
                                 (setf number-of-predictions (+ number-of-predictions
                                                                (cdr (assoc :nr-of-predictions sentence-evaluation-result))))
                                 (setf number-of-gold-standard-predictions (+ number-of-gold-standard-predictions
                                                                              (cdr (assoc :nr-of-gold-standard-predictions sentence-evaluation-result))))
                                 finally return
                                 `((:precision . ,(compute-precision number-of-correct-predictions number-of-predictions))
                                   (:recall . ,(compute-recall number-of-correct-predictions number-of-gold-standard-predictions))
                                   (:f1-score . ,(compute-f1-score number-of-correct-predictions number-of-predictions number-of-gold-standard-predictions))
                                   (:nr-of-correct-predictions . ,number-of-correct-predictions)
                                   (:nr-of-predictions . ,number-of-predictions)
                                   (:nr-of-gold-standard-predictions . ,number-of-gold-standard-predictions)))))
    (when print-to-standard-output
      (format t "~%~%~%############## EVALUATION RESULTS ##############~%")
      (format t "~a~%" evaluation-result))
    evaluation-result))



(defun evaluate-propbank-sentences-per-roleset (list-of-propbank-sentences cxn-inventory &key
                                                                           (selected-rolesets nil) (silent nil) (print-to-standard-output t))
  "Returns a.o. precision, recall, F1 score for evaluation of list-of-propbank-sentences."
  (let* ((list-of-final-nodes (loop for sentence in list-of-propbank-sentences
                                    for sentence-number from 1
                                    do (format t "~%Comprehending sentence ~a: ~a~%" sentence-number (sentence-string sentence))
                                    collect (second (multiple-value-list
                                                     (comprehend sentence
                                                                 :cxn-inventory cxn-inventory
                                                                 :silent silent
                                                                 :selected-rolesets selected-rolesets)))))
         (all-rolesets (or selected-rolesets
                           (remove-duplicates (loop for sentence in list-of-propbank-sentences
                                                    append (mapcar #'frame-name (propbank-frames sentence)))
                                              :test #'equalp)))
         (evaluation-results-per-roleset (loop for roleset in all-rolesets
                                               collect (loop with number-of-correct-predictions = 0
                                                             with number-of-predictions = 0
                                                             with number-of-gold-standard-predictions = 0
                                                             for sentence in list-of-propbank-sentences
                                                             for sentence-number from 1
                                                             for sentence-evaluation-result = (evaluate-propbank-sentence sentence cxn-inventory
                                                                                                                          :selected-rolesets (list roleset)
                                                                                                                          :silent silent
                                                                                                                          :cipn (nth1 sentence-number list-of-final-nodes))
                                 
                                                             do
                                                             (when print-to-standard-output
                                                               (format t "~%Computing scores for sentence ~a: ~a~%" sentence-number (sentence-string sentence)))
                                                             (setf number-of-correct-predictions (+ number-of-correct-predictions
                                                                                                    (cdr (assoc :nr-of-correct-predictions sentence-evaluation-result))))
                                                             (setf number-of-predictions (+ number-of-predictions
                                                                                            (cdr (assoc :nr-of-predictions sentence-evaluation-result))))
                                                             (setf number-of-gold-standard-predictions (+ number-of-gold-standard-predictions
                                                                                                          (cdr (assoc :nr-of-gold-standard-predictions sentence-evaluation-result))))
                                                             finally
                                                             return
                                                             `((:precision . ,(compute-precision number-of-correct-predictions number-of-predictions))
                                                               (:recall . ,(compute-recall number-of-correct-predictions number-of-gold-standard-predictions))
                                                               (:f1-score . ,(compute-f1-score number-of-correct-predictions
                                                                                               number-of-predictions
                                                                                               number-of-gold-standard-predictions))
                                                               (:nr-of-correct-predictions . ,number-of-correct-predictions)
                                                               (:nr-of-predictions . ,number-of-predictions)
                                                               (:nr-of-gold-standard-predictions . ,number-of-gold-standard-predictions)))))
         (evaluation-results-overall (loop  with number-of-correct-predictions = 0
                                            with number-of-predictions = 0
                                            with number-of-gold-standard-predictions = 0
                                            for evaluation-result-per-roleset in evaluation-results-per-roleset
                                            do
                                            (setf number-of-correct-predictions
                                                  (+ number-of-correct-predictions (cdr (assoc :nr-of-correct-predictions evaluation-result-per-roleset))))
                                            (setf number-of-predictions
                                                  (+ number-of-predictions (cdr (assoc :nr-of-predictions evaluation-result-per-roleset))))
                                            (setf number-of-gold-standard-predictions
                                                  (+ number-of-gold-standard-predictions (cdr (assoc :nr-of-gold-standard-predictions evaluation-result-per-roleset))))
                                            finally
                                            return
                                            `((:precision . ,(compute-precision number-of-correct-predictions number-of-predictions))
                                              (:recall . ,(compute-recall number-of-correct-predictions number-of-gold-standard-predictions))
                                              (:f1-score . ,(compute-f1-score number-of-correct-predictions number-of-predictions number-of-gold-standard-predictions))
                                              (:nr-of-correct-predictions . ,number-of-correct-predictions)
                                              (:nr-of-predictions . ,number-of-predictions)
                                              (:nr-of-gold-standard-predictions . ,number-of-gold-standard-predictions)))))
    ;; Printing
    (when print-to-standard-output
      (format t "~%~%~%############## EVALUATION RESULTS ##############~%")
      (format t "~%############## Per Roleset ##############~%")
      (loop for roleset in all-rolesets
            for result in evaluation-results-per-roleset
            if (or (cdr (assoc :recall result)) (cdr (assoc :recall result)))
            do
            (format t "~a: ~a.~%" roleset result))
      (format t "~%############## Overall ##############~%")
      (format t "Overall: ~a.~%" evaluation-results-overall))
    evaluation-results-overall))



;; Evaluate an individual sentence ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun evaluate-propbank-sentence (propbank-sentence cxn-inventory &key (selected-rolesets nil) (silent nil) (syntactic-analysis nil) (cipn nil))
  "Evaluates a conll sentence in terms of number-of-predictions, number-of-correct-predictions and number-of-gold-standard-predictions."
  (let* ((final-node (or cipn
                         (second (multiple-value-list (comprehend propbank-sentence
                                                                  :cxn-inventory cxn-inventory
                                                                  :silent silent
                                                                  :syntactic-analysis syntactic-analysis
                                                                  :selected-rolesets selected-rolesets)))))
         (extracted-frame-set  (extract-frames (car-resulting-cfs (cipn-car final-node))))
         (extracted-frames (remove-if-not #'frame-with-name (frames extracted-frame-set)))
         ;; Number of gold-standard predictions
         (number-of-gold-standard-predictions (loop with number-of-gold-standard-predictions = 0
                                                    for frame in (propbank-frames propbank-sentence)
                                                    if (or (null selected-rolesets)
                                                           (find (frame-name frame) selected-rolesets :test #'equalp))
                                                    do (loop for role in (frame-roles frame)
                                                             do
                                                             (setf number-of-gold-standard-predictions (+ number-of-gold-standard-predictions (length (indices role)))))
                                                    finally
                                                    return number-of-gold-standard-predictions))
         ;; Number of predictions made by the grammar
         (number-of-predictions (loop with number-of-predictions = 0
                                      for frame in extracted-frames
                                      if (or (null selected-rolesets)
                                             (find (symbol-name (frame-name frame)) selected-rolesets :test #'equalp))
                                      do
                                      ;; for frame-elements
                                      (loop for role in (frame-elements frame)
                                            do
                                            (setf number-of-predictions (+ number-of-predictions (length (indices role)))))
                                      ;; from frame-evoking-element
                                      (when (and (frame-evoking-element frame) (index (frame-evoking-element frame)))
                                        (setf number-of-predictions (+ number-of-predictions 1)))
                                      finally
                                      return number-of-predictions))
         ;; Number of correct predictions made
         (number-of-correct-predictions (loop with number-of-correct-predictions = 0
                                              for predicted-frame in extracted-frames
                                              ;; check whether we're interested in the frame
                                              if (or (null selected-rolesets)
                                                     (find (symbol-name (frame-name predicted-frame)) selected-rolesets :test #'equalp))
                                              do
                                              ;; For frame elements
                                              (loop for predicted-frame-element in (frame-elements predicted-frame)
                                                    for predicted-indices = (indices predicted-frame-element)
                                                    do (loop for index in predicted-indices
                                                             when (correctly-predicted-index-p index predicted-frame-element predicted-frame (propbank-frames propbank-sentence))
                                                             do (setf number-of-correct-predictions (+ number-of-correct-predictions 1))))
                                              ;; For frame-evoking element
                                              (when (correctly-predicted-fee-index-p (index (frame-evoking-element predicted-frame))
                                                                                     predicted-frame
                                                                                     (propbank-frames propbank-sentence))
                                                (setf number-of-correct-predictions (+ number-of-correct-predictions 1)))
                                              finally
                                              return number-of-correct-predictions)))


    ;; Printing
    (unless silent
      (add-element `((h3 :style "margin-bottom:3px;") "Frame representation:"))
      (add-element (make-html extracted-frame-set :expand-initially t)))
    ;; Return the results as an a-list.
    `((:precision . ,(compute-precision number-of-correct-predictions number-of-predictions))
      (:recall . ,(compute-recall number-of-correct-predictions number-of-gold-standard-predictions))
      (:f1-score . ,(compute-f1-score number-of-correct-predictions number-of-predictions number-of-gold-standard-predictions))
      (:nr-of-correct-predictions . ,number-of-correct-predictions)
      (:nr-of-predictions . ,number-of-predictions)
      (:nr-of-gold-standard-predictions . ,number-of-gold-standard-predictions))))


(defun correctly-predicted-index-p (index predicted-frame-element predicted-frame gold-frames)
  "Returns t if the index form the predicted-frame occurs in the same role of the same frame in the gold-standard annotation."
  (let ((predicted-frame-name (frame-name predicted-frame))
        (predicted-role (fe-role predicted-frame-element)))
    (loop for gold-frame in gold-frames
          if (when (and (equalp (frame-name gold-frame) (symbol-name predicted-frame-name))
                        (eql (index (frame-evoking-element predicted-frame)) (first (indices (find "V" (frame-roles gold-frame) :key #'role-type :test #'equalp)))))
               (loop for gold-role in (find-all (symbol-name predicted-role) (frame-roles gold-frame) :key #'role-type :test #'equalp)
                     if (find index (indices gold-role))
                     return t))
          do
          (return t))))

(defun correctly-predicted-fee-index-p (index predicted-frame gold-frames)
  "Returns t if the index form the frame-evoking-element occurs in the same role of the same frame in the gold-standard annotation."
  (let ((predicted-frame-name (frame-name predicted-frame)))
    (loop for gold-frame in gold-frames
          if (and (equalp (frame-name gold-frame) (symbol-name predicted-frame-name))
                  (find "V" (frame-roles gold-frame) :key #'role-type :test #'equalp)
                  (find index (indices (find "V" (frame-roles gold-frame) :key #'role-type :test #'equalp))))
          do
          (return t))))

(defmethod frame-with-name ((frame frame))
  "Return t if the frame has a non-variable name."
  (not (equalp "?" (subseq (symbol-name (frame-name frame)) 0 1))))


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
