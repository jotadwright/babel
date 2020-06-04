(in-package :propbank-english)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                              ;;
;; Evaluating propbank-english grammars.                        ;;
;;                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun evaluate-propbank-sentences (list-of-propbank-sentences cxn-inventory &key
                                                               (selected-rolesets nil) (silent nil) (print-to-standard-output t)
                                                               (list-of-syntactic-analyses nil))
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
                                                                                              :silent silent
                                                                                              :syntactic-analysis (nth1 sentence-number list-of-syntactic-analyses))
                                 do
                                 (when print-to-standard-output
                                   (format t "~%Sentence ~a: ~a~%" sentence-number (sentence-string sentence)))
                                 (setf number-of-correct-predictions (+ number-of-correct-predictions (cdr (assoc :nr-of-correct-predictions sentence-evaluation-result))))
                                 (setf number-of-predictions (+ number-of-predictions (cdr (assoc :nr-of-predictions sentence-evaluation-result))))
                                 (setf number-of-gold-standard-predictions (+ number-of-gold-standard-predictions (cdr (assoc :nr-of-gold-standard-predictions sentence-evaluation-result))))
                                 finally
                                 (cond ((= 0 number-of-gold-standard-predictions)
                                        (return
                                         `((:precision . ,(if (= 0 number-of-predictions) 1.0 0.0))
                                           (:recall . 1.0)
                                           (:f1-score . ,(float (* 2 (/ (* (if (= 0 number-of-predictions) 1.0 0.0)
                                                                           1.0)
                                                                        (+ (if (= 0 number-of-predictions) 1.0 0.0)
                                                                           1.0)))))
                                           (:nr-of-correct-predictions . ,number-of-correct-predictions)
                                           (:nr-of-predictions . ,number-of-predictions)
                                           (:nr-of-gold-standard-predictions . ,number-of-gold-standard-predictions))))
                                       ((= 0 number-of-predictions)
                                        (return
                                         `((:precision . 1.0)
                                           (:recall . 0.0)
                                           (:f1-score . 0.0)
                                           (:nr-of-correct-predictions . ,number-of-correct-predictions)
                                           (:nr-of-predictions . ,number-of-predictions)
                                           (:nr-of-gold-standard-predictions . ,number-of-gold-standard-predictions))))
                                       ((= 0 number-of-correct-predictions)
                                        (return
                                         `((:precision . 0.0)
                                           (:recall . 0.0)
                                           (:f1-score . 0.0)
                                           (:nr-of-correct-predictions . ,number-of-correct-predictions)
                                           (:nr-of-predictions . ,number-of-predictions)
                                           (:nr-of-gold-standard-predictions . ,number-of-gold-standard-predictions))))
                                       (t
                                        (return
                                         `((:precision . ,(float (/ number-of-correct-predictions number-of-predictions)))
                                           (:recall . ,(float (/ number-of-correct-predictions number-of-gold-standard-predictions)))
                                           (:f1-score . ,(float (* 2 (/ (* (/ number-of-correct-predictions number-of-predictions)
                                                                           (/ number-of-correct-predictions number-of-gold-standard-predictions))
                                                                        (+ (/ number-of-correct-predictions number-of-predictions)
                                                                           (/ number-of-correct-predictions number-of-gold-standard-predictions))))))
                                           (:nr-of-correct-predictions . ,number-of-correct-predictions)
                                           (:nr-of-predictions . ,number-of-predictions)
                                           (:nr-of-gold-standard-predictions . ,number-of-gold-standard-predictions))))))))
    (when print-to-standard-output
      (format t "~%~%~%############## EVALUATION RESULTS ##############~%")
      (format t "~a~%" evaluation-result))
    evaluation-result))


(defun evaluate-propbank-sentence (propbank-sentence cxn-inventory &key (selected-rolesets nil) (silent nil) (syntactic-analysis nil))
  "Evaluates a conll sentence in terms of number-of-predictions, number-of-correct-predictions and number-of-gold-standard-predictions."
  (let* ((sentence-string (sentence-string propbank-sentence))
         (solution-and-cipn (multiple-value-list (comprehend sentence-string :cxn-inventory cxn-inventory :silent silent :syntactic-analysis syntactic-analysis :selected-rolesets selected-rolesets)))
         (cipn (second solution-and-cipn))
         (extracted-frames (extract-frames (car-resulting-cfs (cipn-car cipn))))
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
         ;; Number of predication made by the grammar
         (number-of-predictions (loop with number-of-predictions = 0
                                      for frame in (frames extracted-frames)
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
                                              for predicted-frame in (frames extracted-frames)
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
      (add-element (make-html extracted-frames :expand-initially t)))
    ;; Return the numbers as an a-list.
    `((:nr-of-correct-predictions . ,number-of-correct-predictions)
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


