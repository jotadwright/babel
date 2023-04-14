;;;; interaction.lisp

(in-package :duckie-language-learning)


;; ---------------
;; + Interaction +
;; ---------------
              

(defun execute (irl-program agent)
  (let* ((solutions (evaluate-irl-program irl-program (ontology agent)
                                          :primitive-inventory (primitive-inventory agent)))) 
    (if (equal (length solutions) 1)
      (let* ((solution (first solutions))
             (target-var (get-target-var irl-program))
             (answer (value (find target-var solution :key #'var))))
    answer))))

(defun confirm-answer (answer)
  (capi:prompt-for-confirmation (format nil "I think the answer is: ~a" (value answer))))


(defmethod interact ((experiment duckie-language-learning-experiment)
                     interaction &key)
  (let ((answer-correct? nil)
        (correct-answer nil)
        (fcg-solution? nil)
        (cipn nil)
        (agent (hearer interaction))
        (meaning nil))
  (setf (utterance (hearer interaction)) (downcase (capi:prompt-for-string "Enter your question:")))
  (multiple-value-bind (resulting-meaning resulting-cipn)
      (comprehend (utterance (hearer interaction))
                  :cxn-inventory (grammar (hearer interaction)))
    (setf cipn resulting-cipn)
    (setf meaning resulting-meaning)

    ;; comprehension werkt zonder repairs
    ;; dus vragen aan de user of het juist is
    (when (and (find 'fcg::succeeded (statuses cipn))
               (not (some-applied-repair-in-tree cipn)))
      (let* ((answer (execute meaning (hearer interaction))))
        (setf fcg-solution? t)
        (setf answer-correct? (confirm-answer answer))
        (setf correct-answer (if answer-correct? (value answer)
                               (downcase (capi:prompt-for-string "Tell me the correct answer please?")))))

      (cond (answer-correct?
             ;; joepie, rewarden!
             (run-alignment agent answer-correct? cipn))
            (fcg-solution?
             ;; spijtig, punishen!
             (run-alignment agent answer-correct? cipn)
             ;; en nieuwe holophrase leren
             (learn-through-intention-reading-and-pattern-finding agent correct-answer cipn)))))))

(defun learn-through-intention-reading-and-pattern-finding (agent correct-answer cipn)
  (let* ((answer-category
          (loop for fields in (data-fields (ontology agent))
                when (find (downcase correct-answer) (cdr fields)
                           :key #'value :test #'string=)
                  return it))
         (intention (compose-program agent answer-category))
         (holophrase-cxn (create-holophrase-cxn cipn intention)))
    (add-cxn holophrase-cxn (grammar agent))))
            
(defun run-alignment (agent answer-correct? cipn)
  (let ((applied-cxns (mapcar #'get-original-cxn
                              (applied-constructions cipn))))
    (loop for cxn in applied-cxns
          if answer-correct?
          do (progn (incf (attr-val cxn :score) 0.1)
               (when (> (attr-val cxn :score) 1.0)
                 (setf (attr-val cxn :score) 1.0)))
          else
          do (progn (decf (attr-val cxn :score) 0.1)
               (when (< (attr-val cxn :score) 0.0)
                 (delete-cxn cxn (grammar agent)))))))

(defun ask-correct-answer (agent)
  (let ((correct-answer
         (downcase (capi:prompt-for-string "I'm afraid I didn't understand your question, what would the answer be?"))))
    (loop for fields in (data-fields (ontology agent))
          when (find (downcase correct-answer) (cdr fields) :key #'value :test #'string=)
            return it)))

(defun some-applied-repair-in-tree (node)
  ;; some node in the tree can be added by a repair
  ;; luckily, the handle-fix methods write this
  ;; on the blackboard of the initial node, so
  ;; we don't have to traverse the entire tree
  (multiple-value-bind (some-repair-applied-p foundp)
      (find-data (gl::initial-node node) :some-repair-applied)
    (when foundp some-repair-applied-p)))
