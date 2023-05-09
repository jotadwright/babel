(in-package :duckie-language-learning)

(defun get-cxn-type (cxn)
  (attr-val cxn :cxn-type))

(defun cxn-score (cxn)
  (attr-val cxn :score))

(defun handle-duckie-punctuation (utterance)
  ;; The utterance should start with an uppercase
  ;; letter and have a question mark at the end.
  ;; The semicolon (if present) should be attached
  ;; to the word in front.
  
  ;; A function from the CLEVR-era.
  (format nil "~@(~a~)?"
          (if (search ";" utterance)
            (loop with words = nil
                  for word in (split utterance #\space)
                  if (string= word ";")
                    do (push (mkstr (pop words) word) words)
                  else do (push word words)
                  finally (return
                           (list-of-strings->string
                            (reverse words))))
            utterance)))

(defun cipn-utterance (cipn)
  (handle-duckie-punctuation
   (list-of-strings->string
    (render
     (extract-forms
      (left-pole-structure
       (initial-cfs (cip cipn))))
     (get-configuration
      (construction-inventory cipn)
      :render-mode)))))

(defun find-cxn-by-type-form-and-meaning (type form meaning cxn-inventory)
  "returns a cxn with the same meaning and form if it's in the cxn-inventory"
  (loop for cxn in (find-all type (constructions-list cxn-inventory) :key #'get-cxn-type)
        when (and (irl:equivalent-irl-programs? form (extract-form-predicates cxn))
                  (irl:equivalent-irl-programs? meaning (extract-meaning-predicates cxn)))
          return cxn))

(defun form-predicates->hash-string (form-predicates)
  ;; the last string predicate
  (third
   (last-elt
    (find-all 'string form-predicates
              :key #'first))))

(defun meaning-predicates->hash-meaning (meaning-predicates)
  (let* ((all-primitives
          (mapcar #'first meaning-predicates))
         (all-primitives-but-bind
          (remove 'bind all-primitives))
         (target-variable
          (get-target-var meaning-predicates)))
    ;; if there are only bind statements
    (if (null all-primitives-but-bind)
      ;; take the last element of the first binding
      (last-elt (first (find-all 'bind meaning-predicates :key #'first)))
      ;; otherwise, take the primitive that holds the target var
      (first (find target-variable meaning-predicates :key #'second)))))

;; Node check
(defun some-applied-repair-in-tree (node)
  "Some node in the tree can be added by a repair
   luckily, the handle-fix methods write this
   on the blackboard of the initial node, so
   we don't have to traverse the entire tree."
  (multiple-value-bind (some-repair-applied-p foundp)
      (find-data (initial-node node) :some-repair-applied)
    (when foundp some-repair-applied-p)))

;; Process input from user
(defun process-text-from-capi (agent text)
  ;; text from capi needs to be symbol to find in irl ontology, integers also need to be symbols
  (let* ((correct-answer (intern (upcase text)))
         (answer (loop for fields in (data-fields (ontology agent))
                       if (listp (cdr fields))
                         when (find correct-answer (cdr fields)
                                    :key #'id :test #'equal)
                           return it)))
    (if (string= (first (split-sequence:split-sequence  #\Space text)) "car-in")
      (let ((new-car (copy-object (get-data *ontology* 'agent-car))))
        (setf (zone new-car) (intern (upcase (second (split-sequence:split-sequence  #\Space text)))))
        new-car)
      answer)))

(defun ask-correct-answer (agent)
  (let* ((answer-string (downcase (capi:prompt-for-string "Tell me the correct answer please?")))
         (answer (process-text-from-capi agent answer-string)))
    (set-data (blackboard (grammar agent)) :ground-truth-topic answer)
    answer))

 ;; returns either the concept from the ontology or when the answer was "car-in zone-1" a car object with position zone-1.
(defun ask-answer (agent)
  (let* ((answer-string (downcase (capi:prompt-for-string "I'm afraid I didn't understand your question, what would the answer be?")))
         (answer (process-text-from-capi agent answer-string)))
    (set-data (blackboard (grammar agent)) :ground-truth-topic answer)
    answer))

(defun confirm-answer (answer)
  (if (eq (type-of answer) 'duckie-agent-car)
    (capi:prompt-for-confirmation (format nil "Moving duckie-agent-car to: ~a" (zone answer)))
    (capi:prompt-for-confirmation (format nil "I think the answer is: ~a" (id answer)))))
