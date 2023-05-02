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
