;;;; grammar.lisp

(in-package :clevr-learning)

(defun empty-cxn-set ()
  (let* ((grammar-name (make-const "clevr-learning-grammar"))
         (cxn-inventory
          (eval `(def-fcg-constructions ,grammar-name
                   :cxn-inventory ,grammar-name
                   :feature-types ((args sequence)
                                   (form set-of-predicates)
                                   (meaning set-of-predicates)
                                   (subunits set)
                                   (footprints set))
                   :fcg-configurations ((:cxn-supplier-mode . :scores))
                   :visualization-configurations ((:show-constructional-dependencies . nil))))))
    cxn-inventory))

(defgeneric make-cxn-name (thing cxn-inventory &key add-cxn-suffix))

(defmethod make-cxn-name ((string string)
                          (cxn-inventory fcg-construction-set)
                          &key (add-cxn-suffix t))
  "Transform an utterance into a suitable construction name"
  (declare (ignore cxn-inventory))
  (make-const
   (substitute #\- #\Space
               (remove-spurious-spaces
                (upcase
                 (remove-punctuation
                  (if add-cxn-suffix
                    (string-append string "-cxn")
                    string)))))))

(defun form-constraints-with-variables (utterance mode)
  "Extract form constraints as if they would appear in a construction."
  (let ((form-constraints-with-constants
         (remove 'sequence
                 (extract-forms (left-pole-structure
                                 (de-render utterance mode)))
                 :key #'first)))
    (mapcar #'(lambda (form-constraint)
                (cons (first form-constraint)
                      (if (equal 'string (first form-constraint))
                        (list (variablify (second form-constraint)) (third form-constraint))
                        (mapcar #'variablify (rest form-constraint)))))
            form-constraints-with-constants)))

(define-event new-holophrase-cxn (cxn fcg-construction))
(define-event lexicon-changed)

(defun add-holophrase-cxn (grammar form chunk &key (initial-score 0.5))
  (let* ((cxn-name (make-cxn-name form grammar))
         (form-constraints
          (form-constraints-with-variables
           form (get-configuration grammar :de-render-mode))))
    (multiple-value-bind (cxn-inventory cxn)
        (eval
         `(def-fcg-cxn ,cxn-name
                       ((?holophrase-unit
                         (syn-cat (phrase-type holophrase)))
                        <-
                        (?holophrase-unit
                         (HASH meaning ,(irl-program chunk))
                         --
                         (HASH form ,form-constraints)))
                       :cxn-inventory ,grammar
                       :attributes (:score ,initial-score
                                    :form ,form
                                    :meaning ,(id chunk))))
      (declare (ignorable cxn-inventory))
      (notify new-holophrase-cxn cxn)
      (notify lexicon-changed)
      cxn)))

(defun inc-cxn-score (cxn &key (delta 0.1) (upper-bound 1.0))
  "increase the score of the cxn"
  (incf (attr-val cxn :score) delta)
  (when (> (attr-val cxn :score) upper-bound)
    (setf (attr-val cxn :score) upper-bound))
  cxn)

(defun dec-cxn-score (agent cxn &key (delta 0.1) (lower-bound 0.0)
                            (remove-on-lower-bound t))
  "decrease the score of the cxn.
   remove it when it reaches 0"
  (decf (attr-val cxn :score) delta)
  (when (<= (attr-val cxn :score) lower-bound)
    (if remove-on-lower-bound
      (progn (notify lexicon-changed)
        (with-disabled-monitor-notifications
          (delete-cxn cxn (grammar agent))))
      (setf (attr-val cxn :score) lower-bound)))
  (grammar agent))

(defun get-form-competitors (agent cxns)
  "Get cxns with the same meaning as cxn"
  (let ((all-cxns-with-meaning
         (remove-duplicates
          (loop for cxn in cxns
                append (find-all (attr-val cxn :meaning)
                                 (constructions-list (grammar agent))
                                 :key #'(lambda (cxn) (attr-val cxn :meaning)))))))
    (loop for cxn in cxns
          do (delete cxn all-cxns-with-meaning))
    all-cxns-with-meaning))

(defun get-meaning-competitors (agent cxns)
  "Get cxns with the same form as cxn"
  (let ((all-cxns-with-form
         (remove-duplicates
          (loop for cxn in cxns
                append (find-all (attr-val cxn :form)
                                 (constructions-list (grammar agent))
                                 :key #'(lambda (cxn) (attr-val cxn :form))
                                 :test #'string=)))))
    (loop for cxn in cxns
          do (setf all-cxns-with-form
                   (remove cxn all-cxns-with-form)))
    all-cxns-with-form))
  