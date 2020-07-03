(in-package :clevr-learning)

;;;; This file contains functions related to the
;;;; agent's grammar

(defmethod cip-goal-test ((node cip-node) (mode (eql :evaluate-on-scene)))
  "Check if the meaning of the current cxn can be executed in this scene
   and leads to a single solution. Store this solution for efficiency reasons."
  (let* ((ontology (find-data (blackboard (construction-inventory node)) 'ontology))
         (irl-program (extract-meanings (left-pole-structure (car-resulting-cfs (cipn-car node))))))
    (when irl-program
      (let ((solutions (evaluate-irl-program irl-program ontology)))
        (when (= (length solutions) 1)
          (let* ((solution (first solutions))
                 (answer (get-target-value irl-program solution)))
            (set-data (blackboard (construction-inventory node)) 'answer answer)))))))

(defun make-agent-cxn-set ()
  "Make the cxn set for the agent of the experiment"
  (let* ((grammar-name (make-const "learner-grammar"))
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

(defun variablify (symbol)
  "Turn a symbol into a variable if it isn't one yet."
  (if (variable-p symbol)
    symbol
    (intern (format nil "?~a" symbol))))

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

(define-event lexicon-changed)
(define-event question-cxn-added (cxn construction))
(define-event question-cxn-removed (cxn construction))

(defun add-holophrase-cxn (grammar utterance chunk interaction-nr &key (initial-score 0.5))
  "Add a cxn to the agent's grammar, mapping the question to the entire
   program. The question and chunk-id is added as attributes"
  (let* ((cxn-name (make-cxn-name utterance grammar))
         (meaning (irl-program chunk))
         (form-constraints (form-constraints-with-variables utterance (get-configuration grammar :de-render-mode))))
    (multiple-value-bind (cxn-inventory cxn)
        (eval
         `(def-fcg-cxn ,cxn-name
                       ((?holophrase-unit
                         (syn-cat (phrase-type holophrase)))
                        <-
                        (?holophrase-unit
                         (HASH meaning ,meaning)
                         --
                         (HASH form ,form-constraints)))
                       :cxn-inventory ,grammar
                       :attributes (:score ,initial-score
                                    :form ,utterance
                                    :meaning ,(id chunk)
                                    :added ,interaction-nr)))
      (declare (ignorable cxn-inventory))
      (notify question-cxn-added cxn)
      (notify lexicon-changed)
      cxn)))

(defun remove-holophrase-cxn (agent cxn)
  "Remove the given cxn and possibly also remove
   the chunk associated with it (if it is no longer
   used by other cxns)"
  (setf (constructions (grammar agent))
        (remove cxn (constructions (grammar agent))))
  (push-data (blackboard (grammar agent)) 'trash cxn)
  (notify lexicon-changed)
  (notify question-cxn-removed cxn))

(defun get-form-competitors (agent cxn)
  "Get cxns with the same meaning as cxn"
  (remove cxn
          (find-all (attr-val cxn :meaning)
                    (constructions (grammar agent))
                    :key #'(lambda (cxn) (attr-val cxn :meaning)))))

(defun get-meaning-competitors (agent cxn)
  "Get cxns with the same form as cxn"
  (remove cxn
          (find-all (attr-val cxn :form)
                    (constructions (grammar agent))
                    :key #'(lambda (cxn) (attr-val cxn :form))
                    :test #'string=)))

(define-event cxn-rewarded (cxn construction))
(define-event cxn-punished (cxn construction))

(defun inc-cxn-score (cxn &key (delta 0.1) (upper-bound 1.0))
  "increase the score of the cxn"
  (incf (attr-val cxn :score) delta)
  (when (> (attr-val cxn :score) upper-bound)
    (setf (attr-val cxn :score) upper-bound))
  (notify cxn-rewarded cxn)
  cxn)

(defun dec-cxn-score (agent cxn &key (delta 0.1) (lower-bound 0.0)
                            (remove-on-lower-bound t))
  "decrease the score of the cxn.
   remove it when it reaches 0"
  (decf (attr-val cxn :score) delta)
  (notify cxn-punished cxn)
  (when (<= (attr-val cxn :score) lower-bound)
    (if remove-on-lower-bound
      (remove-holophrase-cxn agent cxn)
      (setf (attr-val cxn :score) lower-bound)))
  (grammar agent))