
(in-package :fcg)

(defmethod comprehend ((utterance propbank-english::conll-sentence) &key (cxn-inventory *fcg-constructions*) (silent nil) (syntactic-analysis nil)
                       (selected-rolesets nil))
  (let ((initial-cfs (de-render (propbank-english::sentence-string utterance) (get-configuration cxn-inventory :de-render-mode) :cxn-inventory cxn-inventory
                                :syntactic-analysis syntactic-analysis))
        (processing-cxn-inventory (processing-cxn-inventory cxn-inventory)))
    ;; Add sentence annotation to blackboard
    (set-data initial-cfs :annotation (propbank-english::propbank-frames utterance))
    (set-data initial-cfs :selected-rolesets selected-rolesets)
    ;; Notification
    (unless silent (notify parse-started (listify utterance) initial-cfs))
    ;; Construction application
    (multiple-value-bind (solution cip)
        (fcg-apply processing-cxn-inventory initial-cfs '<- :notify (not silent))
      (let ((meaning (and solution
                          (extract-meanings
                           (left-pole-structure (car-resulting-cfs (cipn-car solution)))))))
        ;; Notification
        (unless silent (notify parse-finished meaning processing-cxn-inventory))
        ;; Return value
        (values meaning solution cip)))))

(defmethod comprehend ((utterance string) &key (syntactic-analysis nil) (cxn-inventory *fcg-constructions*)  (silent nil))
  (parse utterance (processing-cxn-inventory cxn-inventory) :silent silent :syntactic-analysis syntactic-analysis))

(defmethod parse ((utterance string) (construction-inventory construction-inventory)
                  &key (silent nil) (syntactic-analysis nil))
  (let ((initial-cfs (de-render utterance (get-configuration construction-inventory :de-render-mode)
                                :syntactic-analysis syntactic-analysis)))
    
    (set-data (blackboard construction-inventory) :input utterance)
                                       
    (unless silent (notify parse-started (listify utterance) initial-cfs))
    (multiple-value-bind
        (solution cip)
        (fcg-apply construction-inventory initial-cfs '<- :notify (not silent))
      (let ((meaning 
             (and solution
                  (extract-meanings
                   (left-pole-structure (car-resulting-cfs (cipn-car solution)))))))
        (unless silent (notify parse-finished meaning construction-inventory))
        (values meaning solution cip)))))



(in-package :propbank-english)

(defun comprehend-and-extract-frames (utterance &key (cxn-inventory *fcg-constructions*) (silent nil) (syntactic-analysis nil)
                                                (selected-rolesets nil))
  (multiple-value-bind (solution cipn)
      (comprehend utterance :cxn-inventory cxn-inventory :silent silent :syntactic-analysis syntactic-analysis :selected-rolesets selected-rolesets)
    (declare (ignore solution))
    (unless silent
      (add-element `((h3 :style "margin-bottom:3px;") "Frame representation:"))
      (add-element (make-html (extract-frames (car-resulting-cfs (cipn-car cipn))) :expand-initially t)))))




(defmethod hash ((construction construction)
                 (mode (eql :hash-lemma))
                 &key &allow-other-keys)
  "Returns the lemma from the attributes of the construction"
  (when (attr-val construction :lemma)
     (remove nil (list (attr-val construction :lemma)))))


(defmethod hash ((node cip-node)
                 (mode (eql :hash-lemma)) 
                 &key &allow-other-keys)
  "Checks all units for a lemma feature."
  (loop for unit in (fcg-get-transient-unit-structure node)
        for lemma = (unit-feature-value unit 'lemma)
        when lemma
        collect it))


(defmethod cip-node-test ((node cip-node) (mode (eql :check-double-role-assignment)))
  "Node test that checks if there is a frame in the resulting meaning
in which there are duplicate role assignments (i.e. unit name of
frame-element filler occurs in more than one slot). "
  (let ((extracted-frames (group-by (extract-meanings (left-pole-structure (car-resulting-cfs (cipn-car node))))
                                    #'third :test #'equalp)))
    (loop with double-role-assignments = nil
          for (frame-var . frame) in extracted-frames
          for frame-elements = (loop for predicate in frame
                                     when (equalp (first predicate) 'frame-element)
                                     collect predicate)
          
          when (or (> (length frame-elements) (length (remove-duplicates frame-elements :key #'fourth :test #'equalp)))
                    (loop for fe in frame-elements
                          for other-fes = (remove fe frame-elements :key #'fourth :test #'equalp)
                          thereis (subconstituent-p (fourth fe) (mapcar #'fourth other-fes) (left-pole-structure (car-resulting-cfs (cipn-car node))))))
          do (push frame-var double-role-assignments)
          finally
          return
          (if double-role-assignments
            ;;some frames contain frame-elements that have identical slot fillers
            (and (push 'double-role-assignment (statuses node)) nil)
            t))))

(defun subconstituent-p (frame-element other-frame-elements unit-structure)
  (loop for ofe in other-frame-elements
        when (subconstituent-p-aux frame-element ofe unit-structure)
        do (return t)))

(defun subconstituent-p-aux (frame-element other-frame-element unit-structure)
  (let ((parent (cadr (find 'parent (unit-body (find frame-element unit-structure :key #'unit-name)) :key #'feature-name))))
    (cond ((null parent)
           nil)
          ((equalp parent other-frame-element)
           t)
          ((subconstituent-p-aux parent other-frame-element unit-structure)
           t))))

(defmethod cip-goal-test ((node cip-node) (mode (eql :no-valid-children)))
  "Checks whether there are no more applicable constructions when a node is
fully expanded and no constructions could apply to its children
nodes."
  (and (or (not (children node))
	   (loop for child in (children node)
                 never (and (cxn-applied child)
                            (not (find 'double-role-assignment (statuses child))))))
       (fully-expanded? node)))


(defmethod cip-goal-test ((cipn cip-node) (mode (eql :gold-standard-meaning)))
  ""
  (or (and (or (not (children cipn))
	   (loop for child in (children cipn)
                 never (and (cxn-applied child)
                            (not (find 'double-role-assignment (statuses child))))))
       (fully-expanded? cipn))
      (let* ((extracted-frames (extract-frames (car-resulting-cfs (cipn-car cipn))))
             (selected-rolesets (get-data (car-resulting-cfs (cipn-car cipn)) :selected-rolesets))
             (annotated-frames (get-data (car-resulting-cfs (cipn-car cipn)) :annotation))
             (number-of-gold-standard-predictions (loop with number-of-gold-standard-predictions = 0
                                                        for frame in annotated-frames
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
                                                                 when (correctly-predicted-index-p index predicted-frame-element predicted-frame
                                                                                                   annotated-frames)
                                                                 do (setf number-of-correct-predictions (+ number-of-correct-predictions 1))))
                                                  ;; For frame-evoking element
                                                  (when (correctly-predicted-fee-index-p (index (frame-evoking-element predicted-frame))
                                                                                         predicted-frame
                                                                                         annotated-frames)
                                                    (setf number-of-correct-predictions (+ number-of-correct-predictions 1)))
                                                  finally
                                                  return number-of-correct-predictions))
             (result (cond ((= 0 number-of-gold-standard-predictions)
                            `((:precision . ,(if (= 0 number-of-predictions) 1.0 0.0))
                              (:recall . 1.0)
                              (:f1-score . ,(float (* 2 (/ (* (if (= 0 number-of-predictions) 1.0 0.0)
                                                              1.0)
                                                           (+ (if (= 0 number-of-predictions) 1.0 0.0)
                                                              1.0)))))
                              (:nr-of-correct-predictions . ,number-of-correct-predictions)
                              (:nr-of-predictions . ,number-of-predictions)
                              (:nr-of-gold-standard-predictions . ,number-of-gold-standard-predictions)))
                           ((= 0 number-of-predictions)
                            `((:precision . 1.0)
                              (:recall . 0.0)
                              (:f1-score . 0.0)
                              (:nr-of-correct-predictions . ,number-of-correct-predictions)
                              (:nr-of-predictions . ,number-of-predictions)
                              (:nr-of-gold-standard-predictions . ,number-of-gold-standard-predictions)))
                           ((= 0 number-of-correct-predictions)
                            `((:precision . 0.0)
                              (:recall . 0.0)
                              (:f1-score . 0.0)
                              (:nr-of-correct-predictions . ,number-of-correct-predictions)
                              (:nr-of-predictions . ,number-of-predictions)
                              (:nr-of-gold-standard-predictions . ,number-of-gold-standard-predictions)))
                           (t
                            `((:precision . ,(float (/ number-of-correct-predictions number-of-predictions)))
                              (:recall . ,(float (/ number-of-correct-predictions number-of-gold-standard-predictions)))
                              (:f1-score . ,(float (* 2 (/ (* (/ number-of-correct-predictions number-of-predictions)
                                                              (/ number-of-correct-predictions number-of-gold-standard-predictions))
                                                           (+ (/ number-of-correct-predictions number-of-predictions)
                                                              (/ number-of-correct-predictions number-of-gold-standard-predictions))))))
                              (:nr-of-correct-predictions . ,number-of-correct-predictions)
                              (:nr-of-predictions . ,number-of-predictions)
                              (:nr-of-gold-standard-predictions . ,number-of-gold-standard-predictions))))))
    
        (when (= (cdr (assoc :f1-score result)) 1.0)
          t))))



(defun all-rolesets-for-framenet-frame (framenet-frame-name)
  (loop for predicate in *pb-data*
        for rolesets = (rolesets predicate)
        for rolesets-for-framenet-frame = (loop for roleset in rolesets
                                                    when (find framenet-frame-name (aliases roleset) :key #'framenet :test #'member)
                                                    collect (id roleset))
        when rolesets-for-framenet-frame
        collect it))

;; (all-rolesets-for-framenet-frame 'opinion)


(defun all-sentences-annotated-with-roleset (roleset &key (split #'train-split)) ;;or #'dev-split
  (loop for sentence in (funcall split *propbank-annotations*)
        when (find roleset (propbank-frames sentence) :key #'frame-name :test #'equalp)
        collect sentence))

;; Retrieve all sentences in training set for a given roleset:
;; (all-sentences-annotated-with-roleset "believe.01")

;; Retrieve all sentences in de development set for a given roleset (for evaluation):
;; (length (all-sentences-annotated-with-roleset "believe.01" :split #'dev-split)) ;;call #'length for checking number


(defun print-propbank-sentences-with-annotation (roleset &key (split #'train-split))
  "Print the annotation of a given roleset for every sentence of the
split to the output buffer."
  (loop for sentence in (funcall split *propbank-annotations*)
        for sentence-string = (sentence-string sentence)
        for selected-frame = (loop for frame in (propbank-frames sentence)
                                   when (string= (frame-name frame) roleset)
                                   return frame)
        when selected-frame ;;only print if selected roleset is present in sentence
        do (let ((roles-with-indices (loop for role in (frame-roles selected-frame)
                                       collect (cons (role-type role) (role-string role)))))
             (format t "~a ~%" sentence-string)
             (loop for (role-type . role-string) in roles-with-indices
                   do (format t "~a: ~a ~%" role-type role-string)
                   finally (format t "~%")))))


;; (print-propbank-sentences-with-annotation "believe.01")


(defun fcg::equivalent-propbank-construction  (cxn-1 cxn-2)
  (cond ((eq 'fcg::processing-construction (type-of cxn-1))
         (and ;(equalp (name cxn-1) (name cxn-2))
              (= (length (right-pole-structure cxn-1)) (length (right-pole-structure cxn-2)))
              (equalp (remove nil (mapcar #'(lambda (unit)
                                                       (second (find 'lex-class (unit-body unit) :key #'first)))
                                                   (right-pole-structure cxn-1)))
                               (remove nil (mapcar #'(lambda (unit)
                                                       (second (find 'lex-class (unit-body unit) :key #'first)))
                                                   (right-pole-structure cxn-2))))
              (equalp (remove nil (mapcar #'(lambda (unit)
                                                       (second (find 'phrase-type (unit-body unit) :key #'first)))
                                                   (right-pole-structure cxn-1)))
                               (remove nil (mapcar #'(lambda (unit)
                                                       (second (find 'phrase-type (unit-body unit) :key #'first)))
                                                   (right-pole-structure cxn-2)))
                               )
              (equalp (remove nil (mapcar #'(lambda (unit)
                                                       (second (find 'lemma (unit-body unit) :key #'first)))
                                                   (right-pole-structure cxn-1)))
                               (remove nil (mapcar #'(lambda (unit)
                                                       (second (find 'lemma (unit-body unit) :key #'first)))
                                                   (right-pole-structure cxn-2))))))
  ((eq (type-of cxn-1) 'fcg-construction)
   (and (= (length (conditional-part cxn-1)) (length (conditional-part cxn-2)))
              (equalp (remove nil (mapcar #'(lambda (unit)
                                                       (second (find 'lex-class (comprehension-lock unit) :key #'first)))
                                                   (conditional-part cxn-1)))
                               (remove nil (mapcar #'(lambda (unit)
                                                       (second (find 'lex-class (comprehension-lock unit) :key #'first)))
                                                   (conditional-part cxn-2)))
                               )
              (equalp (remove nil (mapcar #'(lambda (unit)
                                                       (second (find 'phrase-type (comprehension-lock unit) :key #'first)))
                                                   (conditional-part cxn-1)))
                               (remove nil (mapcar #'(lambda (unit)
                                                       (second (find 'phrase-type (comprehension-lock unit) :key #'first)))
                                                   (conditional-part cxn-2)))
                               )
              (equalp (remove nil (mapcar #'(lambda (unit)
                                                       (second (find 'lemma (comprehension-lock unit) :key #'first)))
                                                   (conditional-part cxn-1)))
                               (remove nil (mapcar #'(lambda (unit)
                                                       (second (find 'lemma (comprehension-lock unit) :key #'first)))
                                                   (conditional-part cxn-2)))
                               )))))