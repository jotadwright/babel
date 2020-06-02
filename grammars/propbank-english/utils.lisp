(in-package :propbank-english)

(defun comprehend-and-extract-frames (utterance &key (cxn-inventory *fcg-constructions*) (silent nil) (syntactic-analysis nil))
  (multiple-value-bind (solution cipn)
      (comprehend utterance :cxn-inventory cxn-inventory :silent silent :syntactic-analysis syntactic-analysis)
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
          
          when (> (length frame-elements) (length (remove-duplicates frame-elements :key #'fourth :test #'equalp)))
          do (push frame-var double-role-assignments)
          finally
          return
          (if double-role-assignments
            ;;some frames contain frame-elements that have identical slot fillers
            (and (push 'double-role-assignment (statuses node)) nil)
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
