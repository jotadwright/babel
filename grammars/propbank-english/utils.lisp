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
