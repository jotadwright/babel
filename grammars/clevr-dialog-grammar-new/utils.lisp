(in-package :clevr-dialog-grammar)

(defmethod de-render ((utterance string)
                      (mode (eql :de-render-scene-and-memory))
                      &key scene memory
                      &allow-other-keys)
  "De-renders an utterance and puts image and memory in root."
  (let* ((root-unit `(root
                      ,@(cdr (get-root (left-pole-structure (de-render utterance :de-render-string-meets))))
                      (subunits (scene-unit memory-unit))))
         (scene-unit `(scene-unit
                       (scene ,scene)))
         (memory-unit `(memory-unit
                        (memory ,memory))))
    (make-instance 'coupled-feature-structure 
		   :left-pole `(,root-unit
                                ,scene-unit
                                ,memory-unit)
		   :right-pole '((root)))))

(defmethod understand ((utterance string)
                       &key 
                       (cxn-inventory *fcg-constructions*)
                       (scene '?scene)
                       (memory '?memory)
                        silent)
  "Comprehends an utternace given an image and memory."
  (let* ((initial-cfs (de-render utterance (get-configuration cxn-inventory :de-render-mode)
                                 :scene scene
                                 :memory memory)))
    (unless silent (notify parse-started (listify utterance) initial-cfs))
    (multiple-value-bind
        (solution cip)
        (fcg-apply (processing-cxn-inventory cxn-inventory) initial-cfs '<- :notify (not silent))
      (let ((meaning 
             (and solution
                  (extract-meanings
                   (left-pole-structure (car-resulting-cfs (cipn-car solution)))))))
        (unless silent (notify parse-finished meaning (processing-cxn-inventory cxn-inventory)))
        (values meaning solution cip)))))

(defmethod understand-until-solution ((utterance string)
                                      &key 
                                      (cxn-inventory *fcg-constructions*)
                                      (scene '?scene)
                                      (memory '?memory)
                                      silent)
  "Comprehends an utterance given an image and memory."
  (let* ((initial-cfs (de-render utterance (get-configuration cxn-inventory :de-render-mode)
                                 :scene scene
                                 :memory memory)))
    (unless silent (notify parse-started (listify utterance) initial-cfs))
    (let* ((sols (loop repeat 100
                       for (solution cip) = (multiple-value-list (fcg-apply (processing-cxn-inventory cxn-inventory) initial-cfs '<- :notify (not silent)))
                      until (find 'succeeded (fcg:statuses solution))
                      return (list solution cip)))
          (solution (first sols))
          (cip (second sols)))
      (let ((meaning 
             (and solution
                  (extract-meanings
                   (left-pole-structure (car-resulting-cfs (cipn-car solution)))))))
        (unless silent (notify parse-finished meaning (processing-cxn-inventory cxn-inventory)))
        (values meaning solution cip)))))


(defmethod apply-heuristic ((node cip-node) (mode (eql :cxn-score)))
  "Returns the cxn score"
  (cdr (assoc :score (attributes (get-original-cxn (car-applied-cxn (cipn-car node)))))))
