(in-package :visual-dialog)

;; ------------------
;; RELATE primtive ;;
;; ------------------

(defprimitive relate ((target world-model)
                      (source world-model)
                      (segmented-scene world-model)
                      (scene pathname-entity)
                      (spatial-relation category))
  ;; first case; given source-object and spatial relation, compute the target set
  ((source segmented-scene scene spatial-relation => target)
   (let ((scene-object-id-list (loop for obj in (objects (object-set (first (set-items segmented-scene))))
                                   collect (id obj)))
         (source-object-id-list  (loop for obj in (objects (object-set (first (set-items source))))
                                    collect (id obj)))
         (scene-object-list (collect-objects-from-world-model segmented-scene)))
     (multiple-value-bind (bind-scores bind-values)
       (evaluate-neural-primitive
        "relate"
        (get-data ontology 'server-address)
        (get-data ontology 'cookie-jar)
        `(:target nil
          :source ,source-object-id-list
          :segmented-scene ,scene-object-id-list
          :scene ,(namestring (path scene))
          :spatial-relation ,(spatial-relation spatial-relation)))
     (loop for scores in bind-scores
           for values in bind-values
           do (let ((objects-with-attn
                     (loop for attn in (getf values 'target)
                           for score in (getf scores 'target)
                           for attn-id = (intern attn :visual-dialog)
                           for object = (copy-object (find attn-id scene-object-list :key #'id))
                           do (setf (scores (attention object)) score)
                           collect object)))
                (bind (target ;(getf scores 'target)
                       1.0
                       (make-instance 'world-model
                                      :set-items (list (make-instance 'turn
                                                                      :object-set (make-instance 'object-set :objects objects-with-attn)))))))))))
  
  :primitive-inventory *subsymbolic-primitives*)
