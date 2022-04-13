(in-package :visual-dialog)

;; -----------------------
;; SEGMENT-SCENE primitive ;;
;; -----------------------

(defprimitive segment-scene ((segmented-scene world-model)
                             (scene-pathname pathname-entity))
  ;; first case; given scene-pathname compute segmented-scene
  ;; if computed scene is already calculated, find it back in ontology, so no problems with ids changing over turns in dialog
  ((scene-pathname => segmented-scene)
   ;;do stuff --> call to server etc
   (multiple-value-bind (bind-scores bind-values)
       (evaluate-neural-primitive
        "segment-scene"
        (get-data ontology 'server-address)
        (get-data ontology 'cookie-jar)
        `(:segmented-scene nil
          :scene ,(namestring (path scene-pathname))))
     (loop for scores in bind-scores
           for values in bind-values
           do (let ((objects-with-attn
                     (loop for attn in (getf values 'segmented-scene)
                           for attention = (make-instance 'attention :id (intern attn :visual-dialog))
                           do (when (not (get-data ontology 'silent))
                                (request-attn (get-data ontology 'server-address) (get-data ontology 'cookie-jar) attention))
                           collect (make-instance 'object
                                                  :id (intern attn :visual-dialog)
                                                  :attention attention))))
                (bind (segmented-scene (getf scores 'segmented-scene)
                                       (make-instance 'world-model
                                                      :id 'context
                                                      :path scene-pathname
                                                      :set-items
                                                      (list
                                                       (make-instance 'turn
                                                                      :timestamp 'permanent
                                                                      :object-set
                                                                      (make-instance 'object-set
                                                                                     :objects objects-with-attn))))))))))
  
  ;; second case; given segmented-scene compute scene-pathname
  ((segmented-scene => scene-pathname)
   )
  
  ;; third case; check consistency
  ((segmented-scene scene-pathname =>)
   )
  :primitive-inventory *subsymbolic-primitives*)

           