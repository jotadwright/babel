(in-package :visual-dialog)

;; -----------------
;; QUERY primtive ;;
;; -----------------

(defmethod attribute-none ((attribute-category attribute-category)
                           ontology)
  (let ((none nil))
    (if (equal (attribute attribute-category) 'shape)
      (loop for attr in (get-data ontology 'shapes)
            do (if (equal (id attr) 'none)
                 (setf none attr)))
      )
    
    (if (equal (attribute attribute-category) 'size)
      (loop for attr in (get-data ontology 'sizes)
            do (if (equal (id attr) 'none)
                 (setf none attr)))
      )
    (if (equal (attribute attribute-category) 'color)
      (loop for attr in (get-data ontology 'colors)
            do (if (equal (id attr) 'none)
                 (setf none attr)))
      )
    
    (if (equal (attribute attribute-category) 'material)
      (loop for attr in (get-data ontology 'materials)
            do (if (equal (id attr) 'none)
                 (setf none attr))))
    (if (equal (attribute attribute-category) 'digit)
      (loop for attr in (get-data ontology 'digits)
            do (if (equal (id attr) 'none)
                 (setf none attr))))
    (if (equal (attribute attribute-category) 'bgcolor)
      (loop for attr in (get-data ontology 'bgcolors)
            do (if (equal (id attr) 'none)
                 (setf none attr))))
    (if (equal (attribute attribute-category) 'style)
      (loop for attr in (get-data ontology 'styles)
            do (if (equal (id attr) 'none)
                 (setf none attr))))
    none))


(defprimitive query ((target-category attribute)
                     (source-object-set world-model)
                     (scene pathname-entity)
                     (attribute attribute-category))
  ;; first case; given attribute and source-object, compute the target category
  ((source-object-set scene attribute => target-category)
   (let ((source-object-list (loop for obj in (collect-objects-from-world-model source-object-set)
                                    collect (id obj))))
     (if source-object-list
       (multiple-value-bind (bind-scores bind-values)
           (evaluate-neural-primitive
            "query"
            (get-data ontology 'server-address)
            (get-data ontology 'cookie-jar)
            `(:source ,source-object-list
              :attribute ,(let ((attr
                                 (if (eq (attribute attribute) 'digit)
                                   'number
                                   (attribute attribute))))
                            attr)
              :scene ,(namestring (path scene))
              :target nil))
         (loop for scores in bind-scores
               for values in bind-values
               do 
                 (bind (target-category
                        ;(getf scores 'target)
                        1.0
                        (if (not (equal (getf values 'target) "NONE"))
                          (find-entity-by-id
                           ontology
                           (intern (getf values 'target)
                                   :visual-dialog))
                          (attribute-none attribute ontology) )
                      ))))
       (bind (target-category 1.0 (attribute-none attribute ontology))))))
   
  ;; second case; given source-object and target-category, compute the attribute
  ((source-object-set scene target-category => attribute)
   (let ((source-object-list (loop for obj in (collect-objects-from-world-model source-object-set)
                                    collect (id obj))))
     (multiple-value-bind (bind-scores bind-values)
         (evaluate-neural-primitive
          "query"
          (get-data ontology 'server-address)
          (get-data ontology 'cookie-jar)
          `(:source ,source-object-list
            :target ,(category-value target-category)
            :scene ,(namestring (path scene))
            :attribute nil)))
     (loop for scores in bind-scores
           for values in bind-values
           do (bind (attribute
                     (getf scores 'attribute)
                     (find (intern (getf values 'attribute)
                                   :visual-dialog)
                           (get-data ontology 'attributes)
                           :key #'id))))))

  ;; third case; given source-object, compute pairs of attribute and target-category
  ((source-object-set scene => attribute target-category)
   (let ((source-object-list (loop for obj in (collect-objects-from-world-model source-object-set)
                                    collect (id obj))))
   (multiple-value-bind (bind-scores bind-values)
       (evaluate-neural-primitive
        "query"
        (get-data ontology 'server-address)
        (get-data ontology 'cookie-jar)
        `(:source ,source-object-list
          :target nil
          :scene ,(namestring (path scene))
          :attribute nil))
     (loop for scores in bind-scores
           for values in bind-values
           do (bind (attribute
                     (getf scores 'attribute)
                     (find (intern (getf values 'attribute)
                                   :visual-dialog)
                           (get-data ontology 'attributes)
                           :key #'id))
                    (target-category
                     (getf scores 'target)
                     (find-entity-by-id
                      ontology
                      (intern (getf values 'target)
                              :visual-dialog))))))))
  
  ;; fourth case; if given source-object, attribute and target-category, check
  ;; for consistency
  ((source-object-set attribute target-category scene =>)
   (let* ((source-object-list (loop for obj in (collect-objects-from-world-model source-object-set)
                                    collect (id obj)))
          (consistentp
           (evaluate-neural-primitive
            "query"
            (get-data ontology 'server-address)
            (get-data ontology 'cookie-jar)
            `(:source ,source-object-list
              :target ,(category-value target-category)
              :scene ,(namestring (path scene))
              :attribute ,(attribute attribute))))))
   consistentp)
  :primitive-inventory *subsymbolic-primitives*)
  

