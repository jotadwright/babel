(in-package :muhai-cookingbot)

(defun draw-recipe (network &key path)
  "Draws a predicate network by linking its variables"
  (let ((path (or path
                  (make-file-name-with-time
                   (merge-pathnames wi::*graphviz-output-directory*
                                    (make-pathname :name "recipe" 
                                                   :type "pdf"))))))    
    (draw-predicate-network network :path path :open t :format "pdf")))

(defmethod collapsed-entity-html ((e list) element-id)
  "html for the collapsed version of an entity"
  `((div :class "entity-box")
    ((div :class "entity-title")
     ((a ,@(make-expand/collapse-link-parameters 
            element-id t "expand entity")
         :name ,(mkstr (car e)))
      ,(format nil "~(~a~)" element-id)))))

(defmethod expanded-entity-html ((e list) element-id parameters)
  "html for the expanded version of an entity"
  (lambda ()
    `((div :class "entity-box")
      ((div :class "entity-title")
       ((a ,@(make-expand/collapse-link-parameters
              element-id nil "collapse entity")
           :name ,(mkstr (car e)))
        ,(format nil "~(~a~)" element-id)))
      ((table :class "entity" :cellpadding "0" :cellspacing "0") 
       ((tr)
        ((td :class "entity-details")
         ,(format nil "~(~a~)" e)))))))
        ;; ,(make-html-for-alist (cdr e) (make-id 'entity) nil t parameters)))))))




(defun make-html-for-alist (alist element-id expand/collapse-all-id expand-initially parameters)
  (make-expandable/collapsable-element 
   element-id expand/collapse-all-id
   ;; collapsed version
   (collapsed-entity-html alist element-id)
   ;; expanded version
   (expanded-entity-html alist element-id parameters)
   :expand-initially expand-initially))



(defmethod make-html-for-entity-details ((e kitchen-entity) &key expand-initially)
  "Default HTML visualisation method for object of class kitchen-entity."
  (loop for slot in (closer-mop:class-slots (class-of e))
        for slot-name = (closer-mop:slot-definition-name slot)
        for slot-value = (slot-value e slot-name)
        if (or (symbolp slot-value)
               (stringp slot-value)
               (numberp slot-value))
          collect (if (eq 'id slot-name)
                      ""
                      `((div :class "entity-detail")
                        ,(format nil "~(~a~): ~(~a~)" slot-name slot-value)))

        ;; TODO PRETTY PRINT HTML
        else if (eq slot-name 'sim-arguments)
               collect `((div :class "entity-detail")
                         ,(format nil "~(~a~)" slot-name)
                         ,(make-html-for-alist slot-value (make-id 'entity) nil expand-initially nil))
                  
        else if (listp slot-value)
               collect  `((div :class "entity-detail")
                          ,@(loop for el in slot-value
                                  collect (make-html el :expand-initially expand-initially)))
        
        else
          collect `((div :class "entity-detail")
                    ,(format nil "~(~a~): " slot-name)
                    ,(make-html slot-value :expand-initially expand-initially))))
