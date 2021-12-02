(in-package :muhai-cookingbot)

(defun draw-recipe (network &key path)
  "Draws a predicate network by linking its variables"
  (let ((path (or path
                  (make-file-name-with-time
                   (merge-pathnames wi::*graphviz-output-directory*
                                    (make-pathname :name "recipe" 
                                                   :type "pdf"))))))    
    (draw-predicate-network network :path path :open t :format "pdf")))


(defmethod make-html-for-entity-details ((e kitchen-entity) &key expand-initially)
  "Default HTML visualisation method for object of class kitchen-entity."
  (loop for slot in (closer-mop:class-slots (class-of e))
     for slot-name = (closer-mop:slot-definition-name slot)
        for slot-value = (slot-value e slot-name)
        if (and (or (symbolp slot-value) (stringp slot-value) (numberp slot-value)))
        collect
        (if (eq 'id slot-name)
          ""
          `((div :class "entity-detail")
            ,(format nil "~(~a~): ~(~a~)" slot-name slot-value)))
        else if (listp slot-value)
        collect
        `((div :class "entity-detail")
          ,@(loop for el in slot-value
                  collect (make-html el :expand-initially expand-initially)))
        else
        collect
        `((div :class "entity-detail")
          ,(format nil "~(~a~): " slot-name)
          ,(make-html slot-value :expand-initially expand-initially))))
