(in-package :mwm)

;; make html of mwm-object
(defmethod make-html-for-entity-details ((object mwm-object) &key)
  (append
   (loop for attr in '(x-pos y-pos area wh-ratio R G B roughness nr-of-sides nr-of-corners)
         append `(((div :class "entity-detail")
                   ,(let ((value (funcall (symbol-function attr) object)))
                      (if (listp value)
                        (format nil "~a = (~{~,2f~^, ~})" attr value)
                        (format nil "~a = ~,2f" attr value))))))))

;; make html of object set
(defmethod make-html-for-entity-details ((set mwm-object-set) &key)
  `(((div :class "entity-detail")
     ,@(loop for object in (objects set)
             collect (make-html object :expand-initially t)))))

;; make html of mwm-category
(defmethod make-html-for-entity-details ((category min-max-category) &key)
  `(((div :class "entity-detail") ,(format nil "attribute: ~a" (attribute category)))
    ((div :class "entity-detail") ,(format nil "lower-bound: ~,2f" (lower-bound category)))
    ((div :class "entity-detail") ,(format nil "upper-bound: ~,2f" (upper-bound category)))))

(defmethod make-html-for-entity-details ((category prototype-category) &key)
  `(((div :class "entity-detail") ,(format nil "attribute: ~a" (attribute category)))
    ((div :class "entity-detail") ,(format nil "prototype: ~,2f" (prototype category)))
    ((div :class "entity-detail") ,(format nil "variance: ~,2f" (variance category)))))

(defmethod make-html-for-entity-details ((category prototype-min-max-category) &key)
  `(((div :class "entity-detail") ,(format nil "attribute: ~a" (attribute category)))
    ((div :class "entity-detail") ,(format nil "prototype: ~,2f" (prototype category)))
    ((div :class "entity-detail") ,(format nil "lower-bound: ~,2f" (lower-bound category)))
    ((div :class "entity-detail") ,(format nil "upper-bound: ~,2f" (upper-bound category)))))

;; make html of clevr object
(defmethod make-html-for-entity-details ((object clevr-object) &key)
  `(((div :class "entity-detail") ,(format nil "size: ~a" (size object)))
    ((div :class "entity-detail") ,(format nil "color: ~a" (color object)))
    ((div :class "entity-detail") ,(format nil "material: ~a" (material object)))
    ((div :class "entity-detail") ,(format nil "shape: ~a" (shape object)))
    ((div :class "entity-detail") ,(format nil "x-pos: ~a" (if (> (first (coordinates object)) 245.5)
                                                             "RIGHT" "LEFT")))
    ((div :class "entity-detail") ,(format nil "y-pos: ~a" (if (> (second (coordinates object)) 177)
                                                             "FRONT" "BACK")))))