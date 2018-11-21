;;;; /html.lisp

(in-package :demo-wtnschp)

;; + Sensory Object +

(define-css 'color "div.color { display:inline-block;position:relative;overflow:hidden; }")

(defmethod make-html-for-entity-details ((obj sensory-object) &key)
  (let ((width 170)
        (height 120))
    (append
     `(((div :class "color")
        ((svg :xmlns "http://www.w3.org/2000/svg"
              :width ,(format nil "~,2f" width)
              :height ,(format nil "~,2f" height))
         ((rect :x "0" :y "0"
                :width ,(mkstr width)
                :height ,(mkstr height)
                :style ,(format nil "fill:rgb(~{~a~^, ~})" (mapcar #'round (rgbcolor obj))))))))
     `(((div :class "entity-detail")
        ,(format nil "rgb = (~{~,2f~^, ~})" (rgbcolor obj)))))))

;; + Object Set +

(defmethod make-html-for-entity-details ((set object-set) &key)
  `(((div :class "entity-detail") 
     ,@(loop for e in (entities set)
             collect (make-html e :expand-initially t)))))

;; + Category +

(defmethod make-html-for-entity-details ((color-cat color-category) &key)
  (let ((width 200)
        (height 120))
    (append
     `(((div :class "color")
        ((svg :xmlns "http://www.w3.org/2000/svg"
              :width ,(format nil "~,2f" width)
              :height ,(format nil "~,2f" height))
         ((rect :x "0" :y "0"
                :width ,(mkstr width)
                :height ,(mkstr height)
                :style ,(format nil "fill:rgb(~{~a~^, ~})" (mapcar #'round (prototype color-cat))))))))
     `(((div :class "entity-detail")
        ,(format nil "prototype = (~{~,2f~^, ~})" (prototype color-cat)))))))

;; + Category Set +

(defmethod make-html-for-entity-details ((set category-set) &key)
  `(((div :class "entity-detail")
     ,@(loop for e in (entities set)
             collect (make-html e :expand-initially t)))))

;; + Lex Item +

(defmethod make-html ((e color-lex-item)
                      &rest parameters
                      &key (expand/collapse-all-id (make-id 'entity))
                      (expand-initially nil))
  `((div :class "entity")
    ,(let ((element-id (make-id (id e))))
          (make-expandable/collapsable-element 
           element-id expand/collapse-all-id
           `((div :class "entity-box")
             ((div :class "entity-title")
              ((a ,@(make-expand/collapse-link-parameters 
                     element-id t "expand entity")
                  :name ,(format nil "prototype = (~{~,2f~^, ~})" (rgbcolor e)))
               ,(format nil "prototype = (~{~,2f~^, ~})" (rgbcolor e)))))
           (lambda ()
             `((div :class "entity-box")
               ((div :class "entity-title")
                ((a ,@(make-expand/collapse-link-parameters element-id nil 
                                                            "collapse entity")
                    :name ,(format nil "prototype = (~{~,2f~^, ~})" (rgbcolor e)))
                 ,(format nil "prototype = (~{~,2f~^, ~})" (rgbcolor e))))
               ((table :class "entity" :cellpadding "0" :cellspacing "0") 
                ((tr)
                 ((td :class "entity-details")
                  ,@(apply 'make-html-for-entity-details e parameters))))))
           :expand-initially expand-initially))
    ((table :class "entity")
     ((tr) ((td :class "entity-type") 
            ,(format nil "~(~a~)" (type-of e)))))))

(defmethod make-html-for-entity-details ((item color-lex-item) &key)
  (let ((width 200)
        (height 120))
    (append
     `(((div :class "color")
        ((svg :xmlns "http://www.w3.org/2000/svg"
              :width ,(format nil "~,2f" width)
              :height ,(format nil "~,2f" height))
         ((rect :x "0" :y "0"
                :width ,(mkstr width)
                :height ,(mkstr height)
                :style ,(format nil "fill:rgb(~{~a~^, ~})" (mapcar #'round (rgbcolor item))))))))
     (loop for word in (words item)
           append `(((div :class "entity-detail")      
                     ,(format nil "~a (~,1f)" (car word) (cdr word))))))))

;; + Lex +

(defmethod make-html ((e color-lex)
                      &rest parameters
                      &key (expand/collapse-all-id (make-id 'entity))
                      (expand-initially nil))
  `((div :class "entity")
    ,(let ((element-id (make-id (id e))))
          (make-expandable/collapsable-element 
           element-id expand/collapse-all-id
           `((div :class "entity-box")
             ((div :class "entity-title")
              ((a ,@(make-expand/collapse-link-parameters 
                     element-id t "expand entity")
                  :name "lexicon")
               "lexicon")))
           (lambda ()
             `((div :class "entity-box")
               ((div :class "entity-title")
                ((a ,@(make-expand/collapse-link-parameters element-id nil 
                                                            "collapse entity")
                    :name "lexicon")
                 "lexicon"))
               ((table :class "entity" :cellpadding "0" :cellspacing "0") 
                ((tr)
                 ((td :class "entity-details")
                  ,@(apply 'make-html-for-entity-details e parameters))))))
           :expand-initially expand-initially))
    ((table :class "entity")
     ((tr) ((td :class "entity-type") 
            ,(format nil "~(~a~)" (type-of e)))))))

(defmethod make-html-for-entity-details ((set color-lex) &key)
  `(((div :class "entity-detail")
     ,@(loop for e in (entities set)
             collect (make-html e :expand-initially t)))))