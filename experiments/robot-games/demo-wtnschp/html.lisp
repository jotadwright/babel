;;;; /html.lisp

(in-package :demo-wtnschp)

;; + Sensory Object +

(define-css 'color "div.color { display:inline-block;position:relative;overflow:hidden; }")

(defmethod make-html-for-entity-details ((obj sensory-object) &key)
  (let ((width 120)
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
        ,(format nil "rgb = (~{~,2f~^, ~})" (mapcar #'round (rgbcolor obj))))))))

;; + Object Set +

(defmethod make-html-for-entity-details ((set object-set) &key)
  `(((div :class "entity-detail") 
     ,@(loop for e in (entities set)
             collect (make-html e :expand-initially t)))))

;; + Category +

(defmethod make-html-for-entity-details ((color-cat color-category) &key)
  `(((div :class "entity-detail")
     ,(format nil "prototype = (~{~,2f~^, ~})" (prototype color-cat)))))