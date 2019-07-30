;;;; /html.lisp

(in-package :roos)

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
     (loop for key in '(:xpos :ypos :color)
           for value = (rest (assoc key (features obj)))
           collect `((div :class "entity-detail")
                     ,(if (listp value)
                        (format nil "~a = (~{~,2f~^, ~})" key value)
                        (format nil "~a = ~,2f" key value)))))))

;; + Object Set +

(defmethod make-html-for-entity-details ((set object-set) &key)
  `(((div :class "entity-detail") 
     ,@(loop for e in (entities set)
             collect (make-html e :expand-initially t)))))

;; + Category +

(defmethod make-html-for-entity-details ((cat category) &key)
  `(((div :class "entity-detail")
     ,(if (listp (prototype cat))
        (format nil "prototype = (~{~,2f~^, ~})" (prototype cat))
        (format nil "prototype = ~,2f" (prototype cat))))))

#|
(defmethod make-html-for-entity-details ((cat color-category) &key)
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
                :style ,(format nil "fill:rgb(~{~a~^, ~})" (prototype cat)))))))
     `(((div :class "entity-detail")
        ,(format nil "R = ~a" (first (prototype cat)))))
     `(((div :class "entity-detail")
        ,(format nil "G = ~a" (second (prototype cat)))))
     `(((div :class "entity-detail")
        ,(format nil "B = ~a" (third (prototype cat))))))))
|#