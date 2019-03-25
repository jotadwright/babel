(in-package :origins-of-syntax)

(defmethod make-html-for-entity-details ((object syntax-object) &key discriminatory-features)
  "Visualizing a syntax-object."
  (let* ((shape (cdr (assoc :shape (attributes object))))
         (shape-symbol (ecase shape
                        (rectangle "&#x25ad;")
                        (triangle "&#x25b3;")
                        (circle "&#x25cb;")
                        (square "&#x25a1;")))
         (size (cdr (assoc :size (attributes object))))
         (size-number (ecase size
                        (tiny 0)
                        (small 20)
                        (large 40)
                        (huge 60)))
         (color (cdr (assoc :color (attributes object)))))
  `(((div :class "entity-detail" :style "text-align:center")
     ((span :style ,(format nil "font-size:~,2fpx;color:~,2f;" (+ 20 size-number) color))
      ,shape-symbol))
    ,@(loop for attribute in (reverse (attributes object))
            collect
            `((div :class "entity-detail" :style ,(format nil "font-weight:~a;"
                                                 (if (find (car attribute) discriminatory-features)
                                                   "bold" "normal"))) ,(format nil "~a: ~,2f" (car attribute) (cdr attribute)))))))

(defmethod make-html-for-entity-details ((set set-of-syntax-objects) &key discriminatory-features)
  "Visualizing a syntax-world."
  `(((div :class "entity-detail") 
     ,@(loop for object in (objects set)
             for n from 0 to (- (length (objects set)) 1)
             collect (make-html object :expand-initially t :discriminatory-features (nth n discriminatory-features))))))



