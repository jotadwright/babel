(in-package :mwm)

;; make html of object
(defmethod make-html-for-entity-details ((object mwm-object) &key)
  `(((div :class "entity-detail")
     ,(format nil "~{~a~^, ~}" (description object)))))

;; make html of object set
(defmethod make-html-for-entity-details ((set mwm-object-set) &key)
  `(((div :class "entity-detail")
     ,@(loop for object in (entities set)
             collect (make-html object :expand-initially t)))))

;; make html of channel
(defmethod make-html-for-entity-details ((channel channel) &key)
  `(((div :class "entity-detail")
     ,(format nil "~a" (value channel)))))

;; make html of mwm-lex
(defmethod make-html-for-entity-details ((lex mwm-lex) &key)
  (append
   `(((div :class "entity-detail")
      ,(format nil "~a" (form lex))))
   `(((div :class "entity-detail")
      ,(format nil "~a" (meaning lex))))))