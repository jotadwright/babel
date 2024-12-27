(in-package :crs-conventionality)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                     ;;
;; This file specifies all html visualisations for the crs conventionality experiments ;;
;;                                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; entity sets ;;
;;;;;;;;;;;;;;;;;

(defmethod make-html-for-entity-details ((set crs-conventionality-entity-set) &key)
  "Visualizing a crs conventionality world."
  `(((div :class "entity-detail") 
     ,@(loop for entity in (entities set)
             collect (make-html entity :expand-initially t)))))

(defmethod make-html-for-entity-details ((entity crs-conventionality-entity) &key &allow-other-keys)
  (append
   `(((div :class "entity-detail") 
      ,(format nil "ID: ~(~a~)" (id entity))))
   (when (world entity)
     `(((div :class "entity-detail") 
        ,(format nil "WORLD: ~(~a~)" (id (world entity))))))))

;; agents & population ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod make-html-for-entity-details ((population crs-conventionality-population) &key)
  "Visualizing a crs conventionality population"
  `(((div :class "entity-detail") 
     ,@(loop for agent in (agents population)
             collect (make-html agent)))))

(defmethod make-html-for-entity-details ((agent crs-conventionality-agent) &key &allow-other-keys)
  `(((div :class "entity-detail") 
     ,(format nil "discourse role: ~a"  (discourse-role agent)))))

(defmethod collapsed-entity-html ((agent crs-conventionality-agent) element-id)
  "html for the collapsed version of an agent"
  `((div :class "entity-box")
    ((div :class "entity-title")
     ((a ,@(make-expand/collapse-link-parameters 
            element-id t "expand entity")
         :name ,(mkstr (id agent)))
      ,(format nil "~(~a~)" (id agent))))))

(defmethod expanded-entity-html ((agent crs-conventionality-agent) element-id parameters)
  "html for the expanded version of an entity"
  (lambda ()
    `((div :class "entity-box")
      ((div :class "entity-title")
       ((a ,@(make-expand/collapse-link-parameters
              element-id nil "collapse agent")
           :name ,(mkstr (id agent)))
        ,(format nil "~(~a~)" (id agent))))
      ((table :class "agent" :cellpadding "0" :cellspacing "0") 
       ((tr)
        ((td :class "entity-details")
         ,@(apply 'make-html-for-entity-details agent parameters)))))))


(defmethod make-html ((agent crs-conventionality-agent)
                      &rest parameters
                      &key (expand/collapse-all-id (make-id 'agent))
                      (expand-initially nil))
  `((div :class "entity")
    ,(let ((element-id (make-id (id agent))))
       (make-expandable/collapsable-element 
        element-id expand/collapse-all-id
        ;; collapsed version
        (collapsed-entity-html agent element-id)
        ;; expanded version
        (expanded-entity-html agent element-id parameters)
        :expand-initially expand-initially))
    ((table :class "agent")
     ((tr) ((td :class "entity-type") 
            ,(format nil "~(~a~)" (type-of agent)))))))



;(add-element (make-html (world *naming-game-canonical*)))
;(add-element (make-html (population *naming-game-canonical*)))
