(in-package :crs-conventionality)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                     ;;
;; This file specifies all html visualisations for the crs conventionality experiments ;;
;;                                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod make-html-for-entity-details ((world crs-conventionality-world) &key)
  "Visualizing a crs conventionality world."
  `(((div :class "entity-detail") 
     ,@(loop for entity in (entities world)
             collect (make-html entity :expand-initially t)))))


(defmethod make-html-for-entity-details ((population crs-conventionality-population) &key)
  "Visualizing a crs conventionality population"
  `(((div :class "entity-detail") 
     ,@(loop for agent in (agents population)
             collect (make-html agent :expand-initially t)))))







#|(defmethod make-html ((agent crs-conventionality-agent)
                      &rest parameters
                      &key (expand/collapse-all-id (make-id 'agent))
                      (expand-initially nil))
  `((div :class "agent")
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
            ,(format nil "~(~a~)" (type-of agent)))))))|#