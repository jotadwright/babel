(in-package :visual-dialog)

;; ----------------------------------------------------------------;;
;; This file contains functions to display the objects of the      ;;
;; visual dialog world in the web interface                        ;;
;; ----------------------------------------------------------------;;

;; DRAWING OBJECTS
;; maybe we need to find a way to visualize the spatial relationships as well
(defmethod make-html-for-entity-details ((object object) &key)
  `(((div :class "entity-detail") ,(format nil "Attributes: ~a" (attributes object)))
    ((div :class "entity-detail") ,(make-html (attention object) ))
    ;((div :class "entity-detail") ,(format nil "Topic: ~a" (mentioned-object object)))
    ))

#|(defmethod make-html-for-entity-details ((object relation-set) &key)
  `(((div :class "entity-detail") ,(format nil "Leftmost: ~a" (leftmost object)))
    ((div :class "entity-detail") ,(format nil "Rightmost: ~a" (rightmost object)))
    ((div :class "entity-detail") ,(format nil "Most in front: ~a" (most-in-front object)))
    ((div :class "entity-detail") ,(format nil "Most in back: ~a" (most-in-back object)))
    ((div :class "entity-detail") ,(format nil "Immediate right: ~a" (immediate-right object)))
    ((div :class "entity-detail") ,(format nil "Immediate front: ~a" (immediate-front object)))
    ((div :class "entity-detail") ,(format nil "Middle: ~a" (middle object)))))|#

(defmethod make-html-for-entity-details ((set object-set) &key)
  `(((div :class "entity-detail") 
     ,@(loop for object in (objects set)
             collect (make-html object :expand-initially t)))
    ;((div :class "entity-detail") ,(make-html (scene-configuration set) :expand-initially t))
    ))

(defmethod make-html-for-entity-details ((hist world-model) &key)
  `(((div :class "entity-detail")
     ,@(loop for item in (reverse (set-items hist))
             collect (make-html item :expand-initially t)))))

(defmethod make-html-for-entity-details ((hitem turn) &key)
  `(((div :class "entity-detail") ,(format nil "Timestep: ~a" (timestamp hitem)))
    ((div :class "entity-detail") ,(format nil "Utterance type: ~a" (question-type hitem)))
    ((div :class "entity-detail") ,(format nil "Question: ~a" (question hitem)))
    ((div :class "entity-detail") ,(format nil "Answer: ~a" (answer hitem)))
    ((div :class "entity-detail") ,(format nil "Topic: ~a" (topic-list hitem)))
    ((div :class "entity-detail") ,(make-html (object-set hitem) :expand-initially t))))



;; DRAWING CATEGORIES
(defmethod make-html-for-entity-details ((category category) &key)
  `(((div :class "entity-detail") ,(format nil "~a" (id category)))))

(defmethod make-html-for-entity-details ((attr-category attribute-category) &key)
  `(((div :class "entity-detail") ,(format nil "~a" (attribute attr-category)))))



(defmethod make-html-for-entity-details ((attn attention) &key)
  (when (img-path attn)
    (copy-file (img-path attn) (pathname "~/Sites/"))

    (if (null cl-user:*localhost-user-dir*)
      (warn "Set your *localhost-user-dir* in init-babel")
      `(((img :src ,(string-append cl-user:*localhost-user-dir*
                                   (format nil "~a.png" (id attn)))
              :width "480"
              :height "320"

              ))))))
