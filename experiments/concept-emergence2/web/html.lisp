(in-package :cle)

;; -----------------------
;; + Make HTML functions +
;; -----------------------

;; make html of entity
(defmethod make-html-for-entity-details ((entity entity) &key agent topic world)
  (let ((title-font (if (equal topic (id entity)) "font-weight:bold;" ""))
        (features (sort (loop for feature-name being the hash-keys of (features object)
                                  using (hash-value value)
                                collect (cons feature-name value))
                          (lambda (x y) (string< (symbol-name x) (symbol-name y)))
                          :key #'car)))
    (append
     ;; symbolic attributes
     (loop for attr being the hash-keys of (description object) 
           using (hash-value val)
           if (is-channel-available world (current-view agent) attr (features object))
             append `(((div :class "entity-detail" :style ,(format nil "~a" title-font))
                       ,(format nil "~a = ~,2f" attr val))))
     ;; continuous features
     `(((hr :style ,(format nil "margin: 0px;"))))
     `(((hr :style ,(format nil "margin: 0px;"))))
     ;; add border to the first key, val pair
     (let* ((attr (caar features))
            (val (cdar features)))
       `(((div :class "entity-detail" :style ,(format nil "border-top: 0px dashed #563; ~a" title-font))
          ,(format nil "~a = ~,2f" attr val))))
     ;; then draw the rest
     (loop for (attr . val) in (rest features)
           append `(((div :class "entity-detail" :style ,(format nil "~a" title-font))
                     ,(format nil "~a = ~,2f" attr val)))))))


;; make html of object set
(defmethod make-html-for-entity-details ((set cle-scene) &key agent topic world)
  `(((div :class "entity-detail")
     ,@(loop for entity in (entities set)
             collect (make-html entity :agent agent :topic topic :world world :expand-initially t)))))

;; make-html of cxn
(defmethod make-html ((cxn cxn) &key)
  `((div)
    ,(s-dot->svg
      (cxn->s-dot cxn))))

;; make html of cle-category
(defmethod make-html-for-entity-details ((wd concept-representations::weighted-distribution) &key)
  `(((div :class "entity-detail")
     ,(format nil "attribute: ~a" (feature-name wd)))
    ((div :class "entity-detail")
     ,(format nil "mean: ~,2f" (mean (distribution wd))))
    ((div :class "entity-detail")
     ,(format nil "variance: ~,2f" (st-dev (distribution wd))))))


;; -------------------------------------
;; Drawing object entities with colors +
;; -------------------------------------

(defun get-hex-value-of-object-color (entity)
  (case (gethash :color (description entity))
    (gray   "#575757")
    (red    "#AD2223")
    (blue   "#2C4CD7")
    (green  "#1D6914")
    (brown  "#7E4819")
    (purple "#8227C0")
    (cyan   "#28D0D0")
    (yellow "#C1B421")))

(defmethod make-html ((e entity)
                      &rest parameters
                      &key (topic nil)
                      (agent nil)
                      (world nil)
                      (expand/collapse-all-id (make-id 'entity))
                      (expand-initially nil))
  `((div :class "entity")
    ,(let ((element-id (make-id (id e))))
       (make-expandable/collapsable-element 
        element-id expand/collapse-all-id
        ;; collapsed version
        (collapsed-entity-html e agent topic world element-id)
        ;; expanded version
        (expanded-entity-html e agent topic world element-id parameters)
        :expand-initially expand-initially))
    ((table :class "entity")
     ((tr) ((td :class "entity-type") 
            ,(format nil "~(~a~)" (type-of e)))))))

(defmethod collapsed-entity-html ((e entity) agent (topic symbol) world element-id)
  "html for the collapsed version of an entity"
  (let ((border-thickness (if (equal topic (id e))
                            "3px"
                            "1px"))
        (border-color (if (equal topic (id e))
                        "#878484"
                        "#000000"))
        (title-color (if (equal topic (id e))
                       (get-hex-value-of-object-color e)
                       (if (equal 'entity (type-of e)) (get-hex-value-of-object-color e) "#562"))))
  `((div :class "entity-box")
    ((div :class "entity-title" :style ,(format nil "background-color:~a; ~a" border-color title-color))
     ((a ,@(make-expand/collapse-link-parameters 
            element-id t "expand entity")
         :name ,(mkstr (id e)))
      ,(format nil (if (equal topic (id e))
                     "topic: ~(~a~)"
                     "~(~a~)")  (id e)))))))

(defmethod expanded-entity-html ((e entity) agent (topic symbol) world element-id parameters)
  "html for the expanded version of an entity"
  (let ((border-thickness (if (equal topic (id e))
                            "3px"
                            "1px"))
        (border-color (if (equal topic (id e))
                        "#878484"
                        "#000000"))
        (title-color (if (equal topic (id e))
                       (get-hex-value-of-object-color e)
                       (if (equal 'entity (type-of e)) (get-hex-value-of-object-color e) "#562")))
        (title-font (if (equal topic (id e))
                      "font-weight:bold;"
                      ""
                      )))
    (lambda ()
      `((div :class "entity-box" :style ,(format nil "border:~a solid ~a; display:inline-block; box-shadow: 4px 2px 12px 1px rgb(0 0 0 / 10%);" border-thickness border-color ))
        ((div :class "entity-title" :style ,(format nil "background-color:~a; ~a" title-color title-font))
         ((a ,@(make-expand/collapse-link-parameters
                element-id nil "collapse entity")
             :name ,(mkstr (id e)))
          ,(format nil (if (equal topic (id e))
                         "Topic: ~(~a~)"
                         "~(~a~)")  (id e))))
        ((table :class "entity" :cellpadding "0" :cellspacing "0") 
         ((tr)
          ((td :class "entity-details")
           ,@(apply 'make-html-for-entity-details e :agent agent :world world parameters))))))))
