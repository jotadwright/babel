(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               ;;
;; HTML en CSS for web-interface ;;
;;                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Fixes ;;
;;;;;;;;;;;

(defmethod make-html ((fix holophrastic-fix)
                      &key expand-initially)
  "Visualisation for holophrastic fixes."
  (let ((element-id (make-id 'fix))
        (link-title `((tt) ,(format nil "~(~a~)" (type-of fix))))
        (expand-collapse-all-id (make-id)))
    `((div :class "fix" :style ,(format nil "border:1px solid ~a;" (assqv 'fix meta-layer-learning::*web-colors*)))
      ,(make-expandable/collapsable-element
 	element-id expand-collapse-all-id
        `((div :class "fix-title"
               :style ,(format nil "background-color:~a;" (assqv 'fix meta-layer-learning::*web-colors*)))
 	  ((a ,@(make-expand/collapse-link-parameters element-id t))
           ,link-title))
        `((span)
          ((div :class "fix-title"
                :style ,(format nil "background-color:~a;" (assqv 'fix meta-layer-learning::*web-colors*)))
           ((a ,@(make-expand/collapse-link-parameters element-id nil))
            ,link-title))
          ((table :class "two-col")
           ((tr) ((td :class "data-field") "issued-by:")
            ((td :class "data")
             ,(make-html (issued-by fix))))
           ((tr) ((td :class "data-field") "problem:")
            ((td :class "data")
             ((td :class "data-field")
              ,(make-html (problem fix)))))
           ((tr) ((td :class "data-field") "fix-constructions:")
            ((td :class "data")
             ((td :class "data-field")
              ,@(loop for cxn in (fix-constructions fix)
                      collect (make-html cxn
                                         :cxn-inventory (cxn-inventory cxn)
                                         :expand-initially expand-initially)))))))
        :expand-initially expand-initially))))


(defmethod make-html ((fix anti-unification-fix)
                      &key expand-initially)
  "Visualisation for anti-unification fixes."
  (let ((element-id (make-id 'fix))
        (link-title `((tt) ,(format nil "~(~a~)" (type-of fix))))
        (expand-collapse-all-id (make-id)))
    `((div :class "fix" :style ,(format nil "border:1px solid ~a;" (assqv 'fix meta-layer-learning::*web-colors*)))
      ,(make-expandable/collapsable-element
 	element-id expand-collapse-all-id
        `((div :class "fix-title"
               :style ,(format nil "background-color:~a;" (assqv 'fix meta-layer-learning::*web-colors*)))
 	  ((a ,@(make-expand/collapse-link-parameters element-id t))
           ,link-title))
        `((span)
          ((div :class "fix-title"
                :style ,(format nil "background-color:~a;" (assqv 'fix meta-layer-learning::*web-colors*)))
           ((a ,@(make-expand/collapse-link-parameters element-id nil))
            ,link-title))
          ((table :class "two-col")
           ((tr) ((td :class "data-field") "issued-by:")
            ((td :class "data")
             ,(make-html (issued-by fix))))
           ((tr) ((td :class "data-field") "problem:")
            ((td :class "data")
             ((td :class "data-field")
              ,(make-html (problem fix)))))
           ((tr) ((td :class "data-field") "base-cxns:")
            ((td :class "data")
             ((td :class "data-field")
              ,@(loop for cxn in (base-cxns fix)
                      collect (make-html cxn
                                         :cxn-inventory (cxn-inventory cxn)
                                         :expand-initially expand-initially)))))
           ((tr) ((td :class "data-field") "fix-constructions:")
            ((td :class "data")
             ((td :class "data-field")
              ,@(loop for cxn in (fix-constructions fix)
                      collect (make-html cxn
                                         :cxn-inventory (cxn-inventory cxn)
                                         :expand-initially expand-initially)))))
           ((tr) ((td :class "data-field") "fix-categories:")
            ((td :class "data")
             ((td :class "data-field")
              ,@(loop for cat in (fix-categories fix)
                      collect (make-html cat)))))
           ((tr) ((td :class "data-field") "fix-links:")
            ((td :class "data")
             ((td :class "data-field")
              ,@(loop for link in (fix-categorial-links fix)
                      collect (make-html link)))))))
        :expand-initially expand-initially))))


(defmethod make-html ((fix categorial-link-fix)
                      &key expand-initially)
  "Visualisation for categorial link fixes."
  (let ((element-id (make-id 'fix))
        (link-title `((tt) ,(format nil "~(~a~)" (type-of fix))))
        (expand-collapse-all-id (make-id)))
    `((div :class "fix" :style ,(format nil "border:1px solid ~a;" (assqv 'fix meta-layer-learning::*web-colors*)))
      ,(make-expandable/collapsable-element
 	element-id expand-collapse-all-id
        `((div :class "fix-title"
               :style ,(format nil "background-color:~a;" (assqv 'fix meta-layer-learning::*web-colors*)))
 	  ((a ,@(make-expand/collapse-link-parameters element-id t))
           ,link-title))
        `((span)
          ((div :class "fix-title"
                :style ,(format nil "background-color:~a;" (assqv 'fix meta-layer-learning::*web-colors*)))
           ((a ,@(make-expand/collapse-link-parameters element-id nil))
            ,link-title))
          ((table :class "two-col")
           ((tr) ((td :class "data-field") "issued-by:")
            ((td :class "data")
             ,(make-html (issued-by fix))))
           ((tr) ((td :class "data-field") "problem:")
            ((td :class "data")
             ((td :class "data-field")
              ,(make-html (problem fix)))))
           ((tr) ((td :class "data-field") "fix-links:")
            ((td :class "data")
             ((td :class "data-field")
              ,@(loop for link in (fix-categorial-links fix)
                      collect (make-html link)))))))
        :expand-initially expand-initially))))


(defmethod make-html-construction-title ((construction fcg-learn-cxn))
  "Print title of construction and node with name and entrenchment score."
 `((span) 
   ,(format nil "~(~a~) (~$)" (name construction) (attr-val construction :entrenchment-score))))


