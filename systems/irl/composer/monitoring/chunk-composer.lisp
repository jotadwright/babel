(in-package :irl)

;; ------------------------------
;; + chunk-composer - make-html +
;; ------------------------------

(defmethod collapsed-chunk-composer-html ((composer chunk-composer)
                                          element-id)
  "html for the collapsed version of a primitive inventory"
  `((div :class "chunk-composer-title")
    ((a ,@(make-expand/collapse-link-parameters
           element-id t "expand chunk composer")
        :name "chunk composer")
     ,(format nil "IRL CHUNK COMPOSER (~a)"
              (length (chunks composer))))))

(defmethod expanded-chunk-composer-html ((composer chunk-composer) element-id
                                         &key (expand-initially nil)
                                         (expand/collapse-all-id (make-id 'cc)))
  "html for the expanded version of a primitive inventory"
  (lambda ()
    `((div :class "chunk-composer-box")
      ((div :class "chunk-composer-title"
            :style "border-bottom:1px dashed;")
       ((a ,@(make-expand/collapse-link-parameters
              element-id nil "collapse chunk composer")
           :name "chunk composer")
        ,(format nil "IRL CHUNK COMPOSER (~a)"
                 (length (chunks composer)))))
      ;; show the configurations
      ((div :style "border-bottom:1px dashed;")
       "Configurations:" ((br))
       ,@(html-hide-rest-of-long-list
          (entries (configuration composer)) 3
          #'html-pprint))
      ;; show the chunks
      ((table :class "chunk-composer")
       ,@(loop for chunk in (chunks composer)
               collect `((tr)
                         ((td :class "chunk-composer-details")
                          ,(make-html chunk :expand-initially expand-initially
                                      :expand/collapse-all-id expand/collapse-all-id))))))))

(defmethod make-html ((composer chunk-composer)
                      &key (expand-initially nil)
                      (expand/collapse-all-id (make-id 'cc)))
  `((div :class "chunk-composer")
    ,(let ((element-id (make-id 'composer)))
       (make-expandable/collapsable-element
        element-id expand/collapse-all-id
        ;; collapsed version
        (collapsed-chunk-composer-html composer element-id)
        ;; expanded version
        (expanded-chunk-composer-html composer element-id
                                      :expand-initially expand-initially
                                      :expand/collapse-all-id expand/collapse-all-id)
        :expand-initially expand-initially))))
