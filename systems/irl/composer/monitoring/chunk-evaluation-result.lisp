(in-package :irl)

;; ---------------------------------------
;; + chunk-evaluation-result - make-html +
;; ---------------------------------------

(defmethod collapsed-cer-html ((result chunk-evaluation-result) element-id)
  (let ((title 
         (append 
          (loop for value in (mapcar #'fourth (bind-statements result))
                for i from 1
                collect (format nil "~(~a~)~:[~;,&#160;~]" value
                                (< i (length (bind-statements result)))))
          (list (format nil " (~,2f)" (score result))))))
    `((div :class "cer-box")
      ((div :class "cer-title")
       ((a ,@(make-expand/collapse-link-parameters element-id t))
        ,@title)))))

(defmethod expanded-cer-html ((result chunk-evaluation-result)
                              element-id
                              &key expand/collapse-all-id
                              (expand-initially nil))
  (let ((title
         (append 
          (loop for value in (mapcar #'fourth (bind-statements result))
                for i from 1
                collect (format nil "~(~a~)~:[~;,&#160;~]" value
                                (< i (length (bind-statements result)))))
          (list (format nil " (~,2f)" (score result)))))
        (bindings-id (make-id 'bindings))
        (target-id (make-id 'target))
        (chunk-id (make-id 'chunk)))
    (lambda ()
      `((div :class "cer-box")
        ((div :class "cer-title")
         ((a ,@(make-expand/collapse-link-parameters element-id nil))
          ,@title))
        ((table :class "cer")
         ((tbody)
          ;; chunk
          ((tr)
           ((td :class "cer-details")
            ,(make-expand/collapse-all-link chunk-id "chunk"))
           ((td :class "cer-details")
            ,(make-html (chunk result) :expand-initially t
                        :expand/collapse-all-id chunk-id)))
          ;; evaluation tree
          ((tr)
           ((td :class "cer-details") "evaluation process")
           ((td :class "cer-details")
            ,(make-html (pip (pip-node result))
                        :target-node (pip-node result))))
          ;; target entity
          ((tr)
           ((td :class "cer-details")
            ,(make-expand/collapse-all-link target-id "target entity"))
           ((td :class "cer-details")
            ,(make-html (target-entity result) :expand/collapse-all-id target-id)))
          ;; bindings
          ((tr)
           ((td :class "cer-details")
            ,(make-expand/collapse-all-link bindings-id "bindings"))
           ((td :class "cer-details") 
            ,@(loop for b in (bindings result)
                    collect (make-html b :expand/collapse-all-id bindings-id))))
          ;; bind statements
          ((tr)
           ((td :class "cer-details") "bind statements")
           ((td :class "cer-details")
            ,(html-pprint (bind-statements result)
                          :max-width 100)))))))))

(defmethod make-html ((result chunk-evaluation-result)
                      &key (expand/collapse-all-id (make-id 'cer))
                      (expand-initially nil))
  (let ((cer-div-id (make-id 'cer)))
    `((div :class "cer")
      ,(make-expandable/collapsable-element 
        cer-div-id expand/collapse-all-id
        (collapsed-cer-html result cer-div-id)
        (expanded-cer-html result cer-div-id
                           :expand/collapse-all-id expand/collapse-all-id
                           :expand-initially expand-initially)
        :expand-initially expand-initially))))

(defun composer-solutions->html (solutions)
  (add-element
   `((div :style "margin-left: 45px;") ,@(html-hide-rest-of-long-list
                                          solutions 3
                                          #'(lambda (result)
                                              `((div :class "cer-float")
                                                ,(make-html result)))))))
