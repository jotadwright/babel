(in-package :irl)

;; -----------------------------------
;; + chunk-composer-node - make-html +
;; -----------------------------------

(defparameter *chunk-composer-node-status-colors*
  '((initial . "#444") (nil . "#444")
    (duplicate . "#520") (solution . "#050")
    (bad-evaluation-results . "#822")
    (match-chunk-failed . "#822")
    (match-chunk-succeeded . "#822")
    (chunk-wrapper-failed . "#822")
    (no-evaluation-results . "#337")
    (expanded . "#480")
    (max-depth-reached . "#888")
    (max-irl-program-length-reached . "#888")))

(defun ccn->title-text (node)
  (if (find 'initial (statuses node))
    (format nil "initial (~a, ~,2f)"
            (created-at node) (cost node))
    (format nil "~{~a~^ ~} (~a, ~,2f)"
            (mapcar (compose #'mkstr #'downcase)
                    (remove 'initial
                            (mapcar #'id
                                    (source-chunks node))))
            (created-at node)
            (cost node))))

(defun ccn-title-html (node element-id node-color &key expand)
  `((div :class "ccn-title"
         :style ,(mkstr "background-color:" node-color ";"
                        (if (eq (first (statuses node)) 'solution)
                          "font-weight:bold" "")))
    ((a ,@(make-expand/collapse-link-parameters element-id expand))
     ((span) ,(ccn->title-text node)))))

(defmethod collapsed-ccn-html ((node chunk-composer-node)
                               element-id node-color)
  `((div :class "ccn-box")
    ,(ccn-title-html node element-id node-color :expand t)))

(defmethod expanded-ccn-html ((node chunk-composer-node)
                              element-id node-color
                              &key (expand-initially nil)
                              (expand/collapse-all-id (make-id 'ccn)))
  (lambda ()
    `((div :class "ccn-box")
      ,(ccn-title-html node element-id node-color :expand nil)
      ((table :class "ccn")
       ;; status
       ((tr :style "border-bottom:1px solid")
        ((td :class "ccn-details") "statuses")
        ((td :class "ccn-details")
         ((span :style ,(mkstr "color:" node-color ";"))
          ,(downcase (mkstr (first (statuses node)))))
         ,@(when (rest (statuses node))
             `(((span) ,(format nil " (~{~a~^, ~})"
                                (mapcar (compose #'downcase #'mkstr)
                                        (rest (statuses node)))))))))
       ;; next handler
       ((tr :style "border-bottom:1px solid")
        ((td :class "ccn-details") "next handler")
        ((td :class "ccn-details")
         ((span)
          ,(if (next-handler node)
             (downcase (mkstr (next-handler node)))
             "none"))))
       ;; source chunks
       ,@(when (source-chunks node)
           `(((tr :style "border-bottom:1px solid")
              ((td :class "ccn-details") "source chunks")
              ((td :class "ccn-details")
               ,@(loop for chunk in (source-chunks node)
                       collect (make-html chunk))))))
       ;; chunk
       ((tr :style "border-bottom:1px solid")
        ((td :class "ccn-details") "chunk")
        ((td :class "ccn-details")
         ,(make-html (chunk node))))
       ;; meaning to match
       ,@(when (meaning (composer node))
           `(((tr :style "border-bottom:1px solid")
              ((td :class "ccn-details") "meaning to match")
              ((td :class "ccn-details")
               ,(irl-program->svg (meaning (composer node)))))))
       ;; matched chunks
       ,@(when (find-data node 'matched-chunks)
           (loop with matched-chunks = (find-data node 'matched-chunks)
                 with new-chunks = (remove (chunk node) matched-chunks)
                 for chunk in new-chunks
                 for i from 1
                 collect `((tr :style "border-bottom:1px solid")
                           ((td :class "ccn-details")
                            ,(format nil "matched chunk ~a" i))
                           ((td :class "ccn-details")
                            ,(make-html chunk)))))
       ;; chunk evaluation results
       ,@(when (cers node)
           `(((tr :style "border-bottom:1px solid")
              ((td :class "ccn-details") "chunk evaluation results")
              ((td :class "ccn-details")
               ,@(html-hide-rest-of-long-list
                  (chunk-evaluation-results node) 3
                  #'(lambda (cer)
                      (make-html cer)))))))
       ))))

(defun collapsed-hidden-composition-subtree-html (element-id)
  `((div :class "ccn-hidden-subtree")
    ((a ,@(make-expand/collapse-link-parameters
           element-id t "expand subtree"))
     "+")))

(defun expanded-hidden-composition-subtree-html (hidden-children element-id
                                                 &key (expand/collapse-all-id (make-id 'subtree)))
  (draw-node-with-children
   `((div :class "ccn-hidden-subtree")
     ((a ,@(make-expand/collapse-link-parameters
            element-id nil "collapse subtree"))
      "-"))
   (loop for child in hidden-children
         collect (make-html child :expand/collapse-all-id expand/collapse-all-id))))

(defmethod make-html ((node chunk-composer-node)
                      &key solutions (draw-as-tree t)
                      (expand-initially nil)
                      (expand/collapse-all-id (make-id 'ccn)))
  (let* ((element-id (make-id 'ccn))
         (node-color
          (or (when (= (created-at node) 0) "#444")
              (assqv (first (statuses node)) *chunk-composer-node-status-colors*)
              (error "no status color defined for status ~a" (first (statuses node)))))
         (node-div
          `((div :class "ccn")
            ,(make-expandable/collapsable-element
              element-id (make-id)
              ;; collapsed element
              (collapsed-ccn-html node element-id node-color)
              ;; expanded element
              (expanded-ccn-html node element-id node-color
                                 :expand/collapse-all-id expand/collapse-all-id)))))
    (if draw-as-tree
      (draw-node-with-children
       node-div
       (let ((subtree-id (make-id 'subtree))
             nodes-to-show nodes-to-hide)
         (if solutions
           (loop for child in (children node)
                 if (on-path-to-target-p child solutions)
                 do (push child nodes-to-show)
                 else do (push child nodes-to-hide))
           (setf nodes-to-show (children node)))
         (shuffle (append
                   (loop for child in nodes-to-show
                         collect (make-html child :solutions solutions
                                            :expand-initially expand-initially
                                            :expand/collapse-all-id expand/collapse-all-id))
                   (if nodes-to-hide
                     (list 
                      (make-expandable/collapsable-element
                       subtree-id expand/collapse-all-id
                       ;; collapsed element
                       (collapsed-hidden-composition-subtree-html subtree-id)
                       ;; expanded element
                       (expanded-hidden-composition-subtree-html nodes-to-hide subtree-id
                                                                 :expand/collapse-all-id expand/collapse-all-id)
                       :expand-initially expand-initially))
                     nil))))
       :color "#aaa")
      `((div :class "ccn-float")
        ,node-div))))
