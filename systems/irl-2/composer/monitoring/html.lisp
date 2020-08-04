
(in-package :irl-2)


;; #########################################################
;; chunk-evaluation-result - make-html
;; ---------------------------------------------------------

(define-css 'cer "
div.cer { display:inline-block;margin-right:0px;margin-top:10px;
           margin-bottom:10px;padding:0px; }
div.cer-box { border:1px solid #562; display:inline-block;}
div.cer div.cer-title  { 
  padding:0px;padding-left:3px;padding-right:3px;
  white-space:nowrap; background-color:#562; }
div.cer div.cer-title > a {color:#fff;}
div.cer div.cer-title > span {color:#fff;}
div.cer-float { display:inline-block;margin-right:10px;
                margin-top:-6px;margin-bottom:8px; }
table.cer {
  border-collapse:collapse; }
table.cer td.cer-type { font-style:italic;padding:0px;padding-left:4px;}
table.cer td.cer-details { vertical-align:top;padding-top:3px;padding-bottom:3px;
  padding-left:5px;padding-right:5px; }
table.cer td.cer-details div.cer-detail { 
  padding-left:4px; padding-right:4px;padding-bottom:1px;padding-top:1px;
  border-top:1px dashed #563;text-align:left; }
table.cer td.cer-details > div { overflow:hidden; }
table.cer td.cer-details div.cer-detail:first-child { border-top:none; }
div.cer-hidden-subtree { padding:0px;margin:0px;padding:0px;margin-bottom:2px; }
")

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
            ,(make-html (processor (evaluation-node result)))))
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
   (html-hide-rest-of-long-list
    solutions 3
    #'(lambda (result)
        `((div :class "cer-float")
          ,(make-html result))))))

;; #########################################################
;; chunk-composer-node - make-html
;; ---------------------------------------------------------

(defparameter *chunk-composer-node-status-colors*
  '((initial . "#444")
    (duplicate . "#520")
    (solution . "#050")
    (bad-evaluation-results . "#822")
    (match-chunk-failed . "#822")
    (no-evaluation-results . "#337")
    (expanded . "#480")
    (max-depth-reached . "#888")))

(define-css 'ccn "
div.ccn { display:inline-block;margin-right:0px;margin-top:10px;
           margin-bottom:10px;padding:0px; }
div.ccn-box { border:1px solid #562; display:inline-block;}
div.ccn div.ccn-title  { 
  padding:0px;padding-left:3px;padding-right:3px;
  white-space:nowrap; background-color:#562; }
div.ccn div.ccn-title > a {color:#fff;}
div.ccn div.ccn-title > span {color:#fff;}
div.ccn-float { display:inline-block;margin-right:10px;
                margin-top:-6px;margin-bottom:8px; }
table.ccn {
  border-collapse:collapse; }
table.ccn td.ccn-type { font-style:italic;padding:0px;padding-left:4px;}
table.ccn td.ccn-details { vertical-align:top;padding-top:3px;padding-bottom:3px;
  padding-left:5px;padding-right:5px; }
table.ccn td.ccn-details div.ccn-detail { 
  padding-left:4px; padding-right:4px;padding-bottom:1px;padding-top:1px;
  border-top:1px dashed #563;text-align:left; }
table.ccn td.ccn-details > div { overflow:hidden; }
table.ccn td.ccn-details div.ccn-detail:first-child { border-top:none; }
div.ccn-hidden-subtree { padding:0px;margin:0px;padding:0px;margin-bottom:2px; }
")

(defun ccn->title-text (node)
  (if (= (node-number node) 0)
    "initial"
    (format nil "~{~a~^ ~} (~a, ~a)"
            (mapcar (compose #'mkstr #'downcase)
                    (remove 'initial
                            (mapcar #'id
                                    (source-chunks node))))
            (node-number node)
            (node-rating node))))

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
       ;; chunk evaluation results
       ,@(when (cers node)
           `(((tr :style "border-bottom:1px solid")
              ((td :class "ccn-details") "chunk evaluation results")
              ((td :class "ccn-details")
               ,@(loop for cer in (chunk-evaluation-results node)
                       collect (make-html cer))))))
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
          (or (when (= (node-number node) 0) "#444")
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
                 if (on-path-to-solution-p child solutions)
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
        


;; #########################################################
;; make html for composition process
;; ---------------------------------------------------------

(defun make-collapsed-html-for-composition-process (composer element-id)
  (let ((solution-nodes
         (find-all 'solution (nodes composer)
                   :key #'statuses :test #'member)))
    `((table :class "two-col")
      ((tbody)
       ((tr)
        ((td)
         ((a ,@(make-expand/collapse-link-parameters
                element-id t "composition process"))
          "composition process"))
        ((td)
         ((div :style "margin-top:-7px")
          ,(make-html (top composer) :expand-initially nil
                      :solutions solution-nodes))))))))

(defun make-expanded-html-for-composition-process (composer element-id)
  (let ((solution-nodes
         (find-all 'solution (nodes composer)
                   :key #'statuses :test #'member)))
    `((table :class "two-col")
      ((tbody)
       ((tr)
        ((td)
         ((a ,@(make-expand/collapse-link-parameters
                element-id nil "composition process"))
          "composition process"))
        ((td)
         ((div :style "margin-top:-7px")
          ,(make-html (top composer) :expand-initially t
                      :solutions solution-nodes))))))))
  
(defmethod make-html-for-composition-process ((composer chunk-composer))
  (let ((element-id (make-id 'composition-process)))
    (make-expandable/collapsable-element
     element-id (make-id)
     (make-collapsed-html-for-composition-process composer element-id)
     (make-expanded-html-for-composition-process composer element-id))))

;; #########################################################
;; chunk-composer - make-html
;; ---------------------------------------------------------

(define-css 'chunk-composer "
div.chunk-composer { display:inline-block;margin-right:10px;margin-top:4px;
         margin-bottom:4px;padding:0px; }
div.chunk-composer-box { border:1px solid; display:inline-block;}
div.chunk-composer div.chunk-composer-title  { 
  padding:0px;padding-left:3px;padding-right:3px;white-space:nowrap; }
div.chunk-composer div.chunk-composer-title > a { color:#40241A;font-weight:bold; }
table.chunk-composer { border-collapse:collapse; }
table.chunk-composer td.chunk-composer-type { font-style:italic;padding:0px;padding-left:4px;}
table.chunk-composer td.chunk-composer-details div.chunk-composer-detail { 
  padding-left:4px; padding-right:4px;padding-bottom:1px;padding-top:1px;
  border-top:1px dashed #563;text-align:left;  }
table.chunk-composer td.chunk-composer-details > div { overflow:hidden; }
table.chunk-composer td.chunk-composer-details div.chunk-composer-detail:first-child { border-top:none;} 
")

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
  

