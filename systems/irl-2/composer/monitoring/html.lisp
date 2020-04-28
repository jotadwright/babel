
(in-package :irl-2)

(defgeneric content->html-table-rows (node &key))

#|
(defun make-tr-for-queue (caption queue)
  `((tr)
    ((td) ,caption)
    ((td) 
     ,@(html-hide-rest-of-long-list
        queue 5 #'(lambda (node) (make-html node :draw-as-tree nil))))))

(defun make-tr-for-evaluation-results (caption evaluation-results)
  (if evaluation-results
      (let ((expand/collapse-all-id (make-id 'results)))
        `((tr)
          ((td) ,(make-expand/collapse-all-link expand/collapse-all-id caption))
          ((td) 
           ,@(html-hide-rest-of-long-list
              evaluation-results 10
              #'(lambda (r)
                  (make-html r :expand/collapse-all-id expand/collapse-all-id))))))
      ""))

(defun make-tr-for-tree (caption top-node)
  (let ((expand/collapse-all-id (make-id 'tree)))
    `((tr)
      ((td) ,(make-expand/collapse-all-link expand/collapse-all-id caption))
      ((td) ,(make-html top-node
                        :expand/collapse-all-id expand/collapse-all-id)))))

(defun make-tr-for-irl-evaluation-search-process (caption node-evaluation-tree)
  (let ((top-node (top node-evaluation-tree)))
    (labels ((all-leaves (node)
               (if (children node)
                 (mappend #'all-leaves (children node))
                 (list node))))
      (let ((tree-id-1 (make-id 'evaluation-tree))
            (tree-id-2 (make-id 'evaluation-tree))
            (best-leaf
             (the-highest (leafs node-evaluation-tree)
                          #'(lambda (x) (or (length (primitives-evaluated x)) 0)))))
        `((tr)
          ((td) ,(make-expandable/collapsable-element 
                  (make-id) tree-id-1
                  `((a ,@(make-expand/collapse-all-link-parameters
                        tree-id-1 "show evaluation process"))
                    ,caption)
                  (make-expand/collapse-all-link tree-id-2 caption)))
          ((td) ,(make-expandable/collapsable-element
                  (make-id) tree-id-1
                  `((a ,@(make-expand/collapse-all-link-parameters
                          tree-id-1 "show evaluation process"))
                  ((i) ,(format 
                         nil "initial~{ - ~(~a~)~}" 
                         (reverse 
                          (mapcar 
                           #'car  
                           (append 
                            (primitives-evaluated best-leaf)
                            (primitives-evaluated-w/o-result best-leaf)))))))
                (lambda ()
                  `((div :style "margin-top:-6px")
                    ,(make-html top-node :expand/collapse-all-id tree-id-2))))))))))

;; #########################################################
;; chunk-evaluation-result - make-html
;; ---------------------------------------------------------

(define-css 'cer "
div.cer { border:1px solid #465; display:inline-block; margin-right:7px; margin-bottom:7px;}
div.cer .cer-title { background-color:#465;padding:1px;
                     padding-left:3px;padding-right:3px; }
div.cer .cer-title > a { color:white; }
div.cer table.cer-details  { margin:3px;margin-bottom:-5px;}
div.cer table.cer-details img { margin:-15px;margin-top:0px;margin-right:0px; }
")

(defmethod make-html ((result chunk-evaluation-result)
                      &key (expand/collapse-all-id (make-id 'cer))
                      (expand-initially nil))
  (let ((cer-div-id (make-id 'cer))
        (bindings-id (make-id 'bindings))
        (title 
         (append 
          (loop for value in (mapcar #'fourth (bind-statements result))
             for i from 1
             collect (format nil "~(~a~)~:[~;,&#160;~]" value
                             (< i (length (bind-statements result)))))
          (list (format nil " (~,2f)" (score result))))))
    (make-expandable/collapsable-element 
     cer-div-id expand/collapse-all-id
     `((div :class "cer")
       ((div :class "cer-title")
        ((a ,@(make-expand/collapse-link-parameters cer-div-id t)) ,@title)))
     (lambda ()
       `((div :class "cer")
         ((div :class "cer-title")
          ((a ,@(make-expand/collapse-link-parameters cer-div-id nil)) ,@title))
         ((table :class "cer-details two-col")
          ((tbody)
           ((tr)
            ((td) "chunk")
            ((td) ,(make-html (chunk result)
                              :expand-initially t)))
           ,@(when (evaluation-tree result)
                   `(,(make-tr-for-irl-evaluation-search-process 
                       "evaluation process"
                       (evaluation-tree result))))
           ((tr)
            ((td) "target entity")
            ((td) ,(make-html (target-entity result))))
           ((tr)
            ((td) ,(make-expand/collapse-all-link bindings-id "bindings"))
            ((td) 
             ,@(loop for b in (bindings result)
                  collect (make-html b :expand/collapse-all-id bindings-id))))
           ((tr)
            ((td) "bind statements")
            ((td) ,(html-pprint (bind-statements result) :max-width 100)))))))
     :expand-initially expand-initially)))

(defmethod make-html ((result chunk-composer-node-solution)
                      &key (expand/collapse-all-id (make-id 'cer))
                      (expand-initially nil))
  (let ((cer-div-id (make-id 'cer))
        (bindings-id (make-id 'bindings))
        (title 
         (append 
          (loop for value in (mapcar #'fourth (bind-statements result))
             for i from 1
             collect (format nil "~(~a~)~:[~;,&#160;~]" value
                             (< i (length (bind-statements result)))))
          (list (format nil " (~,2f)" (score result))))))
    (make-expandable/collapsable-element 
     cer-div-id expand/collapse-all-id
     `((div :class "cer")
       ((div :class "cer-title")
        ((a ,@(make-expand/collapse-link-parameters cer-div-id t)) ,@title)))
     (lambda ()
       `((div :class "cer")
         ((div :class "cer-title")
          ((a ,@(make-expand/collapse-link-parameters cer-div-id nil)) ,@title))
         ((table :class "cer-details two-col")
          ((tbody)
           ((tr)
            ((td) "node")
            ((td) ,(make-html (node result) :draw-as-tree nil)))
           ((tr)
            ((td) "chunk")
            ((td) ,(make-html (chunk result)
                              :expand-initially t)))
           ,@(when (evaluation-tree result)
                   `(,(make-tr-for-irl-evaluation-search-process 
                       "evaluation process"
                       (evaluation-tree result))))
           ((tr)
            ((td) "target entity")
            ((td) ,(make-html (target-entity result))))
           ((tr)
            ((td) ,(make-expand/collapse-all-link bindings-id "bindings"))
            ((td) 
             ,@(loop for b in (bindings result)
                  collect (make-html b :expand/collapse-all-id bindings-id))))
           ((tr)
            ((td) "bind statements")
            ((td) ,(html-pprint (bind-statements result) :max-width 100)))))))
     :expand-initially expand-initially)))

;; #########################################################
;; dead-node?
;; #########################################################

(defgeneric dead-node? (node))

(defmethod dead-node? ((node chunk-composer-node))
  nil)

(defmethod dead-node? ((node single-topic-composer-node))
  "Determines wheter a subtree is fully expanded and does not contain solutions"
  (labels ((dead-node-aux (node)
             (if (children node)
                 (loop for child in (children node)
                    always (dead-node-aux child))
                 (member (status node) 
                         '(match-chunk-failed
                           chunk-wrapper-failed
                           duplicate-chunk
                           bad-chunk
                           max-search-depth-reached
                           no-further-combination-possible
                           no-evaluation-results
                           combined-program)))))
    (and (> (depth node) 1) (children node) (dead-node-aux node))))

;; #########################################################
;; chunk-composer-node - make-html & content->htm-table-rows
;; ---------------------------------------------------------

(define-css 'ccn "
div.ccn { margin-top:5px;margin-bottom:5px; }
div.ccn-light { margin-top:5px;margin-bottom:5px;padding-left:1px;
                font-style:italic; }
div.ccn-float { display:inline-block;margin-right:10px;
                margin-top:-6px;margin-bottom:8px; }
div.ccn .ccn-title { color:white; position:relative; }
div.ccn .ccn-title > a { color:white; }
div.ccn .ccn-title > div.save-button { 
   position:absolute;right:5px;top:1px;display:none;}
div.ccn .ccn-title > div.save-button > a { color:#fff; }
div.ccn .ccn-title { padding:1px;padding-left:3px;padding-right:3px;}
div.ccn .ccn-details { padding:2px; }
div.ccn .ccn-details > table.two-col { margin-bottom:-5px;}
div.ccn-dead-node { padding-left:2px;
                    padding-right:1px;padding-bottom:1px;}
")

(export '*saveable-ccns*)

;; keeps each ccn that is drawn in a hash table for potential saving
(defvar *saveable-ccns* (make-hash-table :test #'equal))

;; empty the hash table when the 'reset' button is pushed
(defun reset-saveable-ccns ()
  (setf *saveable-ccns* (make-hash-table :test #'equal)))

(pushnew #'reset-saveable-ccns wi::*reset-functions*)

;; this is the variable that the node is saved to
(defvar *saved-ccn* nil)

(export '(*saved-ccn* *chunk-composer-node-status-colors*))

(ht-simple-ajax:defun-ajax save-ccn (id) (wi::*ajax-processor*)
  "Called from the html page to save a ccn to a global variable"
  (setf *saved-ccn* (gethash id *saveable-ccns*))
  (add-element `((hr)))
  (add-element `((p) "Saved chunk composer node " ((div) ,(make-html *saved-ccn*))
		 " to global variable " ((tt) ((b) "*saved-ccn*"))))
  (render-xml nil))

;; #* added
(defparameter *chunk-composer-node-handler-colors*
  '((initial . "#8a8") (processed . "#79a") (evaluate . "#7aa")
    (expand . "#268") (evaluation-succeeded . "#273")))

;; #* added 
(defmethod html-color ((node chunk-composer-node))
  (let ((next-handler (cond ((solutions node) 'evaluation-succeeded)
                            ((next-handler node) (next-handler node))
                            (t 'processed))))
    (assqv next-handler *chunk-composer-node-handler-colors*)))

(defmethod make-html ((node chunk-composer-node) 
                      &key (expand/collapse-all-id (make-id 'ccn))
                      (draw-as-tree t)
                      (hide-dead-subtree t)
                      (expand-initially nil)
                      (initial-node nil))
  (let* ((node (copy-object node))
         (element-id (make-id 'ccn))
         (save-button-id (make-id 'ccn))
         (node-color (html-color node))
         (title-div
          `((div :class "ccn-title" 
                 :style ,(mkstr "background-color:" node-color ";"))))
         (title 
          (list (format nil "~a&#160;" (sequence-number node))
                (if (and (not (irl-program->title (irl-program (chunk node))))
                         initial-node)
                  "initial"
                  (irl-program->title (irl-program (chunk node))))))
                  
         (div 
          (make-expandable/collapsable-element 
           element-id expand/collapse-all-id
               `((div :class "ccn" :style ,(mkstr "border:1px solid " node-color))
                 (,@title-div
                  ((a ,@(make-expand/collapse-link-parameters element-id t)) 
                   ,@title)))
           (lambda ()
             `((div :class "ccn" :style ,(mkstr "border:1px solid " node-color)
                    ,@(unless wi::*static-html*
                              `(:onmouseover 
                                ,(mkstr "document.getElementById('" save-button-id
                                        "').style.display = 'inline';")
                                :onmouseout 
                                ,(mkstr "document.getElementById('" save-button-id 
                                        "').style.display = 'none';"))))
               (,@title-div
                ((a ,@(make-expand/collapse-link-parameters element-id nil))
                 ,@title)
                ,@(unless 
                   wi::*static-html* 
                   (setf (gethash (format nil "~(~a~)" save-button-id) 
                                  *saveable-ccns*)
                         node)
                   `(((div :class "save-button" :id ,(mkstr save-button-id)) 
                      ((a :href ,(format nil "javascript:ajax_save_ccn('~(~a~)');" 
                                         save-button-id)
                          :title "save node") "save")))))
               ((div :class "ccn-details")
                ((table :class "two-col")
                 ((tbody)
                  ,@(content->html-table-rows node)
                  )))))
           :expand-initially expand-initially)))
    (if draw-as-tree 
        (draw-node-with-children 
         div
         (if (and hide-dead-subtree (dead-node? node))
             (let ((id (make-id 'sub-tree)))
               (list (make-expandable/collapsable-element 
                      id (make-id)
                      `((div :class "ccn-dead-node")
                        ((a ,@(make-expand/collapse-link-parameters 
                               id t "show dead subtree")) "+"))
                      (draw-node-with-children 
                       `((div :class "ccn-dead-node")
                         ((a ,@(make-expand/collapse-link-parameters 
                                id nil "hide dead subtree")) "-"))
                       (loop for child in (children node)
                          collect (make-html child :hide-dead-subtree nil))))))
             (loop for child in (children node)
                collect (make-html child :hide-dead-subtree hide-dead-subtree
                                   :expand/collapse-all-id expand/collapse-all-id))))
         `((div :class "ccn-float")
           ,div))))

(defmethod content->html-table-rows ((node chunk-composer-node) &key)
  (let ((node-color (html-color node)))
    `(((tr)
       ((td) "next-handler")
       ((td)
        ((span :style ,(mkstr "color:" node-color)) 
                 ,(format nil "~(~a~)" (next-handler node))
                 ,@(if 
                       (handler-history node)
                     `(" (" 
                       ,@(loop for s on (handler-history node)
                               collect 
                               `((span 
                                  :style 
                                  ,(mkstr 
                                    "color:" 
                                    (assqv (car s) 
                                           *chunk-composer-node-handler-colors*)))
                                 ,(format nil "~(~a~)~:[~;, ~]" (car s) (cdr s))))
                       ")")))))
      ((tr)
       ((td) "source-chunks")                   
       ((td) ,@(loop for c in (source-chunks node)
                     collect
                     (make-html c))))
      ((tr)
       ((td) "chunk")
       ((td) ,(make-html (chunk node) :expand-initially t)))
      ,@(if (matched-chunks node)
          (loop with new-chunks 
                = (remove (chunk node) (matched-chunks node))
                with l>1 = (> (length new-chunks) 1)
                for c in new-chunks  for i from 1
                collect `((tr) 
                          ((td) ,(mkstr "matched chunk " (if l>1 i "")))
                          ((td) ,(make-html c :expand-initially t))))
          '(""))
      ,(make-tr-for-evaluation-results 
        "evaluation results" (solutions node)))))

;; ##########################################################
;; single-topic-composer-node - content->html-table-rows
;; ----------------------------------------------------------

(defparameter *chunk-composer-node-status-colors*
  '((initial . "#8a8") (combined-program . "#79a") (combined-call-pattern . "#7aa")
    (recombined-open-variables . "#7a9") (linked-open-variables . "#799")
    (match-chunk-failed . "#876") (chunk-wrapper-failed . "#867")
    (duplicate-chunk . "#765") (bad-chunk . "#744")
    (no-evaluation-results . "#268") (no-further-combination-possible . "#678") 
    (max-search-depth-reached . "#556") 
    (bad-evaluation-results . "#845") (evaluation-succeeded . "#273")))

(defmethod html-color ((node single-topic-composer-node))
  (or (assqv (status node) *chunk-composer-node-status-colors*)
      (error "no status color defined for status ~a" (status node))))

(defmethod content->html-table-rows ((node single-topic-composer-node) &key)
  (let ((default-trs (call-next-method node))
        (node-color (html-color node)))
    `(;;handler
      ,(first default-trs)
      ;;status
      ((tr)
       ((td) "status")
       ((td) 
        ((span :style ,(mkstr "color:" node-color)) 
         ,(format nil "~(~a~)" (status node))
         ,@(if 
               (status-history node)
             `(" (" 
               ,@(loop for s on (status-history node)
                       collect 
                       `((span 
                          :style 
                          ,(mkstr 
                            "color:" 
                            (assqv (car s) 
                                   *chunk-composer-node-status-colors*)))
                         ,(format nil "~(~a~)~:[~;, ~]" (car s) (cdr s))))
               ")")))))
      ;;source-chunks
      ,(second default-trs)
      ;;chunk
      ,(third default-trs)
      ;;matched
      ,@(subseq default-trs 3 (1- (length default-trs)))
      ;;wrapped
      ,@(if (wrapped-chunks node)
          (loop with new-chunks 
                = (loop for chunk in (wrapped-chunks node)
                        unless (or (find chunk (matched-chunks node))
                                   (eq chunk (chunk node)))
                        collect chunk)
                with l>1 = (> (length new-chunks) 1)
                for c in new-chunks  for i from 1
                collect `((tr) 
                          ((td) ,(mkstr "wrapped chunk " (if l>1 i "")))
                          ((td) ,(make-html c :expand-initially t))))
          '(""))
      ;;evaluation trees
      ,@(let ((evaluation-trees (or (loop for sol in (solutions node)
                                          when (evaluation-tree sol)
                                          collect (evaluation-tree sol))
                                    (and (evaluation-tree node)
                                         (list (evaluation-tree node))))))
          (when evaluation-trees
            (loop for tree in evaluation-trees
                  for i from 1
                  collect (make-tr-for-irl-evaluation-search-process
                           (mkstr "evaluation process" 
                                  (if (cdr evaluation-trees)
                                    i ""))
                           tree))))
      ;;bad-results
      ,(make-tr-for-evaluation-results 
        "bad evaluation results" (bad-evaluation-results node))
      ;;actual results
      ,@(last default-trs))))
|#

;; #########################################################
;; chunk-composer-node - make-html
;; ---------------------------------------------------------

(defparameter *chunk-composer-node-status-colors*
  '((initial . "#444")
    (duplicate . "#520")
    (solution . "#050")
    (bad-evaluation-results . "#822")
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
  (if (and (eq (first (statuses node)) 'initial)
              (= (node-number node) 0))
    "initial"
    (format nil "~a (~a, ~a)"
            (downcase
             (mkstr
              (id (chunk node))))
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
       ((tr :style ,(format nil "border-bottom:1px solid ~a" node-color))
        ((td :class "ccn-details") "status")
        ((td :class "ccn-details")
         ((span :style ,(mkstr "color:" node-color ";"))
          ,(downcase (mkstr (first (statuses node)))))))
       ;; next handler
       ((tr :style ,(format nil "border-bottom:1px solid ~a" node-color))
        ((td :class "ccn-details") "next handler")
        ((td :class "ccn-details")
         ((span)
          ,(if (next-handler node)
             (downcase (mkstr (next-handler node)))
             "none"))))
       ;; chunk
       ((tr :style ,(format nil "border-bottom:1px solid ~a" node-color))
        ((td :class "ccn-details") "chunk")
        ((td :class "ccn-details")
         ,(make-html (chunk node))))
       ;; chunk evaluation results
       ((tr :style ,(format nil "border-bottom:1px solid ~a" node-color))
        ((td :class "ccn-details") "chunk evaluation results")
        ((td :class "ccn-details") "TO DO"))))))

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
                      &key solutions (expand-initially nil)
                      (expand/collapse-all-id (make-id 'ccn)))
  (let* ((element-id (make-id 'ccn))
         (node-color
          (or (when (eq (id (chunk node)) 'initial) "#444")
              (assqv (first (statuses node)) *chunk-composer-node-status-colors*)
              (error "no status color defined for status ~a" (first (statuses node))))))
    (draw-node-with-children
     `((div :class "ccn")
       ,(make-expandable/collapsable-element
         element-id (make-id)
         ;; collapsed element
         (collapsed-ccn-html node element-id node-color)
         ;; expanded element
         (expanded-ccn-html node element-id node-color
                            :expand/collapse-all-id expand/collapse-all-id)))
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
     :color "#aaa")))


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
  nil)
  
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
  

