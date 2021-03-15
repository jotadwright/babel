
(in-package :irl)

(defgeneric content->html-table-rows (node &key))

;; #########################################################
;; draw-solutions
;; ---------------------------------------------------------

(defun solution->html (solution index)
  "Draw all the bindings in a single solution"
  (let ((id (make-id 'solution)))
    `((p) ,(make-expand/collapse-all-link 
            id (mkstr "solution " index " "))
      ,@(loop for binding in solution
              collect (make-html binding :expand/collapse-all-id id
                                 :expand-initially t)))))

(defun make-remaining-solutions-expander (nr-of-solutions expand/collapse-all-solutions-id)
  "Create the link to expand the remainder of the solutions"
  (make-expandable/collapsable-element
   (make-id) expand/collapse-all-solutions-id
   `((p) ((a ,@(make-expand/collapse-all-link-parameters
                expand/collapse-all-solutions-id t))
          "solutions 4-" ,nr-of-solutions))
   `((div))))

(defun make-collapsed-solution (solution-html expand/collapse-all-solutions-id)
  "Create a collapsed version of the remainder of the solutions"
  (make-expandable/collapsable-element
   (make-id) expand/collapse-all-solutions-id
   '((div)) solution-html))

(defun solutions->html (solutions)
  "Draws lists of bindings, initially hiding all from the fifth on"
  (if solutions
      (loop with expand/collapse-all-solutions-id = (make-id 'solutions)
            for solution in solutions
            for i from 1
            for p = (solution->html solution i)
            do
            (when (< i 4) (add-element p))
            (when (= i 4) 
              (add-element
               (make-remaining-solutions-expander
                (length solutions)
                expand/collapse-all-solutions-id)))
            (when (>= i 4)
              (add-element 
               (make-collapsed-solution p expand/collapse-all-solutions-id))))
      (add-element `((p) ((b) "no solutions")))))


(defun collect-solutions (solution-nodes)
  (let ((solution-paths
         (loop for node in solution-nodes
               collect (cons node (all-parents node))))
        solutions)
    (loop for solution-path in solution-paths
          for ordered-solution
          = (loop for node in solution-path
                  for parent = (parent node)
                  for new-bindings
                  = (if parent
                      (loop for b in (bindings node)
                            when (new-binding-p b parent)
                            collect b)
                      (find-all-if #'(lambda (b) (value b)) (bindings node)))
                  append new-bindings)
          do (push ordered-solution solutions))
    (sort solutions #'< :key #'length)))

                    

;; #########################################################
;; entity - make-html
;; ---------------------------------------------------------

(export '(make-html-for-entity-details))

(defgeneric make-html-for-entity-details (entity &key)
  (:documentation "Creates a list of divs for the expanded version of an entity"))

(export 'make-html-for-entity-details)

(defmethod make-html-for-entity-details ((e entity) &key &allow-other-keys)
  '(""))

(define-css 'entity "
div.entity { display:inline-block;margin-right:10px;margin-top:4px;
             margin-bottom:4px;padding:0px; }
div.entity-box { border:1px solid #562; display:inline-block;}
div.entity div.entity-title  { 
  padding:0px;padding-left:3px;padding-right:3px;
  white-space:nowrap; background-color:#562; }
div.entity div.entity-title > a {color:#fff;}
div.entity div.entity-title > span {color:#fff;}
table.entity { border-collapse:collapse; }
table.entity td.entity-type { font-style:italic;padding:0px;padding-left:4px;}
table.entity td.entity-details div.entity-detail { 
  padding-left:4px; padding-right:4px;padding-bottom:1px;padding-top:1px;
  border-top:1px dashed #563;text-align:left;  }
table.entity td.entity-details > div { overflow:hidden; }
table.entity td.entity-details div.entity-detail:first-child { border-top:none;} 
")

(defmethod collapsed-entity-html ((e entity) element-id)
  "html for the collapsed version of an entity"
  `((div :class "entity-box")
    ((div :class "entity-title")
     ((a ,@(make-expand/collapse-link-parameters 
            element-id t "expand entity")
         :name ,(mkstr (id e)))
      ,(format nil "~(~a~)" (id e))))))

(defmethod expanded-entity-html ((e entity) element-id parameters)
  "html for the expanded version of an entity"
  (lambda ()
    `((div :class "entity-box")
      ((div :class "entity-title")
       ((a ,@(make-expand/collapse-link-parameters
              element-id nil "collapse entity")
           :name ,(mkstr (id e)))
        ,(format nil "~(~a~)" (id e))))
      ((table :class "entity" :cellpadding "0" :cellspacing "0") 
       ((tr)
        ((td :class "entity-details")
         ,@(apply 'make-html-for-entity-details e parameters)))))))

(defmethod make-html ((e entity)
                      &rest parameters
                      &key (expand/collapse-all-id (make-id 'entity))
                      (expand-initially nil))
  `((div :class "entity")
    ,(let ((element-id (make-id (id e))))
       (make-expandable/collapsable-element 
        element-id expand/collapse-all-id
        ;; collapsed version
        (collapsed-entity-html e element-id)
        ;; expanded version
        (expanded-entity-html e element-id parameters)
        :expand-initially expand-initially))
    ((table :class "entity")
     ((tr) ((td :class "entity-type") 
            ,(format nil "~(~a~)" (type-of e)))))))

;; #########################################################
;; binding - make-html
;; ---------------------------------------------------------

(define-css 'binding "
div.binding { display:inline-block;margin-right:10px;
               margin-bottom:5px; border-collapse:collapse; }
div.binding > div.pprint { margin-left:3px;}
div.binding > table { border-collapse:collapse; }
div.binding > table > tbody > tr > td > div.entity { 
    margin-top:3px;margin-bottom:1px;}
div.binding div.unbound-value {
    color:#562;padding:0px;padding-right:3px;padding-left:3px;
    margin-top:3px;margin-bottom:1px;border:1px dotted #562; }
div.binding td.score { padding-left:4px; }
")

(defmethod make-html ((binding binding) &key (expand/collapse-all-id (make-id 'b))
                      (expand-initially nil))
  `((div :class "binding")
    ,(html-pprint (var binding))
    ((table)
     ((tbody)
      ((tr) 
       ((td) 
        ,(if (value binding)
             (make-html (value binding) :expand-initially expand-initially
                        :expand/collapse-all-id expand/collapse-all-id)
             '((div :class "unbound-value") "unbound"))))
      ,@(when (score binding)
              `(((tr)
                 ((td :class "score")
                  ,(format nil "~,3f" (score binding))))))))))

;; #########################################################
;; primitive-inventory - make-html
;; ---------------------------------------------------------

(define-css 'primitive-inventory "
div.primitive-inventory { display:inline-block;margin-right:10px;margin-top:4px;
         margin-bottom:4px;padding:0px; }
div.primitive-inventory-box { border:1px solid; display:inline-block;}
div.primitive-inventory div.primitive-inventory-title  { 
  padding:0px;padding-left:3px;padding-right:3px;white-space:nowrap; }
div.primitive-inventory div.primitive-inventory-title > a { color:#40241A;font-weight:bold; }
table.primitive-inventory { border-collapse:collapse; }
table.primitive-inventory td.primitive-inventory-type { font-style:italic;padding:0px;padding-left:4px;}
table.primitive-inventory td.primitive-inventory-details div.primitive-inventory-detail { 
  padding-left:4px; padding-right:4px;padding-bottom:1px;padding-top:1px;
  border-top:1px dashed #563;text-align:left;  }
table.primitive-inventory td.primitive-inventory-details > div { overflow:hidden; }
table.primitive-inventory td.primitive-inventory-details div.primitive-inventory-detail:first-child { border-top:none;} 
")

(defmethod collapsed-primitive-inventory-html ((primitive-inventory primitive-inventory) element-id)
  "html for the collapsed version of a primitive inventory"
  `((div :class "primitive-inventory-title")
    ((a ,@(make-expand/collapse-link-parameters
           element-id t "expand primitive inventory")
        :name ,(mkstr (name primitive-inventory)))
     ,(format nil "IRL PRIMITIVE INVENTORY (~a)"
              (length (primitives primitive-inventory))))))

(defmethod expanded-primitive-inventory-html ((primitive-inventory primitive-inventory) element-id
                                              &key (expand-initially nil)
                                              (expand/collapse-all-id (make-id 'pi)))
  "html for the expanded version of a primitive inventory"
  (lambda ()
    `((div :class "primitive-inventory-box")
      ((div :class "primitive-inventory-title"
            :style "border-bottom:1px dashed;")
       ((a ,@(make-expand/collapse-link-parameters
              element-id nil "collapse primitive inventory")
           :name ,(mkstr (name primitive-inventory)))
        ,(format nil "IRL PRIMITIVE INVENTORY (~a)"
                 (length (primitives primitive-inventory)))))
      ;; show the configurations
      ((div :style "border-bottom:1px dashed;")
       "Configurations:" ((br))
       ,@(html-hide-rest-of-long-list (entries (configuration primitive-inventory)) 3 #'html-pprint))
      ;; show the primitives
      ((table :class "primitive-inventory")
       ,@(loop for p in (primitives primitive-inventory)
               collect `((tr)
                         ((td :class "primitive-inventory-details")
                          ,(make-html p :expand-initially expand-initially
                                      :expand/collapse-all-id expand/collapse-all-id))))))))

(defmethod make-html ((primitive-inventory primitive-inventory)
                      &key (expand-initially nil)
                      (expand/collapse-all-id (make-id 'pi)))
  `((div :class "primitive-inventory")
    ,(let ((element-id (make-id (name primitive-inventory))))
       (make-expandable/collapsable-element
        element-id expand/collapse-all-id
        ;; collapsed version
        (collapsed-primitive-inventory-html primitive-inventory element-id)
        ;; expanded version
        (expanded-primitive-inventory-html primitive-inventory element-id
                                           :expand-initially expand-initially
                                           :expand/collapse-all-id expand/collapse-all-id)
        :expand-initially expand-initially))))
            


;; #########################################################
;; primitive - make-html
;; ---------------------------------------------------------

(define-css 'primitive "
div.primitive { display:inline-block;margin-right:10px;margin-top:4px;
                margin-bottom:4px;padding:0px; }
div.primitive-box { border:1px solid #008; display:inline-block;}
div.primitive div.primitive-title  { 
  padding:0px;padding-left:3px;padding-right:3px;
  white-space:nowrap; background-color:#008; }
div.primitive div.primitive-title > a {color:#fff;}
div.primitive div.primitive-title > span {color:#fff;}
table.primitive { border-collapse:collapse; }
table.primitive td.primitive-type { font-style:italic;padding:0px;padding-left:4px;}
table.primitive td.primitive-details div.primitive-detail { 
  padding-left:4px; padding-right:4px;padding-bottom:1px;padding-top:1px;
  border-bottom:1px dashed #563;text-align:left;  }
table.primitive td.primitive-details > div { overflow:hidden; }
table.primitive td.primitive-details div.primitive-detail:first-child { border-top:none;} 
")

(defmethod collapsed-primitive-html ((primitive primitive) element-id)
  `((div :class "primitive-box")
    ((div :class "primitive-title")
     ((a ,@(make-expand/collapse-link-parameters 
            element-id t "expand primitive")
         :name ,(mkstr (id primitive)))
      ,(format nil "~(~a~)" (id primitive))))))

(defmethod expanded-primitive-html ((primitive primitive) element-id)
  (lambda ()
    `((div :class "primitive-box")
      ((div :class "primitive-title")
       ((a ,@(make-expand/collapse-link-parameters
              element-id nil 
              "collapse primitive")
           :name ,(mkstr (id primitive)))
        ,(format nil "~(~a~)" (id primitive))))
      ((table :class "primitive") 
       ((tr)
        ((td :class "primitive-details")
         ((div :class "primitive-detail")
          ,(html-pprint (loop for slot-spec in (slot-specs primitive)
                              collect (list (slot-spec-name slot-spec)
                                            (slot-spec-type slot-spec)))))))
       ,@(loop for eval-spec in (evaluation-specs primitive)
               collect `((tr)
                         ((td :class "primitive-details")
                          ,(html-pprint (pattern eval-spec)))))))))

(defmethod make-html ((primitive primitive) &key (expand-initially nil)
                      (expand/collapse-all-id (make-id 'prim)))
  `((div :class "primitive")
    ,(let ((element-id (make-id (id primitive))))
       (make-expandable/collapsable-element 
        element-id expand/collapse-all-id
        ;; collapsed version
        (collapsed-primitive-html primitive element-id)
        ;; expanded version
        (expanded-primitive-html primitive element-id)
        :expand-initially expand-initially))))

;; #########################################################
;; ontology - make-html
;; ---------------------------------------------------------

(define-css 'ontology "
div.ontology { display:inline-block;margin-right:10px;margin-top:4px;
               margin-bottom:4px;padding:0px; }
div.ontology-box { border:1px solid; display:inline-block;}
div.ontology div.ontology-title  { 
  padding:0px;padding-left:3px;padding-right:3px;white-space:nowrap; }
div.ontology div.ontology-title > a { color:#40241A;font-weight:bold; }
table.ontology { border-collapse:collapse; }
table.ontology td.ontology-type { font-style:italic;padding:0px;padding-left:4px;}
table.ontology td.ontology-details div.ontology-detail { 
  padding-left:4px; padding-right:4px;padding-bottom:1px;padding-top:1px;
  border-top:1px dashed #563;text-align:left;  }
table.ontology td.ontology-details > div { overflow:hidden; }
table.ontology td.ontology-details div.ontology-detail:first-child { border-top:none;} 
")

(defmethod collapsed-ontology-html ((ontology blackboard) element-id)
  `((div :class "ontology-title")
    ((a ,@(make-expand/collapse-link-parameters
           element-id t "expand ontology"))
     ,(format nil "IRL ONTOLOGY (~a)"
              (length (fields ontology))))))

(defmethod expanded-ontology-html ((ontology blackboard) element-id)
  (lambda ()
    `((div :class "ontology-box")
      ((div :class "ontology-title"
            :style "border-bottom:1px dashed;")
       ((a ,@(make-expand/collapse-link-parameters
              element-id nil "collapse ontology"))
        ,(format nil "IRL ONTOLOGY (~a)"
                 (length (fields ontology)))))
      ((table :class "ontology two-col")
       ,@(loop for (label . value) in (data-fields ontology)
               collect `((tr)
                         ((td :class "ontology-details")
                          ,(format nil "~(~a~)" label))
                         ((td :class "ontology-details")
                          ,(cond ((typep value 'entity)
                                  (make-html value))
                                 ((listp value)
                                  `((table :class "ontology")
                                    ((tr)
                                     ,@(loop for v in value
                                             when (typep v 'entity)
                                             collect `((td :class "ontology-details")
                                                       ,(make-html v))))))))))))))

;; We assume the ontology is a blackboard or at least a subclass thereof
;; and that the values of the data fields contain entities or lists of entities
(defmethod make-html ((ontology blackboard)
                      &key (expand-initially nil)
                      (expand/collapse-all-id (make-id 'ontology)))
  `((div :class "ontology")
    ,(let ((element-id (make-id 'ontology)))
       (make-expandable/collapsable-element
        element-id expand/collapse-all-id
        ;; collapsed version
        (collapsed-ontology-html ontology element-id)
        ;; expanded version
        (expanded-ontology-html ontology element-id) 
        :expand-initially expand-initially))))

;; #########################################################
;; irl-program-processor-node - make-html
;; ---------------------------------------------------------

(define-css 'ippn "
div.ippn { display:inline-block;margin-right:0px;margin-top:10px;
           margin-bottom:10px;padding:0px; }
div.ippn-box { border:1px solid #562; display:inline-block;}
div.ippn div.ippn-title  { 
  padding:0px;padding-left:3px;padding-right:3px;
  white-space:nowrap; background-color:#562; }
div.ippn div.ippn-title > a {color:#fff;}
div.ippn div.ippn-title > span {color:#fff;}
table.ippn {
  border-collapse:collapse; }
table.ippn td.ippn-type { font-style:italic;padding:0px;padding-left:4px;}
table.ippn td.ippn-details { vertical-align:top;padding-top:3px;padding-bottom:3px;
  padding-left:5px;padding-right:5px; }
table.ippn td.ippn-details div.ippn-detail { 
  padding-left:4px; padding-right:4px;padding-bottom:1px;padding-top:1px;
  border-top:1px dashed #563;text-align:left; }
table.ippn td.ippn-details > div { overflow:hidden; }
table.ippn td.ippn-details div.ippn-detail:first-child { border-top:none; }
div.ippn-hidden-subtree { padding:0px;margin:0px;padding:0px;margin-bottom:2px; }
")

(defun new-binding-p (child-binding parent-node)
  "Check if the value of the binding has changed between parent and child"
  (when parent-node
    (let ((parent-binding (find (var child-binding) (bindings parent-node) :key #'var)))
      (if (typep (value child-binding) 'entity)
        (not (equal-entity (value child-binding) (value parent-binding)))
        (not (eql (value child-binding) (value parent-binding)))))))

(defparameter *irl-program-processor-node-status-colors*
  '((initial . "#444")
    (evaluated . "#480")
    (solution . "#050")
    (inconsistent . "#822")
    (bad-node . "#822")
    (evaluated-w/o-result . "#337")
    (duplicate . "#520")
    (max-depth-reached . "#888")
    (max-nr-of-nodes . "#888")
    (not-evaluated . "#444")))

(defun ippn->title-text (node)
  (if (eq (status node) 'initial)
    "initial"
    (format nil "~a"
            (downcase
             (mkstr
              (car (primitive-under-evaluation node)))))))

(defun ippn-title-html (node element-id node-color &key expand)
  `((div :class "ippn-title"
         :style ,(mkstr "background-color:" node-color ";"
                        (if (eql (status node) 'solution)
                          "font-weight:bold" "")))
    ((a ,@(make-expand/collapse-link-parameters element-id expand))
     ((span) ,(ippn->title-text node)))))

(defmethod collapsed-ippn-html ((node irl-program-processor-node) element-id node-color)
  `((div :class "ippn-box")
    ,(ippn-title-html node element-id node-color :expand t)))

(defmethod expanded-ippn-html ((node irl-program-processor-node) element-id node-color
                               &key (expand-initially nil)
                               (expand/collapse-all-id (make-id 'ippn)))
  (lambda ()
      (multiple-value-bind (new-bindings existing-bindings unbound)
          (loop for b in (bindings node)
                if (null (value b)) collect b into unbound
                else if (new-binding-p b (parent node)) collect b into new
                else collect b into existing
                finally (return (values new existing unbound)))
        `((div :class "ippn-box")
          ,(ippn-title-html node element-id node-color :expand nil)
          ((table :class "ippn")
           ;; status
           ((tr :style ,(format nil "border-bottom:1px solid ~a" node-color))
            ((td :class "ippn-details") "status")
            ((td :class "ippn-details")
             ((span :style ,(mkstr "color:" node-color ";"))
              ,(downcase (mkstr (status node))))))
           ;; new bindings
           ((tr)
            ((td :class "ippn-details") "new bindings")
            ((td :class "ippn-details")
             ((table :class "ippn")
              ,@(if new-bindings
                  (loop for b in new-bindings
                        collect `((tr)
                                  ((td :class "ippn-details")
                                   ((div :class "binding") ,(html-pprint (var b))))
                                  ((td :class "ippn-details")
                                   ((div :class "binding")
                                    ,(if (value b)
                                       (make-html (value b) :expand-initially expand-initially
                                                  :expand/collapse-all-id expand/collapse-all-id)
                                       '((div :class "unbound-value") "unbound"))))))
                  `(((span) "&#8709;"))))))
           ;; existing bindings
           ((tr)
            ((td :class "ippn-details") "existing bindings")
            ((td :class "ippn-details")
             ((table :class "ippn")
              ,@(if existing-bindings
                  (html-hide-rest-of-long-list
                   existing-bindings 3
                   (lambda (b)
                     `((tr)
                       ((td :class "ippn-details")
                        ((div :class "binding") ,(html-pprint (var b))))
                       ((td :class "ippn-details")
                        ((div :class "binding")
                         ,(if (value b)
                            (make-html (value b))
                            '((div :class "unbound-value") "unbound")))))))
                  `(((span) "&#8709;"))))))
           ;; unbound
           ((tr)
            ((td :class "ippn-details") "unbound")
            ((td :class "ippn-details")
             ((table :class "ippn")
              ,@(if unbound
                  (html-hide-rest-of-long-list
                   unbound 3
                   (lambda (b)
                     `((tr)
                       ((td :class "ippn-details")
                        ((div :class "binding") ,(html-pprint (var b))))
                       ((td :class "ippn-details")
                        ((div :class "binding")
                         ,(if (value b)
                            (make-html (value b))
                            '((div :class "unbound-value") "unbound")))))))
                  `(((span) "&#8709;")))))))))))
  
(defun collapsed-hidden-subtree-html (element-id)
  `((div :class "ippn-hidden-subtree")
    ((a ,@(make-expand/collapse-link-parameters element-id t "expand subtree"))
     "+")))

(defun expanded-hidden-subtree-html (hidden-children element-id
                                     &key (expand/collapse-all-id (make-id 'subtree)))
  (draw-node-with-children
   `((div :class "ippn-hidden-subtree")
     ((a ,@(make-expand/collapse-link-parameters element-id nil "collapse subtree"))
      "-"))
   (loop for child in hidden-children
         collect (make-html child :expand/collapse-all-id expand/collapse-all-id))))
                                                  
(defun on-path-to-target-p (node targets)
  (when targets
    (loop for target in targets
          when (or (eql node target)
                   (member node (parents target)))
          return t)))

(defmethod make-html ((node irl-program-processor-node)
                      &key targets (expand-initially nil)
                      (expand/collapse-all-id (make-id 'ippn)))
  (let* ((element-id (make-id 'ippn))
         (node-color
          (or (assqv (status node) *irl-program-processor-node-status-colors*)
              (error "no status color defined for status ~a" (status node)))))
    (draw-node-with-children
     `((div :class "ippn")
       ,(make-expandable/collapsable-element
         element-id (make-id)
         ;; collapsed element
         (collapsed-ippn-html node element-id node-color)
         ;; expanded element
         (expanded-ippn-html node element-id node-color
                             :expand/collapse-all-id expand/collapse-all-id)))
     (let ((subtree-id (make-id 'subtree))
           nodes-to-show nodes-to-hide)
       (if targets
         (loop for child in (children node)
               if (on-path-to-target-p child targets)
               do (push child nodes-to-show)
               else do (push child nodes-to-hide))
         (setf nodes-to-show (children node)))
       (shuffle (append
        (loop for child in nodes-to-show
              collect (make-html child :targets targets
                                 :expand-initially expand-initially
                                 :expand/collapse-all-id expand/collapse-all-id))
        (if nodes-to-hide
          (list 
           (make-expandable/collapsable-element
            subtree-id expand/collapse-all-id
            ;; collapsed element
            (collapsed-hidden-subtree-html subtree-id)
            ;; expanded element
            (expanded-hidden-subtree-html nodes-to-hide subtree-id
                                          :expand/collapse-all-id expand/collapse-all-id)
            :expand-initially expand-initially))
          nil))))
     :color "#aaa")))

(defmethod make-html ((processor irl-program-processor)
                      &key (expand/collapse-all-id (make-id 'ipp))
                      (expand-initially nil))
  (let ((solution-nodes
         (find-all 'solution (nodes processor) :key #'status))
        (deepest-inconsistent-node
         (the-biggest #'node-depth (find-all 'inconsistent (nodes processor) :key #'status))))
    ;; when drawing the search tree, only show the path to the solution
    ;; or the deepest inconsistent node when there is no solution
    (make-html (top processor) :targets (or solution-nodes
                                            (when deepest-inconsistent-node
                                              (list deepest-inconsistent-node)))
               :expand/collapse-all-id expand/collapse-all-id
               :expand-initially expand-initially)))

(defun make-collapsed-html-for-irl-evaluation-process (processor element-id)
  `((table :class "two-col")
    ((tbody)
     ((tr)
      ((td)
       ((a ,@(make-expand/collapse-link-parameters element-id t "evaluation process"))
        "evaluation process"))
      ((td)
       ((div :style "margin-top:-7px")
        ,(make-html processor :expand-initially nil)))))))

(defun make-expanded-html-for-irl-evaluation-process (processor element-id)
  `((table :class "two-col")
    ((tbody)
     ((tr)
      ((td)
       ((a ,@(make-expand/collapse-link-parameters element-id nil "evaluation process"))
        "evaluation process"))
      ((td)
       ((div :style "margin-top:-7px")
        ,(make-html processor :expand-initially t)))))))
       

(defmethod make-html-for-irl-evaluation-process ((processor irl-program-processor))
  (let ((element-id (make-id 'evaluation-process)))
    (make-expandable/collapsable-element
     element-id (make-id)
     (make-collapsed-html-for-irl-evaluation-process processor element-id)
     (make-expanded-html-for-irl-evaluation-process processor element-id))))


;; #########################################################
;; make-tr-for-irl-program
;; ---------------------------------------------------------

(defun make-tr-for-irl-program (caption irl-program)
  "Makes a table row with caption as the first cell and the s-expression
   and a graph of irl-program as the second cell"
  `((tr)
    ((td) ,caption)
    ((td) ,(html-pprint irl-program :max-width 100)
     ,(if (loop for x in irl-program
                if (eq (car x) 'bind) count x into number-of-bind-statements
                else count x into number-of-primitives
                finally (return (or (> number-of-primitives 1)
                                    (and (= number-of-primitives 1)
                                         (> number-of-bind-statements 0)))))
        (irl-program->svg irl-program)
        ""))))

;; #########################################################
;; chunk - make-html
;; ---------------------------------------------------------

(define-css 'chnk "
div.chnk { border:1px solid #543; display:inline-block; margin-right:5px; margin-bottom:5px;}
div.chnk-title { position:relative;background-color:#543;padding:1px;
                 padding-left:3px;padding-right:3px; }
div.chnk-title > a { color:white; }
div.chnk-title > div.save-button { position:absolute;right:5px;top:1px;display:none;}
div.chnk-title > div.save-button > a { color:#fff; }
div.chnk table.chnk-details { margin:3px;margin-bottom:-5px;}
div.chnk .chnk-details img { margin:-15px;margin-top:0px;margin-right:0px; }
")

;; keeps each chunk that is drawn in a hash table for potential saving
(defvar *saveable-chunks* (make-hash-table :test #'equal))

;; empty the hash table when the 'reset' button is pushed
(defun reset-saveable-chunks ()
  (setf *saveable-chunks* (make-hash-table :test #'equal)))

(pushnew #'reset-saveable-chunks wi::*reset-functions*)

;; this is the variable that the chunk is saved to
(defvar *saved-chunk* nil)

(export '*saved-chunk*)

(ht-simple-ajax:defun-ajax save-chunk (id) (wi::*ajax-processor*)
  "Called from the html page to save a chunk to a global variable"
  (setf *saved-chunk* (gethash id *saveable-chunks*))
  (add-element `((hr)))
  (add-element `((p) "Saved chunk " ((div) ,(make-html *saved-chunk*))
		 " to global variable " ((tt) ((b) "*saved-chunk*"))))
  (render-xml nil))

(defmethod make-html ((chunk chunk) &key (expand/collapse-all-id (make-id 'chunk))
                      (expand-initially nil))
  (let ((chunk-div-id (make-id 'chunk))
        (title (format nil "~(~a~) (~,2f)" (id chunk) (score chunk)))
        (save-button-id (make-id 'chunk)))
    (make-expandable/collapsable-element 
     chunk-div-id expand/collapse-all-id
     ;; collapsed version
     `((div :class "chnk")
       ((div :class "chnk-title")
        ((a ,@(make-expand/collapse-link-parameters chunk-div-id t)) ,title)))
     ;; expanded version
     (lambda ()
       `((div :class "chnk"
              ;; on mouse over, show button to save chunk
              ,@(unless wi::*static-html* 
                  `(:onmouseover 
                    ,(mkstr "document.getElementById('" save-button-id
                            "').style.display = 'inline';")
                    :onmouseout 
                    ,(mkstr "document.getElementById('" save-button-id 
                            "').style.display = 'none';"))))
         ((div :class "chnk-title")
          ((a ,@(make-expand/collapse-link-parameters chunk-div-id nil)) ,title)
          ,@(unless wi::*static-html*
              (setf (gethash (format nil "~(~a~)" save-button-id) *saveable-chunks*)
                    chunk)
              `(((div :class "save-button" :id ,(mkstr save-button-id)) 
                 ((a :href ,(format nil "javascript:ajax_save_chunk('~(~a~)');" 
                                    save-button-id)
                     :title "save chunk") "save")))))
         ((table :class "chnk-details two-col")
          ((tbody)
           ,(if (target-var chunk)
              `((tr)
                ((td) "target var")
                ((td)
                 ((div :style "display:inline-block")
                  ,(html-pprint (car (target-var chunk))))
                 ,(format nil "  (~(~a~))" (cdr (target-var chunk)))))
              "")
           ,(if (open-vars chunk)
              `((tr)
                ((td) "open vars")
                ((td) 
                 ,@(loop for (id . type) in (open-vars chunk)
                         for i from 1
                         append `(((span :style "white-space:nowrap")
                                   ((div :style "display:inline-block")
                                    ,(html-pprint id))
                                   ,(format nil "  (~(~a~))" type)
                                   ,(if (< i (length (open-vars chunk)))
                                      "," ""))
                                  " "))))
              "")
           ,(make-tr-for-irl-program "irl program" (irl-program chunk))
           ((tr)
            ((td) "score")
            ((td) ,(format nil "~,2f" (score chunk))))))))
     :expand-initially expand-initially)))
