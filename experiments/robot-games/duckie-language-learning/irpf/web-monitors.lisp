(in-package :duckie-language-learning)

;; ---------------
;; + Web monitor +
;; ---------------

;; Diagnostic trigger
;; ------------------
(define-event diagnostic-trigger
  (node cip-node)
  (diagnostic diagnostic))

(define-event-handler (trace-fcg diagnostic-trigger)
  (add-element '((hr)))
  (let ((cip (cip node)))
    (add-element `((div)
                   ((table :class "two-col")
                    ((tbody)
                     ((tr)
                      ((td) "initial structure")
                      ((td) ,(make-html-fcg-light (initial-cfs cip)
                                                  :configuration nil
                                                  :feature-types (feature-types (original-cxn-set (construction-inventory cip))))))
                     ((tr)
                      ((td) "application process")
                      ((td) ((div :id ,(mkstr (make-id 'subtree-id)))
                             ,(make-html-fcg-light (top-node cip))))))))))
  (add-element `((h2) ((b :style "color:#E65C00") "Comprehension unsuccesful - jump to meta-layer"))))

;; Apply fix
;; ------------------
(define-event fix-applied (repair-name symbol) 
                          (form list) 
                          (learned-cxns list) 
                          (cip construction-inventory-processor) 
                          (categorial-network categorial-network) 
                          (new-links list))

(define-event-handler (trace-fcg fix-applied)
  (add-element '((hr :style "border-top: 3px dashed #E65C00;background-color:#fff")))
  (add-element '((h2) "Meta-layer: Pattern Finding"))
  (add-element `((h3) ,(format nil "Applied repair: ~a with form: \"~{~a~^ ~}\" and learned:"
                               repair-name
                               (render form :render-string-meets)
                               (mapcar #'(lambda (cxn) (name cxn)) learned-cxns))))
  (add-element `((div :class "indent-irpf")
                 ,@(html-hide-rest-of-long-list 
                    learned-cxns 5
                    #'(lambda (construction) 
                        (make-html construction :expand-initially nil
                                   :configuration (visualization-configuration (construction-inventory cip))
                                   :wrap-in-paragraph nil)))))
  (if new-links
    (progn
      (add-element '((h3) "New links are added to the categorial network:"))
      (add-element `((div :class "indent-irpf") ,(s-dot->svg (new-categorial-links->s-dot categorial-network new-links))))))
  (add-element '((hr :style "border-top: 3px dashed #E65C00;background-color:#fff"))))

(defun new-categorial-links->s-dot (categorial-network new-links)
  (let* ((g (fcg::graph categorial-network))
         (graph-properties '((s-dot::fontcolor "#000000")
                             (s-dot::fontsize "10.0")
                             (s-dot::fontname "Helvetica")
                             (s-dot::rankdir "LR")))
         (all-node-names
          (remove-duplicates
           (loop for (from . to) in new-links
                 append (list from to))))
         (all-node-ids
          (loop for node-name in all-node-names
                for id = (gethash node-name (graph-utils::nodes g))
                collect id))
         (all-edges
          (loop for (from . to) in new-links
                collect (cons (gethash from (graph-utils::nodes g))
                              (gethash to (graph-utils::nodes g)))))
         (s-dot-nodes
          (loop for node-name in all-node-names
                for node-id in all-node-ids
                collect (graph-utils::categorial-network-node->s-dot
                         node-name node-id)))
         (s-dot-edges
          (loop for (from-id . to-id) in all-edges
                for edge-weight = (graph-utils::edge-weight g from-id to-id)
                collect (graph-utils::categorial-network-edge->s-dot
                         from-id to-id
                         :weight edge-weight :directedp nil
                         :colored-edges-0-1 nil))))
    `(s-dot::graph ,graph-properties
                   ,@s-dot-nodes
                   ,@s-dot-edges)))