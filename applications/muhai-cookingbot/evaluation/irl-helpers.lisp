(in-package :irl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IRL NODE HELPER FUNCTIONS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(get-predicate-name get-output-binding get-output-value
          get-output-kitchen-state get-full-node-sequence))

(defmethod get-predicate-name ((irl-node pip-node))
  "Get the predicate name belonging to this node's primitive under evaluation."
  (first (primitive-under-evaluation irl-node)))

(defmethod get-output-binding ((irl-node pip-node))
  "Get the binding belonging to the primary output of this node's primitive under evaluation."
  (let ((target-var (second (primitive-under-evaluation irl-node)))
        (list-of-bindings (bindings irl-node)))
    (find target-var list-of-bindings :key #'var)))  

(defmethod get-output-value ((irl-node pip-node))
  "Get the value belonging to the binding for the primary output of this node's primitive under evaluation."
  (let ((target-binding (get-output-binding irl-node)))
    (when target-binding
      (value target-binding))))

(defmethod get-output-kitchen-state ((irl-node pip-node))
  "Get the output kitchen state belonging to this node's primitive under evaluation."
  (let ((all-vars (all-variables (list (primitive-under-evaluation irl-node))))
        (list-of-bindings (bindings irl-node)))
    (loop for var in all-vars
          for binding = (find var list-of-bindings :key #'var)
          if (eql (type-of (value binding)) 'muhai-cookingbot::kitchen-state)
            do (return (value binding)))))

(defmethod get-full-node-sequence ((irl-node pip-node))
  "Get the full sequence of nodes that led to the given IRL node.
   Sequence goes from the starting node with the first executed primitive up to and including the given IRL node."
  (let ((node irl-node)
        (node-seq '()))
    (loop do
         (push node node-seq)
         (setf node (parent node))
       while node)
    (rest node-seq)))