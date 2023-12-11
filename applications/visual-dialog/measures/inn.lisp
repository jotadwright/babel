(in-package :visual-dialog)

(defparameter *visual-dialog-inn* nil)
  
;; The network will store information about nodes and edges so we can export
;; that later for Gephi networks. The real action happens in the update slot,
;; where we will list changes to be made to the network. 
(defstruct integrative-network 
  (nodes (make-hash-table))
  (edges (make-hash-table))
  (bound-variables (make-hash-table))
  (update-nodes (make-hash-table))
  (update-edges (make-hash-table)))

;; For each node, we will keep track of its type, connected nodes, and whether it has been under 
;; the system's attention in the current narrative.
(defstruct integrative-network-node label type connected-nodes under-attention)


;; ------------------------------------------------------------------------------------------
;; 1. Identify network updates
;; ------------------------------------------------------------------------------------------
;; Generic function for identifying which updates are needed to the 
;; network, such as which nodes to add.
;; Each method returns either NIL or a list of objects that 
;; another method will handle (e.g. variables, entities, bindings, ...).

(defgeneric identify-network-updates (nodes network &key &allow-other-keys))

;; ------------------------------------------------------------------------------------------
;; (a) semantic networks: method called after an FCG parsing task.
;; ------------------------------------------------------------------------------------------

(defstruct semantic-network primitives bindings)

(defmethod identify-network-updates ((semantic-network semantic-network)
                                     (network integrative-network)
                                     &key &allow-other-keys)
  ;; This method is always called after parsing with FCG. 
  (if (null (semantic-network-primitives semantic-network)) ;; We handled all primitives
    nil
    ;; We handle the first primitive and recursively deal with the rest.
    (let* ((primitive (pop (semantic-network-primitives semantic-network)))
           (predicate (first primitive))
           (arguments (rest primitive))
           (predicate-id (make-const predicate)))
      (unless (eql 'bind (first primitive)) ;; Don't handle bind statements yet, that is done in interpretation
        ;; Add the predicate as a new node with new edges
        (setf (gethash predicate (integrative-network-update-nodes network)) (make-integrative-network-node
                                                                                 :label (mkstr predicate)
                                                                                 :type 'predicate
                                                                                 :connected-nodes arguments
                                                                                 :under-attention t)
              (gethash predicate (integrative-network-update-edges network)) arguments)
        ;; Handle the arguments
        (dolist (argument arguments)
          (identify-network-updates argument network 
                                    :under-attention? t
                                    :source predicate-id)))
      ;; Handle the other primitives:
      (identify-network-updates semantic-network network))))

;; ------------------------------------------------------------------------------------------
;; (b) accessible entities: the discourse model contains a list of accessible entities
;;                          consisting of an IRL-binding that binds a variable and an entity.
;; ------------------------------------------------------------------------------------------

(defmethod identify-network-updates ((node binding)
                                     (network integrative-network)
                                     &key exhaustive? &allow-other-keys)
  ;; An IRL-binding relates a variable to an entity.
  (let* ((node-id (get-object-id node)) ;; The variable name
         (value (slot-value node 'value)) ;; An entity
         (value-id (if value (get-object-id value))))
    ;; If the binding is already known, we only have to add the edge
   ; (if (gethash node-id (integrative-network-nodes network))
        #|(and (gethash node-id (integrative-network-nodes network))
             (eql 'answered-question (integrative-network-node-type 
                                      (gethash node-id (integrative-network-nodes network))))
             )|#
   ;   (progn 
   ;     (push value-id (integrative-network-node-connected-nodes (gethash node-id (integrative-network-nodes network))))
   ;     (push value-id (gethash node-id (integrative-network-update-edges network)))
   ;     (identify-network-updates value network :under-attention? t :exhaustive? exhaustive?))
      ;; Otherwise we add a new node and its edges to the update list
      (if value
        (progn
          (setf (gethash node-id (integrative-network-update-nodes network)) (make-integrative-network-node
                                                                              :label node-id
                                                                              :type 'answered-question
                                                                              :connected-nodes (list value-id)
                                                                              :under-attention t)
                (gethash node-id (integrative-network-update-edges network)) (list value-id))
          ;; Now handle the entity.
          (identify-network-updates value network :under-attention? t :exhaustive? exhaustive?))    
        (setf (gethash node-id (integrative-network-update-nodes network)) (make-integrative-network-node
                                                                            :label node-id
                                                                            :type 'open-question
                                                                            :under-attention t)))
   ;  )
    ;; Return NIL
    nil))

(defmethod identify-network-updates ((entity entity)
                                     (network integrative-network)
                                     &key exhaustive? (under-attention? nil) &allow-other-keys)
  ;; Most complex object to handle because there may be new edges, and 
  ;; values can be lists of entities.
  (let* ((node-id (get-object-id entity))
         (node (make-integrative-network-node :label node-id :type 'entity :under-attention under-attention?)))
    ;; If the node does not exist yet, add it to our update-list:
    (unless (gethash node-id (integrative-network-nodes network))
      (setf (gethash node-id (integrative-network-update-nodes network)) node)
      ;; When we want to search exhaustively, handle the edges as well:
      (if exhaustive?
        ;; Only handle edges when we search exhaustively:
        (let* ((slot-names (remove-if #'(lambda(x)
                                          (member x '(id persistent-id)))
                                      (get-slot-names entity)))
               (slot-values (loop for slot-name in slot-names
                                  for value = (slot-value entity slot-name)
                                  append (cond ((or (null value)
                                                    (eql 'none value)) (list (get-object-id value)))
                                               ((listp value)  value)
                                               (t
                                                (list value)))))
               (edges (mapcar #'get-object-id slot-values)))
          ;; Add edges so they will be checked if update is required.
          (setf (integrative-network-node-connected-nodes node) edges
                (gethash node-id (integrative-network-update-edges network)) edges)
          ;; Handle the other nodes:
          (identify-network-updates slot-values network :exhaustive? exhaustive? :under-attention? nil))
        ;;  Else return NIL:
        nil))))


(defmethod identify-network-updates ((nodes cons)
                                     (network integrative-network)
                                     &key exhaustive? &allow-other-keys)
  ;Identify network updates for cons, the links between the variables
  ;First replace in the edges network the old variable with the new one. Otherwise, the collect-edges will detect a change in the old and new meaning network, because the variable is replaced
  (loop for key being each hash-key of (integrative-network-edges network)
          using (hash-value v)
        do (setf (gethash key (integrative-network-edges network))
                 (substitute  (cdr nodes) (first nodes) v)))
  (identify-network-updates (first nodes) network :source (list (cdr nodes)) :open-question? nil)  
  (setf (gethash (first nodes) (integrative-network-update-edges network)) (list (cdr nodes))))

;; ------------------------------------------------------------------------------------------
;; (c) List: Intended to be used as handling a list of bindings (= accessible entities), 
;;           but can be used for any list.
;; ------------------------------------------------------------------------------------------
(defmethod identify-network-updates ((nodes list)
                                     (network integrative-network)
                                     &key exhaustive? &allow-other-keys)
  (dolist (node nodes)
    (identify-network-updates node network :exhaustive? exhaustive?)))

;; ------------------------------------------------------------------------------------------
;; (d) T and other atomic values: For any value of an entity's slot (except variables)
;; ------------------------------------------------------------------------------------------

(defmethod identify-network-updates ((object t)
                                     (network integrative-network)
                                     &key source (under-attention? nil) (open-question? t) &allow-other-keys)
  (let* ((node-id (get-object-id object))
         ;(existing-node (or (gethash node-id (integrative-network-update-nodes network)) 
         ;                   (gethash node-id (integrative-network-nodes network))))
         ;(existing-edges (if existing-node (integrative-network-node-connected-nodes existing-node)))
         existing-node
         existing-edges
         (node (make-integrative-network-node
                :label node-id
                :type (cond ((not open-question?) 'answered-question)
                            ((and existing-node (variable-p node-id)) 'answered-question)
                            ((variable-p node-id) 'open-question)
                            (t
                             'value))
                :connected-nodes (if source
                                   (union (list source) existing-edges)
                                   existing-edges)
                :under-attention under-attention?)))
    ;; Add (or replace) the node to the list of updates:
    (setf (gethash node-id (integrative-network-update-nodes network)) node)
    ;; We have reached a terminal node so return NIL.
    nil))


(defun get-node-shape (type)
  (case type
    (open-question "diamond")
    (answered-question "diamond")
    (entity "hexagon")
    (predicate "triangle")
    (t
     "square")))

(defun get-node-color (type)
  (case type
    (open-question "red")
    (answered-question "green")
    (entity "cyan")
    (predicate "purple")
    (t
     "grey")))

(defun format-node (id &key type label (opacity 1.0))
  "Format a node according to its type."
  (let ((shape (get-node-shape type))
        (color (get-node-color type)))
    (if label 
      (format nil "{ id: '~a', label: '~a', shape: '~a', color: '~a', opacity: ~a}" id label shape color opacity)
      (format nil "{ id: '~a', shape: '~a', color: '~a', opacity: ~a}" id shape color opacity))))



(defun handle-node-updates (integrative-network)
  "Handle nodes that need to be added or updated."
  (let* ((node-update-spec (integrative-network-update-nodes integrative-network))
         (nodes-to-be-added nil)
         (nodes-to-be-updated nil)) ;; Do nodes also need to be removed?
    (loop for key being the hash-keys in node-update-spec
          for node = (gethash key node-update-spec)
          for existing-node = (gethash key (integrative-network-nodes integrative-network))
          for formatted-node = (format-node key
                                            :label (integrative-network-node-label node)
                                            :type (integrative-network-node-type node)
                                            :opacity (if (integrative-network-node-under-attention node)
                                                       1.0
                                                       0.1))
          do (if existing-node
               (push formatted-node nodes-to-be-updated)
               (push formatted-node nodes-to-be-added))
             (setf (gethash key (integrative-network-nodes integrative-network)) node))
    ;; Reset updates:
    (setf (integrative-network-update-nodes integrative-network) (make-hash-table))
    ;; Return the formatted nodes:
    (values nodes-to-be-added nodes-to-be-updated)))


(defun remove-nodes (integrative-network)
  "Handle nodes that need to be added or updated."
  (let* ((nodes (integrative-network-nodes integrative-network))
         (edges (integrative-network-edges integrative-network))
         (nodes-to-be-removed nil)
         (edges-to-be-removed nil)) ;; Do nodes also need to be removed?
    (loop for key being the hash-keys in nodes
          for node = (gethash key nodes)
          for formatted-node = (format-node key
                                            :label (integrative-network-node-label node)
                                            :type (integrative-network-node-type node)
                                            :opacity (if (integrative-network-node-under-attention node)
                                                       1.0
                                                       0.1))
          do (push formatted-node nodes-to-be-removed))
    (loop for key being the hash-keys in edges
          do (loop for to-node in (gethash key edges)
                   ;; Format the edge and collect it
                   do (push (wi::format-vis-js-edge key to-node) edges-to-be-removed)))
    ;; Return the formatted nodes:
    (values nodes-to-be-removed edges-to-be-removed)))

(defun cons-new (e lst)
  (if (member e lst)
    lst
    (cons e lst)))

(defun collect-new-edges (integrative-network)
  "Edges are collected with filter for double-edges and formatted on the fly."
  (let* ((old-edges (integrative-network-edges integrative-network))
         (intermediate-edge-spec (integrative-network-update-edges integrative-network))
         (new-table (make-hash-table))
         (new-edge-spec (loop for edge-key being the hash-keys in intermediate-edge-spec
                              when (or
                                    (not (gethash edge-key old-edges))
                                       (and
                                        (gethash edge-key old-edges)
                                        (not (permutation-of? (gethash edge-key old-edges)
                                                              (gethash edge-key intermediate-edge-spec))))
                                       )
                                do (setf (gethash edge-key new-table) (gethash edge-key intermediate-edge-spec))))
         (already-handled nil)
         (new-edges (loop for key being the hash-keys in new-table
                           do (push key already-handled)
                           append (loop for to-node in (gethash key new-table)
                                        ;; Unless an edge in the other direction was already collected 
                                      ;  unless (and (member to-node already-handled)
                                      ;              (member key (gethash to-node new-table)))
                                          ;; Format the edge and collect it
                                          collect (wi::format-vis-js-edge key to-node)))))
    
    ;; Also add edges to the persistent network:
    (loop for key being the hash-keys in new-table
          do (loop for target in (gethash key new-table)
                   unless (member key (gethash target (integrative-network-edges integrative-network)))
                     do (setf (gethash key (integrative-network-edges integrative-network))
                              (cons-new target (gethash key (integrative-network-edges integrative-network))))))

    ;; Reset the update network:
    (setf (integrative-network-update-edges integrative-network) (make-hash-table))
    ;; Return the formatted edges:
    new-edges))




;; ------------------------------------------------------------------------------------------
;; Helper functions for accessing slots and information.
;; ------------------------------------------------------------------------------------------

(defun get-object-id (object)
  (cond ((null object) (make-var 'question))
        ((eql 'none object) (make-const 'none))
        ((or (symbolp object) (numberp object)) object)
        ((eql 'binding (type-of object)) (slot-value object 'variable))
        (t
         (id object))))

(defun get-slot-names (clos-object)
  "Retrieve the slot names associated with the class of a clos-object."  
  (mapcar #'closer-mop:slot-definition-name
          (closer-mop:class-slots (class-of clos-object))))


(defun initialize-values ()
  (setf (slot-value (monitors::get-monitor 'questions-introduced-by-grammar) 'values) nil
        (slot-value (monitors::get-monitor 'questions-solved-by-grammar) 'values) nil
        (slot-value (monitors::get-monitor 'questions-solved-by-mental-simulation) 'values) nil
        (slot-value (monitors::get-monitor 'node-names-fcg) 'values) nil
        (slot-value (monitors::get-monitor 'node-names-irl) 'values) nil))


(defun instantiate-meaning-network (network)
  (let ((vars (loop for primitive in network
                      collect (rest primitive)))
        (binds (loop for primitive in network
                     when (eq (first primitive) 'bind)
                       collect primitive)))
    (loop for var in vars
          for bind = (find var binds)
          when bind
            do (setf var (last-elt bind)))))


(defparameter *counter* 0)

(defun get-next-number ()
  (incf *counter*))


(defparameter *predicate-counter* 0)