(in-package :irl-2)
                                     
;; ############################################################################
;; irl-program-processor
;; ----------------------------------------------------------------------------

(defclass irl-program-processor (tree)
  ((irl-program :documentation "The irl-program being processed"
                :accessor irl-program :initarg :irl-program
                :initform nil)
   (queue :documentation "The queue that drives the search process"
          :accessor queue :initarg :queue :initform nil)
   (solutions :documentation "Solutions, i.e. lists of lists of bindings"
              :accessor solutions :initarg :solutions
              :initform nil)
   (node-counter :documentation "Counter for the number of nodes"
                 :accessor node-counter :initarg :node-counter
                 :initform 0 :type number)
   (configuration :type configuration :initform (make-config)
                  :initarg :configuration :accessor configuration
                  :documentation "Configuration of the primitive inventory is copied over")
   (primitives :type list :initform nil :initarg :primitives :accessor primitives
               :documentation "The list of primitives")
   (ontology :type blackboard :initform (make-blackboard)
             :initarg :ontology :accessor ontology
             :documentation "The ontology used during processing")
   (blackboard :type blackboard :initform (make-blackboard)
               :initarg :blackboard :accessor blackboard
               :documentation "A blackboard to store data"))
  (:documentation "The IPP handles the evaluation of an irl program"))


(defclass irl-program-processor-node (tree-node)
  ((status :documentation "Status of the node."
           :accessor status :initarg :status :initform 'initial :type symbol)
   (bindings :documentation "Available bindings at this point in processing"
             :accessor bindings :initarg :bindings)
   (primitive-under-evaluation
    :documentation "The primitive being evaluated in this node"
    :accessor primitive-under-evaluation
    :initarg :primitive-under-evaluation :initform nil)
   (primitives-evaluated
    :documentation "List of evaluated primitives"
    :accessor primitives-evaluated
    :initarg :primitives-evaluated :initform nil)
   (primitives-remaining
    :documentation "List of remaining primitives"
    :accessor primitives-remaining
    :initarg :primitives-remaining :initform nil)
   (irl-program-processor
    :documentation "A pointer to the processor"
    :accessor processor :initarg :processor
    :initform nil)
   (node-number
    :documentation "The number of the node"
    :accessor node-number :initarg :node-number
    :type number :initform 0)
   (node-depth
    :documentation "The depth of the node"
    :accessor node-depth :initarg :node-depth
    :type number :initform 0)))


(defun make-irl-program-processor (irl-program ontology primitive-inventory)
  (make-instance 'irl-program-processor :irl-program irl-program
                 :configuration (configuration primitive-inventory)
                 :primitives (primitives primitive-inventory)
                 :ontology ontology))

(defmethod all-parents ((node irl-program-processor-node))
  "Returns all parents of the current node, thus not including
   the current node itself. The parents are sorted with the
   most recent node first, i.e. the initial node is always last"
  (labels ((collect-parents (node)
             (if (parent node)
               (append (list node) (collect-parents (parent node)))
               (list node))))
    (when (parent node)
      (collect-parents (parent node)))))
      