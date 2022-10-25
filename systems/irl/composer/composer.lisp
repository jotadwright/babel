(in-package :irl)

;; #########################################
;; classes and methods
;; -----------------------------------------

(defclass chunk-composer (tree blackboard)
  ((primitive-inventory
    :type primitive-inventory :initarg :primitive-inventory
    :initform *irl-primitives* :accessor primitive-inventory
    :documentation "A primitive inventory")
   (ontology
    :type blackboard :initarg :ontology
    :initform (make-blackboard) :accessor ontology
    :documentation "The ontology")
   (configuration
    :type configuration :initarg :configuration
    :initform (make-config) :accessor configuration
    :documentation "The composer configuration")
   (chunks
    :initarg :chunks :accessor chunks :initform nil
    :documentation "The available chunks")
   (solutions
    :initarg :solutions :accessor solutions :initform nil
    :documentation "The composer solutions")
   (queue
    :initarg :queue :accessor queue :initform nil
    :documentation "The queue used in the search process")
   (topic
    :initarg :topic :accessor topic :initform nil
    :documentation "The thing the composer is looking for")
   (meaning
    :initarg :meaning :accessor meaning :initform nil
    :documentation "The meaning the composer starts from")
   (node-counter
    :initarg :node-counter :accessor node-counter
    :initform 0 :type number)))

  
(defmethod set-configuration ((composer chunk-composer) key value
                              &key (replace t))
  (set-configuration (configuration composer)
                     key value :replace replace))

(defmethod get-configuration ((composer chunk-composer) key &key)
  (get-configuration (configuration composer) key))

(defmethod copy-object ((composer chunk-composer))
  composer)

;; #########################################
;; make composer
;; -----------------------------------------

(defmethod initialize-instance :after ((composer chunk-composer) &key)
  ;; add hashed-nodes to the composer blackboard
  (set-data composer 'hashed-nodes (make-hash-table))
  ;; max depth for composer search process
  (set-configuration composer :max-search-depth 10)
  ;; How to arange the queue
  (set-configuration composer :queue-mode :best-first)
  ;; A list of functions that are called on the chunk
  ;; of each search node to create new chunks.
  (set-configuration composer :chunk-expansion-modes
                     '(:combine-program))
  ;; A list of functions that is called whenever a new
  ;; node is created to filter out bad nodes.
  (set-configuration composer :chunk-node-tests
                     '(:restrict-search-depth
                       :check-duplicate))
  ;; A function that is called whenever a new node is
  ;; created to rate the node. Returns a float.
  ;; lower == better
  (set-configuration composer :node-cost-mode
                     :short-programs-with-few-primitives-and-open-vars)
  ;; Computes a score for a chunk, a float
  ;; between 0 (very bad) and 1 (very good).
  (set-configuration composer :chunk-score-mode
                     :source-chunks-average)
  ;; A function that is called before chunk evaluation
  ;; to add things to the irl program. Gets the chunk
  ;; and returns a new chunk or nil when the wrapping
  ;; failed.
  (set-configuration composer :chunk-wrapper :identity)
  ;; A list of functions that are called after chunk
  ;; evaluation. Returns t when the result is a good  one
  (set-configuration composer :chunk-evaluation-goal-tests '(:identity))
  ;; A function that is called after chunk evaluation
  ;; to compute a score for a result (higher == better).
  (set-configuration composer :chunk-evaluation-score-mode
                     :chunk-and-binding-score-with-few-duplicates))


(defclass chunk-composer-node (tree-node blackboard)
  ((composer
    :initarg :composer :accessor composer
    :documentation "Pointer to the composer")
   (statuses
    :type list :initarg :statuses
    :initform nil :accessor statuses
    :documentation "The statuses of the node")
   (next-handler
    :type symbol :initarg :next-handler
    :initform 'match :accessor next-handler
    :documentation "The next handler of the node")
   (source-chunks
    :type list :initarg :source-chunks
    :accessor source-chunks :initform nil
    :documentation "The source chunks of the node")
   (chunk
    :type chunk :initarg :chunk :accessor chunk
    :documentation "The current chunk of the node")
   (chunk-evaluation-results
    :type list :accessor cers :initform nil
    :accessor chunk-evaluation-results
    :documentation "The evaluation results of this node")
   (created-at
    :documentation "The number of the node" :initform -1
    :accessor created-at :initarg :created-at :type number)
   (depth
    :documentation "The depth of the node"
    :accessor depth :initarg :depth :type number)
   (cost
    :documentation "The cost of the node; lower == better"
    :accessor cost :initarg :cost :type float)))

(export '(make-chunk-composer))

(defun make-chunk-composer (&key topic meaning initial-chunk chunks
                                 configurations ontology
                                 (primitive-inventory *irl-primitives*))
  ;; check the input
  (when (and (null topic) (null meaning))
    (error "Must provide either :topic or :meaning"))
  (unless (or chunks (get-data ontology 'chunks))
    (error "Composer has no chunks to work with. Either provide :chunks
            or place 'chunks in the ontology"))
  ;; initialise the initial chunk, the composer and the initial node
  (let* ((initial-chunk
          (if initial-chunk initial-chunk
            (if topic
              (make-instance 'chunk
                             :target-var `(?topic . ,(type-of topic))
                             :open-vars `((?topic . ,(type-of topic))))
              (make-instance 'chunk))))
         (composer
          (make-instance 'chunk-composer :topic topic :meaning meaning
                         :primitive-inventory primitive-inventory
                         :ontology (if ontology ontology (make-blackboard))
                         :chunks (if chunks chunks (get-data ontology 'chunks))))
         (initial-node
          (make-instance 'chunk-composer-node
                         :chunk initial-chunk
                         :composer composer
                         :statuses '(initial)
                         :created-at 0 :depth 0 :cost 0)))
    (set-configurations composer configurations)
    (cond (meaning (setf (next-handler initial-node) 'match))
          (topic (setf (next-handler initial-node) 'expand)))
    ;; start the queue
    (add-node composer initial-node)
    (push initial-node (queue composer))
    composer))

(defmethod get-configuration ((node chunk-composer-node) key &key)
  (get-configuration (composer node) key))

(defmethod print-object ((node chunk-composer-node) stream)
  (if *print-pretty*
    (pprint-logical-block (stream nil)
      (format stream "<chunk-composer-node ~a, ~a,~:_ statuses: ~(~a~),~:_>"
              (created-at node) (cost node) (statuses node)))
    (format stream "<chunk-composer-node ~a, ~a,~:_ statuses: ~(~a~),~:_>"
              (created-at node) (cost node) (statuses node))))

(defmethod copy-object ((node chunk-composer-node))
  node)