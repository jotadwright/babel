(in-package :irl-2)

;; #########################################
;; classes and methods
;; -----------------------------------------

(defclass chunk-composer (tree blackboard)
  ((primitive-inventory :type primitive-inventory :initarg :primitive-inventory
                        :initform *irl-primitives* :accessor primitive-inventory
                        :documentation "A primitive inventory")
   (primitive-inventory-configurations
    :type (or null configuration) :initarg :primitive-inventory-configurations
    :initform (make-config) :accessor primitive-inventory-configurations
    :documentation "A set of configurations for the primitive inventory
                    that are local to the composer, i.e. they will only
                    be used when evaluating composer nodes")
   (ontology :type blackboard :initarg :ontology
             :initform (make-blackboard) :accessor ontology
             :documentation "The ontology")
   (configuration :type configuration :initarg :configuration
                  :initform (make-config) :accessor configuration
                  :documentation "The composer configuration")
   (chunks :initarg :chunks :accessor chunks :initform nil
           :documentation "The available chunks")
   (solutions :initarg :solutions :accessor solutions :initform nil
              :documentation "The composer solutions")
   (queue :initarg :queue :accessor queue :initform nil
          :documentation "The queue used in the search process")
   (topic :initarg :topic :accessor topic :initform nil
          :documentation "The thing the composer is looking for")
   (meaning :initarg :meaning :accessor meaning :initform nil
            :documentation "The meaning the composer starts from")
   (node-counter :initarg :node-counter :accessor node-counter
                 :initform 0 :type number)))

  
(defmethod set-configuration ((composer chunk-composer) key value
                              &key (replace t))
  (set-configuration (configuration composer)
                     key value :replace replace))


(defmethod get-configuration ((composer chunk-composer) key &key)
  (get-configuration (configuration composer) key))


(defclass chunk-composer-node (tree-node blackboard)
  ((composer :initarg :composer :accessor composer
             :documentation "Pointer to the composer")
   (statuses :type list :initarg :statuses
             :initform '(initial) :accessor statuses
             :documentation "The statuses of the node")
   (next-handler :type symbol :initarg :next-handler
                 :initform 'match :accessor next-handler
                 :documentation "The next handler of the node")
   (source-chunks :type list :initarg :source-chunks
                  :accessor source-chunks :initform nil
                  :documentation "The source chunks of the node")
   (chunk :type chunk :initarg :chunk :accessor chunk
          :documentation "The current chunk of the node")
   (chunk-evaluation-results :type list :accessor cers :initform nil
                             :accessor chunk-evaluation-results
                             :documentation "The evaluation results of this node")
   (node-number :documentation "The number of the node"
                :accessor node-number :initarg :node-number
                :type number :initform 0)
   (node-depth :documentation "The depth of the node"
               :accessor node-depth :initarg :node-depth
               :type number :initform 0)
   (node-rating :documentation "The rating of the node; lower == better"
                :accessor node-rating :initarg :node-rating
                :type float :initform 1.0)))


;; #########################################
;; make composer
;; -----------------------------------------

(defmethod initialize-instance :after ((composer chunk-composer) &key)
  ;; add hashed-nodes to the composer blackboard
  (set-data composer 'hashed-nodes (make-hash-table))
  ;; max depth for composer search process
  (set-configuration composer :max-search-depth 10)
  ;; A list of functions that are called on the chunk
  ;; of each search node to create new chunks.
  (set-configuration composer :expand-chunk-modes
                     '(:combine-program))
  ;; A list of functions that is called whenever a new
  ;; node is created to filter out bad nodes.
  (set-configuration composer :check-node-modes
                     '(:check-duplicate))
  ;; A function that is called whenever a new node is
  ;; created to rate the node. Returns a float.
  ;; lower == better
  (set-configuration composer :node-rating-mode
                     :short-programs-with-few-primitives-and-open-vars)
  ;; Computes a score for a chunk, a float
  ;; between 0 (very bad) and 1 (very good).
  (set-configuration composer :chunk-scoring-mode :source-chunks-average)
  ;; A function that is called before chunk evaluation
  ;; to add things to the irl program. Gets the chunk
  ;; and returns a new chunk or nil when the wrapping
  ;; failed.
  (set-configuration composer :chunk-wrapper-mode :identity)
  ;; A list of functions that are called after chunk
  ;; evaluation. Returns t when the result is a good
  ;; one
  (set-configuration composer :check-chunk-evaluation-result-modes
                     '(:identity))
  ;; A function that is called after chunk evaluation
  ;; to compute a score for a result (higher == better).
  (set-configuration composer :chunk-evaluation-result-scoring-mode
                     :chunk-and-binding-score-with-few-duplicates))


(export '(make-chunk-composer))

(defun make-chunk-composer (&key topic meaning initial-chunk chunks
                                 configurations ontology
                                 (primitive-inventory *irl-primitives*)
                                 primitive-inventory-configurations)
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
         (pi-configurations
          (if primitive-inventory-configurations
            (make-configuration :entries primitive-inventory-configurations)
            (make-config)))
         (composer
          (make-instance 'chunk-composer :topic topic :meaning meaning
                         :primitive-inventory primitive-inventory
                         :ontology (if ontology ontology (make-blackboard))
                         :chunks (if chunks chunks
                                   (get-data ontology 'chunks))
                         :primitive-inventory-configurations pi-configurations))
         (initial-node
          (make-instance 'chunk-composer-node :chunk initial-chunk
                         :composer composer)))
    (loop for configuration in configurations
          do (set-configuration composer (first configuration)
                                (rest configuration)))
    (cond (meaning (setf (next-handler initial-node) 'match))
          (topic (setf (next-handler initial-node) 'expand)))
    ;; start the queue
    (add-node composer initial-node)
    (push initial-node (queue composer))
    composer))