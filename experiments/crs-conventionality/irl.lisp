(in-package :crs-conventionality)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  Defining primitives and composing programs  ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-irl-primitives naming-game-primitives
  :primitive-inventory *naming-game-primitives*)

(def-irl-primitives concept-emergence-game-primitives
  :primitive-inventory *concept-emergence-game-primitives*)


(defprimitive retrieve-from-scene ((entity-in-scene naming-game-entity)
                                   (entity-from-grammar naming-game-entity)
                                   (scene crs-conventionality-entity-set))
  
  ;; when entity-from-grammar and scene are given, retrieve entity-in-scene
  ((entity-from-grammar scene => entity-in-scene)
   (let ((retrieved-entity (find (id entity-from-grammar) (entities scene) :key #'id)))
     (when retrieved-entity
       (bind (entity-in-scene 1.0 retrieved-entity)))))
  
  ;; when entity-from-grammar, entity-in-scene and scene are given, check for consistency
  ((entity-from-grammar entity-in-scene scene =>)
   (equal-entity (find (id entity-from-grammar) (entities scene) :key #'id)
                 entity-in-scene))

  ((scene => entity-from-grammar entity-in-scene)
   (loop for object in (entities scene)
         for object-in-grammar = (make-instance 'naming-game-entity :id (id object))
         collect (bind (entity-in-scene 1.0 object)
                       (entity-from-grammar 1.0 object-in-grammar))))

  ((entity-in-scene scene => entity-from-grammar)
   (let ((grammar-entity (make-instance 'naming-game-entity :id (id entity-in-scene))))
     (when grammar-entity
       (bind (entity-from-grammar 1.0 grammar-entity)))))
  
  :primitive-inventory *naming-game-primitives*)



(defprimitive filter-by-concept ((filtered-entity concept-representations::entity)
                                 (entity-set crs-conventionality-entity-set)
                                 (concept concept-representations::concept))
  
  ((entity-set concept => filtered-entity)
   (loop for entity in (entities entity-set)
         for similarity-score = (concept-representations::concept-entity-similarity concept entity) 
         collect (bind (filtered-entity similarity-score entity))))

  ;; if you have no concept, meaning if the concept is not bound, you can just invent a new concept based on the entity
  ((filtered-entity entity-set => concept)
   (let ((invented-concept (create-concept-representation filtered-entity :weighted-multivariate-distribution)))
     (bind (concept 1.0 invented-concept))))

  ((entity-set => filtered-entity concept)
   (loop for entity in (entities entity-set)
         for invented-concept = (concept-representations::create-concept-representation entity :weighted-multivariate-distribution)
         ;for concept-object = (make-instance 'concept-representations::concept :id 'something)
         do (append-data ontology :concepts invented-concept)
         collect (bind (concept 1.0 invented-concept)
                       (filtered-entity 1.0 entity))))
  
  :primitive-inventory *concept-emergence-game-primitives*)

(defgeneric compose-program (topic partial-meaning primitive-inventory)
    (:documentation "compose a program given a topic and a partial meaning"))

(defmethod compose-program ((topic naming-game-entity) (partial-meaning list) (primitive-inventory irl::primitive-inventory))
  (let* ((chunks (create-chunks-from-primitives (irl::primitives primitive-inventory)))
         (ontology (get-data (blackboard primitive-inventory) :ontology))
         (composer (make-chunk-composer :topic topic :meaning partial-meaning
                                        
                                        :chunks chunks :primitive-inventory primitive-inventory)))
    (get-all-solutions composer)))


(defmethod compose-program ((topic concept-representations::entity) (partial-meaning list) (primitive-inventory irl::primitive-inventory))
  (let* ((chunks (create-chunks-from-primitives (irl::primitives primitive-inventory)))
         (ontology (get-data (blackboard primitive-inventory) :ontology))
         (composer (make-chunk-composer :topic topic :meaning partial-meaning
                                        :chunks chunks :ontology ontology :primitive-inventory primitive-inventory)))
    (get-all-solutions composer)))