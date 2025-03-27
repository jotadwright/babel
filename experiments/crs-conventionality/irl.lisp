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
                                 (concept concept-representations::concept)
                                 (scene crs-conventionality-entity-set))
  ;; First case, you come here during comprehension, you apply the concept on all entities in the scene, you return the binding for the entity with the highest similarity score
  ((scene concept => filtered-entity )
   (let* ((similarities
           (loop for entity in (entities scene)
                 for similarity-score = (concept-representations::concept-entity-similarity concept entity)
                 collect (cons entity similarity-score)))
          (sorted-similarities (sort similarities #'> :key #'cdr))
          (highest (first sorted-similarities)))
     (bind (filtered-entity 1.0 (car highest)))))

  ;; This case is triggered in normal conceptualisation when a concept applied on a specific concept. It returns the discriminative power.
  ;; Calculates the similarity of the concept and the filtered-entity
  ;; maybe we can optimise this in the same way as the concept emergence by using a lazy loop
 ((filtered-entity concept scene =>)
   (let* ((binding (find filtered-entity binding-objects :key #'value))
          (similarities
           (loop for entity in (entities scene)
                 for similarity-score = (concept-representations::concept-entity-similarity concept entity)
                 collect (cons entity similarity-score)))
          (sorted-similarities (sort similarities #'> :key #'cdr))
          (highest (first sorted-similarities)))
     (if (equal (car highest) filtered-entity)
       (let* ((best-other-sim (if (cdr (second sorted-similarities)) (cdr (second sorted-similarities)) 0))
              (discrimation-score (- (cdr highest) best-other-sim)))
         (when binding
           (setf (score binding) discrimation-score)))
       (let* ((entity-and-similarity (find filtered-entity sorted-similarities :key #'car))
              (highest-similarity (cdr highest))
              (entity-similarity (if (cdr entity-and-similarity) (cdr entity-and-similarity) 0))
              (discrimation-score (- entity-similarity highest-similarity)))
         (when binding
           (setf (score binding) discrimation-score))))))
       
 ;; This case is triggered in the metalayer of conceptualisation. A new concept is invented/adopted and the score is set to 1.
 ;; Invention done for every entity in the scene because we are in composer and the topic-entity is the target value.
  ((scene => filtered-entity concept)
   (loop for entity in (entities scene)
         for invented-concept = (concept-representations::create-concept-representation entity :weighted-multivariate-distribution)
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

(in-package :irl)

(defun irl-equal (a b)
  (eq a b))