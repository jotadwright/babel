(in-package :crs-conventionality)

(def-irl-primitives naming-game-primitives
  :primitive-inventory *naming-game-primitives*)

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



(defgeneric compose-program (topic partial-meaning primitive-inventory)
    (:documentation "compose a program given a topic and a partial meaning"))

(defmethod compose-program ((topic naming-game-entity) (partial-meaning list) (primitive-inventory irl::primitive-inventory))
  (let* ((chunks (create-chunks-from-primitives (irl::primitives primitive-inventory)))
         (composer (make-chunk-composer :topic topic :meaning partial-meaning
                                        
                                        :chunks chunks :primitive-inventory primitive-inventory)))
    (get-all-solutions composer)))


;; given a topic and the scene as partial meaning, compose a program
#|
 (activate-monitor trace-irl)
 (compose-program (make-instance 'naming-game-entity :id 'object-1 :world (world *naming-game-canonical*))
                 `((bind crs-conventionality-entity-set ?scene ,(scene (first (interactions *naming-game-canonical*)))))
                  *naming-game-primitives*)

 (let* ((composition-result (compose-program (make-instance 'naming-game-entity :id 'object-1 :world (world *naming-game-canonical*))
                                             `((bind crs-conventionality-entity-set ?scene ,(scene (first (interactions *naming-game-canonical*)))))
                                             *naming-game-primitives*))
        )
   (when (find irl::succeeded (irl::statuses (irl::pip-node composition-result)))
     (
  |#

