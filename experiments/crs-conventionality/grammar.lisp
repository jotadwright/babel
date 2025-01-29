(in-package :crs-conventionality)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                        ;;
;; Code implementing grammars             ;;
;;                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric make-initial-grammar (agent)
  (:documentation "Initialises the grammar of an agent."))


;; Naming game ;;
;;;;;;;;;;;;;;;;;

(defmethod make-initial-grammar ((agent naming-game-agent))
  "Initialises the grammar of a naming game agent."
  (let* ((grammar-name (make-const "grammar")))
    (setf (grammar agent) (eval `(def-fcg-constructions ,grammar-name
                                   :hashed t
                                   :feature-types ((meaning set-of-predicates)
                                                   (form set)
                                                   (footprints set))
                                   :fcg-configurations (;; Rendering and de-rendering
                                                        (:de-render-mode . :de-render-raw)
                                                        (:render-mode . :render-raw)
                                                        (:create-initial-structure-mode-formulation . :topic-and-scene)
                                                        (:create-initial-structure-mode-comprehension . :utterance-and-scene)
                                                        ;; Construction supplier and search
                                                        (:construction-inventory-processor-mode . :heuristic-search)
                                                        (:node-expansion-mode . :full-expansion)
                                                        (:cxn-supplier-mode . :hashed)
                                                        ;; Diagnostics and repairs
                                                        (:diagnostics fcg::diagnose-cip-find-forms)
                                                        (:repairs fcg::repair-through-invention)
                                                        ;; for using heuristics
                                                        (:search-algorithm . :best-first)
                                                        (:heuristics :cxn-score)
                                                        (:heuristic-value-mode . :sum-heuristics-and-parent) 
                                                        ;; goal tests
                                                        (:parse-goal-tests :interpretation-in-scene)
                                                        (:production-goal-tests :topic-retrieved)
                                                        (:max-nr-of-nodes . 3)
                                                        ;; meta-layer
                                                        ;(:consolidate-repairs . t)
                                                        ;(:use-meta-layer . t)
                                                        ))))
    (set-data (blackboard (grammar agent)) :primitive-inventory *naming-game-primitives*)
    (set-data (blackboard (grammar agent)) :ontology (make-blackboard))
    (set-data (get-data (blackboard (grammar agent)) :ontology) :entities (entities (world (experiment (population agent)))))
    ))


(defmethod unify-objects ((x crs-conventionality-entity-set) (y crs-conventionality-entity-set) bindings-list &key cxn-inventory)
  "Method for unifying crs-conventionality-entity-sets."
  (loop for entity-x in (entities x)
        unless (funcall #'always (find entity-x (entities y) :test #'(lambda (entity-x entity-y)
                                                                       (unify-objects entity-x entity-y bindings-list
                                                                                      :cxn-inventory cxn-inventory))))
          do (return +fail+)
        finally (return bindings-list)))

(defmethod unify-objects ((x naming-game-entity) (y naming-game-entity) bindings-list &key cxn-inventory)
  "Standard method for unifying object: unify slot values."
  (unify (id x) (id y) bindings-list :cxn-inventory cxn-inventory))



(in-package :fcg)

(defun make-naming-game-cxn (topic meaning cxn-inventory)
  (let* ((form (make-word))
         (cxn-name (intern (upcase (format nil "~a-cxn" form))))
         (unit-name (intern (upcase (format nil "?~a-unit" form))))
         (topic-as-entity-set (make-instance 'crs-conventionality::crs-conventionality-entity-set
                                             :entities (list topic))))
    
    (make-instance 'fcg-construction
                   :name cxn-name
                   :contributing-part (list (make-instance 'contributing-unit
                                                           :name unit-name
                                                           :unit-structure `((meaning ,meaning))))
                   :conditional-part (list (make-instance 'conditional-unit
                                                       :name unit-name
                                                       :comprehension-lock `((HASH form ,(list form))))
                                           (make-instance 'conditional-unit
                                                          :name 'root
                                                          :formulation-lock `((HASH topic ,topic-as-entity-set))))
                   :feature-types (feature-types cxn-inventory)
                   :cxn-inventory cxn-inventory)
    
    ))


(defun make-word ()
  "Creates a word of nr-of-syllables where a syllable is a consonant plus a vowel"
  (let ((vowels '("a" "e" "i" "o" "u"))
        (consonants '("b" "c" "d" "f" "g" "h" "j" "k" "l" "m" "n" "p" "q" "r" "s" "t" "v" "w" "x" "y" "z"))
        (word ""))
    (setq word "")
    (loop for i from 1 to 3
          do (setq word (concatenate 'string word (nth (random (length consonants)) consonants)))
             (setq word (concatenate 'string word (nth (random (length vowels)) vowels))))
  word))




#|(defmethod unify-objects ((concept cle::concept-distribution) (entity-set crs-conventionality-entity-set) bindings-list &key cxn-inventory)
  "Method for unifying crs-conventionality-entity-sets."
  (loop for entity in (entities entity-set)
        for weighted-similarity = (weighted-similarity entity concept)
        do (set-data (blackboard cxn-inventory) :similarities (cons (cons (id entity) (id concept)) weighted-similarity) )
        do (return +fail+)
        finally (return bindings-list)))

(defmethod weighted-similarity ((object concept-emergence-entity) (concept cle::concept))
  (loop with prototypes = (loop for prototype being the hash-values of (cle::prototypes concept) collect prototype)
        with ledger = (loop for prototype in prototypes sum (weight prototype))
        for prototype in prototypes
        for observation = (gethash (cle::channel prototype) (features object))
        for distance = (cle::observation-distance observation prototype)
        if (and distance (not (zerop ledger)))
          sum (* (expt (/ (weight prototype) ledger) 2)
                 (expt distance 2))
            into mahalanobis
        finally (return (exp (* 1/2 (- mahalanobis))))))

(formulate (topic (first (interactions *concept-emergence-canonical*)))
           :cxn-inventory (grammar (speaker (first (interactions *concept-emergence-canonical*))))
           :agent (speaker (first (interactions *concept-emergence-canonical*)))
           :scene (scene (first (interactions *concept-emergence-canonical*))))
(get-data (blackboard (grammar (speaker (first (interactions *concept-emergence-canonical*))))) 'similarities)

(create-initial-structure


 (bind entity-set ?topic topic)
(retrieve-named-entity ?entity)
(bind entity-set ?topic bolima)

(bind scene ?scene scene)

bolima-in-taal en bolima-in-world



(unify-objects (make-instance 'naming-game-entity :id 'object-5)
               (make-instance 'naming-game-entity :id '?a)
               (list +no-bindings+)
               :cxn-inventory (processing-cxn-inventory *fcg-constructions*))|#

