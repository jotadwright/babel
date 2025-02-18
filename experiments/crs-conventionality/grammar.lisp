(in-package :crs-conventionality)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             ;;
;; Code implementing grammars  ;;
;;                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric make-initial-grammar (agent)
  (:documentation "Initialises the grammar of an agent."))

(defmethod unify-objects ((x crs-conventionality-entity-set) (y crs-conventionality-entity-set) bindings-list &key cxn-inventory)
  "Method for unifying crs-conventionality-entity-sets."
  (loop for entity-x in (entities x)
        unless (funcall #'always (find entity-x (entities y) :test #'(lambda (entity-x entity-y)
                                                                       (unify-objects entity-x entity-y bindings-list
                                                                                      :cxn-inventory cxn-inventory))))
          do (return +fail+)
        finally (return bindings-list)))

;; Naming game ;;
;;;;;;;;;;;;;;;;;

(defmethod make-initial-grammar ((agent naming-game-agent))
  "Initialises the grammar of a naming game agent."
  (let* ((grammar-name (make-const "grammar"))
         (ontology (make-blackboard)))
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
                                                        (:conceptualisation-diagnostics fcg::diagnose-cip-conceptualisation-success)
                                                        (:interpretation-diagnostics fcg::diagnose-cip-interpretation-success)
                                                        (:repairs fcg::repair-through-invention)
                                                        ;; for using heuristics
                                                        (:search-algorithm . :best-first)
                                                        (:heuristics :cxn-score)
                                                        (:heuristic-value-mode . :sum-heuristics-and-parent) 
                                                        ;; goal tests
                                                        (:parse-goal-tests :interpretation-in-scene)
                                                        (:production-goal-tests :topic-retrieved)
                                                        (:node-tests :check-branch-for-solution :check-duplicate :restrict-nr-of-nodes :restrict-search-depth)
                                                        ;; meta-layer 
                                                        ;(:consolidate-repairs . t)
                                                        ;(:use-meta-layer . t)
                                                        ))))
    (set-data (blackboard (grammar agent)) :primitive-inventory *naming-game-primitives*)
    (set-data (blackboard *naming-game-primitives*) :ontology ontology)
    (set-data (blackboard (grammar agent)) :ontology ontology)
    (set-data (get-data (blackboard (grammar agent)) :ontology) :entities (entities (world (experiment (population agent)))))))


(defmethod unify-objects ((x naming-game-entity) (y naming-game-entity) bindings-list &key cxn-inventory)
  "Standard method for unifying objects: unify slot values."
  (unify (id x) (id y) bindings-list :cxn-inventory cxn-inventory))


;; Concept emergence game ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod make-initial-grammar ((agent concept-emergence-game-agent))
  "Initialises the grammar of a naming game agent."
  (let* ((grammar-name (make-const "grammar"))
         (ontology (make-blackboard)))
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
                                                        (:conceptualisation-diagnostics fcg::diagnose-cip-conceptualisation-success)
                                                        (:interpretation-diagnostics fcg::diagnose-cip-interpretation-success)
                                                        (:repairs fcg::repair-through-concept-invention)
                                                        ;; for using heuristics
                                                        (:search-algorithm . :best-first)
                                                        (:heuristics :cxn-score)
                                                        (:heuristic-value-mode . :sum-heuristics-and-parent) 
                                                        ;; goal tests
                                                        (:parse-goal-tests :interpretation-in-scene)
                                                        (:production-goal-tests :topic-retrieved)
                                                        (:node-tests :check-branch-for-solution :check-duplicate :restrict-nr-of-nodes :restrict-search-depth)
                                                        (:max-nr-of-nodes . 10)
                                                        ;; meta-layer 
                                                        ;(:consolidate-repairs . t)
                                                        ;(:use-meta-layer . t)
                                                        )
                                     :visualization-configurations ((:with-search-debug-data . t)
                                                                    (:hide-features . nil)
                                                                    (:show-constructional-dependencies . nil)))))
    ;; ontology is necessary in both irl and fcg
    ;; ONTOLOGY SHOULD BE SPECIFIC TO THE AGENT
    (set-data (blackboard (grammar agent)) :primitive-inventory *concept-emergence-game-primitives*)
    (set-data (blackboard *concept-emergence-game-primitives*) :ontology ontology)
    (set-data (blackboard (grammar agent)) :ontology ontology)
    (set-data (get-data (blackboard (grammar agent)) :ontology) :entities (entities (world (experiment (population agent)))))))


(defmethod unify-objects ((concept concept-representations::concept) (entity-set crs-conventionality-entity-set) bindings-list &key cxn-inventory)
  "Standard method for unifying objects: unify slot values."
  (if (concept-representations::concept-entity-similarity concept (first (entities entity-set)))
    bindings-list
    +fail+))

(in-package :fcg)
;; ! SPECIALISES METHODS IN :fcg


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
