;(ql:quickload :muhai-cookingbot)

(in-package :muhai-cookingbot)

(defparameter *initial-kitchen-state* 
  (make-instance 
   'kitchen-state
   :contents
   (list (make-instance 'fridge
                        :contents (list
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'lime-juice :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'muhai-cookingbot::l)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 0.5)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'mayonnaise :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 500)))))))
         (make-instance 'pantry
                        :contents (list  (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'white-sugar :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 1000)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'cider-vinegar :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'ml)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 500)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'red-bell-pepper :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'piece)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 10)))))                                          
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'extra-virgin-olive-oil :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 50)))))                                         
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'ground-black-pepper :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 500)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'jalapeno :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'piece)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 5)))))

                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'grated-mozzarella :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 500)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'ground-black-pepper :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 500)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'red-onion :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'piece)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 5)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'cooked-bacon :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 500)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'broccoli :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'piece)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 2)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'frozen-corn :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 500)))))))
         (make-instance 'kitchen-cabinet
                        :contents (list
                                   ;; bowls
                                   (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)
                                   (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)
                                   (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'small-bowl) (make-instance 'small-bowl) (make-instance 'small-bowl)

                                   ;; bowl-lids
                                   (make-instance 'medium-bowl-lid) (make-instance 'medium-bowl-lid) (make-instance 'medium-bowl-lid)
                                   (make-instance 'large-bowl-lid) (make-instance 'large-bowl-lid) (make-instance 'large-bowl-lid)

                                   ;; jars
                                   (make-instance 'jar) (make-instance 'jar) (make-instance 'jar)

                                   ;; jar-lids
                                   (make-instance 'jar-lid) (make-instance 'jar-lid) (make-instance 'jar-lid)

                                   ;; wrapping
                                   (make-instance 'plastic-wrap)

                                   ;; tools
                                   (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                   (make-instance 'wooden-spoon) (make-instance 'wooden-spoon) (make-instance 'wooden-spoon)
                                   (make-instance 'knife) (make-instance 'knife) (make-instance 'knife)
                                   (make-instance 'cutting-board) (make-instance 'cutting-board)
                                   (make-instance 'cutting-board) (make-instance 'cutting-board)
                                   

                                   ;; baking equipment
                                   (make-instance 'cookie-sheet)
                                   (make-instance 'baking-tray)
                                   (make-instance 'baking-paper))))))

;; cxn inventory
(def-fcg-constructions broccoli-salad-grammar
  :feature-types ((form set-of-predicates)
                  (meaning set-of-predicates)
                  (ontological-types set)
                  (ontological-subclasses set)
                  (ontological-linked-classes set)
                  (subunits set)
                  (args set)
                  (arg1 set)
                  (referents set)
                  (contents set-of-feature-value-pairs)
                  (footprints set)
                  (items set-of-feature-value-pairs))
  :fcg-configurations ((:de-render-mode . :de-render-recipe-utterance)
                       (:node-tests :restrict-nr-of-nodes :restrict-search-depth)
                       (:parse-goal-tests :no-applicable-cxns :no-strings-in-root :connected-structure)
                       (:construction-inventory-processor-mode . :heuristic-search)
                       (:node-expansion-mode . :full-expansion)
                       (:cxn-supplier-mode . :all-cxns)
                       (:search-algorithm . :best-first)
                       (:heuristics :nr-of-applied-cxns :ontological-distance :nr-of-units-matched)
                       (:heuristic-value-mode . :sum-heuristics-and-parent))
  :visualization-configurations  ((:hide-features nil)
                                  (:show-constructional-dependencies . nil)))


;; unit cxns ;;
;;;;;;;;;;;;;;;

(def-fcg-cxn gram-cxn
             ((?gram-unit
               (ontology g)
               (boundaries (left ?gram-unit)
                           (right ?gram-unit)))
              <-
              (?gram-unit
               --
               (lex-id gram)))
             :feature-types ((ontology default :lookup-in-ontology)))

(def-fcg-cxn grams-morph-cxn
             ((?grams-unit
               (lex-id gram))
              <-
              (?grams-unit
               --
               (HASH form ((string ?grams-unit "grams"))))))

(def-fcg-cxn tablespoon-cxn
             ((?tablespoon-unit
               (ontology tablespoon)
               (boundaries (left ?tablespoon-unit)
                           (right ?tablespoon-unit)))
              <-
              (?tablespoon-unit
               --
               (lex-id tablespoon)))
             :feature-types ((ontology default :lookup-in-ontology)))

(def-fcg-cxn tablespoons-morph-cxn
             ((?tablespoons-unit
               (lex-id tablespoon))
              <-
              (?tablespoons-unit
               --
               (HASH form ((string ?tablespoons-unit "tablespoons"))))))

(def-fcg-cxn head-cxn
             ((?head-unit
               (ontology piece)
               (boundaries (left ?head-unit)
                           (right ?head-unit)))
              <-
              (?head-unit
               --
               (lex-id head)))
             :feature-types ((ontology default :lookup-in-ontology)))

(def-fcg-cxn head-morph-cxn
             ((?head-unit
               (lex-id head))
              <-
              (?head-unit
               --
               (HASH form ((string ?head-unit "head"))))))

;; Quantity-cxn ;;
;;;;;;;;;;;;;;;;;;

(def-fcg-cxn quantity-cxn
             ((?quantity-unit
               (value ?quantity)
               (ontology quantity)
               (boundaries (left ?quantity-unit)
                           (right ?quantity-unit)))
              <-
              (?quantity-unit
               --
               (HASH form ((string ?quantity-unit ?quantity)))))
             :feature-types ((form set-of-predicates :number)
                             (value default :parse-integer)
                             (ontology default :lookup-in-ontology)))

;; ingredient cxns ;;
;;;;;;;;;;;;;;;;;;;;;

(def-fcg-cxn fresh-broccoli-cxn
             ((?fresh-broccoli-unit
               (ontology broccoli)
               (boundaries (left ?fresh-unit)
                           (right ?broccoli-unit))
               (subunits (?fresh-unit ?broccoli-unit)))
              <-
              (?fresh-unit
               --
               (HASH form ((string ?fresh-unit "fresh"))))
              (?broccoli-unit
               --
               (HASH form ((string ?broccoli-unit "broccoli"))))
              (?fresh-broccoli-unit
               --
               (HASH form ((meets ?fresh-unit ?broccoli-unit)))))
             :feature-types ((ontology default :lookup-in-ontology)))


(def-fcg-cxn red-onion-cxn
             ((?red-onion-unit
               (ontology red-onion)
               (boundaries (left ?red-unit)
                           (right ?onion-unit))
               (subunits (?red-unit ?onion-unit)))
              <-
              (?red-unit
               --
               (HASH form ((string ?red-unit "red"))))
              (?onion-unit
               --
               (HASH form ((string ?onion-unit "onion"))))
              (?red-onion-unit
               --
               (HASH form ((meets ?red-unit ?onion-unit)))))
             :feature-types ((ontology default :lookup-in-ontology)))


(def-fcg-cxn cooked-bacon-cxn
             ((?cooked-bacon-unit
               (ontology cooked-bacon)
               (boundaries (left ?cooked-unit)
                           (right ?bacon-unit))
               (subunits (?cooked-unit ?bacon-unit)))
              <-
              (?cooked-unit
               --
               (HASH form ((string ?cooked-unit "cooked"))))
              (?bacon-unit
               --
               (HASH form ((string ?bacon-unit "bacon"))))
              (?cooked-bacon-unit
               --
               (HASH form ((meets ?cooked-unit ?bacon-unit)))))
             :feature-types ((ontology default :lookup-in-ontology)))


(def-fcg-cxn cider-vinegar-cxn
             ((?cider-vinegar-unit
               (ontology cider-vinegar)
               (boundaries (left ?cider-unit)
                           (right ?vinegar-unit))
               (subunits (?cider-unit ?vinegar-unit)))
              <-
              (?cider-unit
               --
               (HASH form ((string ?cider-unit "cider"))))
              (?vinegar-unit
               --
               (HASH form ((string ?vinegar-unit "vinegar"))))
              (?cider-vinegar-unit
               --
               (HASH form ((meets ?cider-unit ?vinegar-unit)))))
             :feature-types ((ontology default :lookup-in-ontology)))


(def-fcg-cxn mayonnaise-cxn
             ((?mayonnaise-unit
               (ontology mayonnaise)
               (boundaries (left ?mayonnaise-unit)
                           (right ?mayonnaise-unit)))
              <-
              (?mayonnaise-unit
               --
               (HASH form ((string ?mayonnaise-unit "mayonnaise")))))
             :feature-types ((ontology default :lookup-in-ontology)))


(def-fcg-cxn sugar-cxn
             ((?sugar-unit
               (ontology sugar)
               (boundaries (left ?sugar-unit)
                           (right ?sugar-unit)))
              <-
              (?sugar-unit
               --
               (HASH form ((string ?sugar-unit "sugar")))))
             :feature-types ((ontology default :lookup-in-ontology)))


(def-fcg-cxn grated-mozzarella-cheese-cxn
             ((?grated-mozzarella-unit
               (ontology grated-mozzarella)
               (boundaries (left ?grated-unit)
                           (right ?cheese-unit))
               (subunits (?grated-unit ?mozzarella-unit ?cheese-unit)))
              <-
              (?grated-unit
               --
               (HASH form ((string ?grated-unit "grated"))))
              (?mozzarella-unit
               --
               (HASH form ((string ?mozzarella-unit "mozzarella"))))
              (?cheese-unit
               --
               (HASH form ((string ?cheese-unit "cheese"))))
              (?grated-mozzarella-unit
               --
               (HASH form ((meets ?grated-unit ?mozzarella-unit)
                           (meets ?mozzarella-unit ?cheese-unit)))))
             :feature-types ((ontology default :lookup-in-ontology)))

(def-fcg-cxn broccoli-cxn
             ((?broccoli-unit
               (ontology broccoli)
               (boundaries (left ?broccoli-unit)
                           (right ?broccoli-unit)))
              <-
              (?broccoli-unit
               --
               (HASH form ((string ?broccoli-unit "broccoli")))))
             :feature-types ((ontology default :lookup-in-ontology)))


(def-fcg-cxn quantity-unit-ingredient-cxn
             ((?noun-phrase-unit
               (phrase-type noun-phrase)
               (subunits (?quantity-unit ?unit-unit ?ingredient-unit))
               (input-args (kitchen-state ?kitchen-state-in)
                           (args (?ingredient-in)))
               (output-args (kitchen-state ?kitchen-state-out)
                            (args (?ingredient-out)))
               (ontology (ontological-class ?ingredient)
                         (ontological-types (ingredient)))
               (boundaries (left ?quantity-unit-left)
                           (right ?ingredient-unit-right)))
              <-
              (?kitchen-state
               --
               (ontological-class kitchen-state)
               (binding-variable ?kitchen-state-in))
              (?quantity-unit
               --
               (value ?quantity)
               (ontology (ontological-class quantity))
               (boundaries (left ?quantity-unit-left)
                           (right ?quantity-unit-right)))
              (?unit-unit
               --
               (ontology (ontological-class ?unit)
                         (ontological-types (unit)))
               (boundaries (left ?unit-unit-left)
                           (right ?unit-unit-right)))
              (?ingredient-unit
               --
               (ontology (ontological-class ?ingredient-in)
                         (ontological-types (ingredient)))
               (boundaries (left ?ingredient-unit-left)
                           (right ?ingredient-unit-right)))
              (?noun-phrase-unit
               (meaning ((fetch-and-proportion ?ingredient-out
                                               ?kitchen-state-out
                                               ?kitchen-state-in
                                               ?target-container
                                               ?ingredient-in
                                               ?quantity
                                               ?unit)))
               --
               (HASH form ((meets ?quantity-unit-right ?unit-unit-left)
                           (meets ?unit-unit-right ?ingredient-unit-left))))))


(def-fcg-cxn x-chopped-cxn
             ((?x-chopped-unit
               (subunits (?x-unit-in-utterance ?comma-unit ?chopped-unit))
               (boundaries (left ?x-unit-in-utterance-left)
                           (right ?chopped-unit))
               (input-args (args (?container-with-ingredients))
                           (kitchen-state ?input-kitchen-state))
               (output-args (args (?chopped-ingredient))
                            (kitchen-state ?output-kitchen-state)))
              <-
              (?x-unit-in-utterance
               --
               (output-args (args (?container-with-ingredients))
                            (kitchen-state ?input-kitchen-state))
               (boundaries (left ?x-unit-in-utterance-left)
                           (right ?x-unit-in-utterance-right)))
              (?comma-unit
               --
               (HASH form ((string ?comma-unit ","))))
              (?chopped-unit
               --
               (HASH form ((string ?chopped-unit "chopped"))))
              (?x-chopped-unit
               (HASH meaning ((peel ?peeled-ingredient ?peelings ?intermediate-kitchen-state
                                    ?input-kitchen-state ?container-with-ingredients ?peeling-tool)
                              (cut ?chopped-ingredient ?output-kitchen-state ?intermediate-kitchen-state
                                   ?peeled-ingredient chopped ?peeling-tool ?cutting-board)))
               --
               (HASH form ((meets ?x-unit-in-utterance-right ?comma-unit)
                           (meets ?comma-unit ?chopped-unit))))))

;; cut-cxn with morphs 'cut' and 'chop up', meaning cut
;; pieces-cxn and bite-size-pieces-cxn (no meaning)
;; verb-x-into-y-cxn

(def-fcg-cxn ingredient-in-world-cxn
             ((?x-ingredient-unit
               (referent (args (?container-with-x)))
               (ontology (ontological-class ?ontological-class-utterance)
                         (ontological-types ?ontological-types))
               (subunits (?x-unit-in-utterance))
               (boundaries (left ?x-unit-in-utterance-left)
                           (right ?x-unit-in-utterance-right)))
              <-
              (?x-unit-in-utterance
               --
               (boundaries (left ?x-unit-in-utterance-left)
                           (right ?x-unit-in-utterance-right))
               (ontology (ontological-class ?ontological-class-utterance)
                         (ontological-types ?ontological-types)))
              (?x-unit-in-world
               --
               (ontological-types (not kitchen-state))
               (properties (contents ((ontological-class ?ontological-class-world))))
               (binding-variable ?container-with-x)))
             :feature-types ((ontological-class default :compare-ontological-vectors)))


;; to do: replace x-in-world and x-in-utterance units
;; with result from ingredient-in-world-cxn
;; to do: make sure that the ingredient has a property
;; that matches with the verb (cuttable)

(def-fcg-cxn cut-x-into-pattern-cxn
             ((?clause-unit
               (meaning ((cut ?output-container ?output-kitchen-state ?kitchen-state-in
                              ?container-with-x chopped ?cutting-tool ?cutting-surface)))
               (output-args (arg1 ?output-container)
                            (kitchen-state ?output-kitchen-state))
               (boundaries (left ?verb-unit-left)
                           (right ?pattern-unit-right))
               (subunits (?ks-unit ?cut-verb-unit ?x-in-utterance-unit ?into-unit ?pattern-unit)))
              <-
              (?ks-unit
               --
               (ontological-class kitchen-state)
               (binding-variable ?kitchen-state-in))
              (?cut-verb-unit
               --
               (lex-id cut)
               (boundaries (left ?verb-unit-left)
                           (right ?verb-unit-right)))
              ;(?verb-unit
              ; --
              ; (ontology (ontological-class ?property))
              ; (boundaries (left ?verb-unit-left)
              ;             (right ?verb-unit-right))
              ; (input-args (kitchen-state ?kitchen-state-in)
              ;             (arg1 ?container-with-x)))
              (?x-in-utterance-unit
               --
               (boundaries (left ?x-unit-in-utterance-left)
                           (right ?x-unit-in-utterance-right))
               (ontology (ontological-class ?ontological-class-utterance)
                         (ontological-types ?ontological-types)))
              (?x-in-world-unit
               --
               (ontological-types (not kitchen-state))
               (properties (contents ((ontological-class ?ontological-class-world))))
               (binding-variable ?container-with-x))
              (?into-unit
               --
               (HASH form ((string ?into-unit "into")
                           (meets ?verb-unit-right ?x-unit-in-utterance-left)
                           (meets ?x-unit-in-utterance-right ?into-unit)
                           (meets ?into-unit ?pattern-unit-left))))
              ;; pattern can also be done via ontology?
              (?pattern-unit
               --
               (lex-id chopped)
               (boundaries (left ?pattern-unit-left)
                           (right ?pattern-unit-right))))
             :feature-types ((ontological-class default :compare-ontological-vectors)))

(def-fcg-cxn cut-morph-cxn
             ((?cut-unit
               (lex-id cut)
               (boundaries (left ?cut-unit)
                           (right ?cut-unit)))
              <-
              (?cut-unit
               --
               (HASH form ((string ?cut-unit "cut"))))))

(def-fcg-cxn chop-up-morph-cxn
             ((?chop-up-unit
               (lex-id cut)
               (boundaries (left ?chop-unit)
                           (right ?up-unit))
               (subunits (?chop-unit ?up-unit)))
              <-
              (?chop-unit
               --
               (HASH form ((string ?chop-unit "chop"))))
              (?up-unit
               --
               (HASH form ((string ?up-unit "up"))))
              (?chop-up-unit
               --
               (HASH form ((meets ?chop-unit ?up-unit))))))

#|
(def-fcg-cxn cut-cxn
             ((?cut-unit
               (ontology cuttable)
               (input-args (kitchen-state ?input-kitchen-state)
                           (arg1 ?input-container))
               (output-args (kitchen-state ?output-kitchen-state)
                            (arg1 ?output-container)))
              <-
              (?cut-unit
               (HASH meaning ((cut ?output-container ?output-kitchen-state ?input-kitchen-state
                                   ?input-container chopped ?cutting-tool ?cutting-surface)))
               --
               (boundaries (left ?left-cut-unit)
                           (right ?right-cut-unit))
               (lex-id cut)))
             :feature-types ((ontology default :lookup-in-ontology)))
|#


(def-fcg-cxn pieces-morph-cxn
             ((?pieces-unit
               (lex-id chopped)
               (boundaries (left ?pieces-unit)
                           (right ?pieces-unit)))
              <-
              (?pieces-unit
               --
               (HASH form ((string ?pieces-unit "pieces")))))
             :feature-types ((ontology default :lookup-in-ontology)))
               

(def-fcg-cxn bite-size-pieces-morph-cxn
             ((?bite-size-pieces-unit
               (lex-id chopped)
               (boundaries (left ?bite-unit)
                           (right ?pieces-unit))
               (subunits (?bite-unit ?size-unit ?pieces-unit)))
              <-
              (?bite-unit
               --
               (HASH form ((string ?bite-unit "bite"))))
              (?size-unit
               --
               (HASH form ((string ?size-unit "size"))))
              (?pieces-unit
               --
               (HASH form ((string ?pieces-unit "pieces"))))
              (?bite-size-pieces-unit
               --
               (HASH form ((meets ?bite-unit ?size-unit)
                           (meets ?size-unit ?pieces-unit)))))
             :feature-types ((ontology default :lookup-in-ontology)))

;; large-bowl-cxn (fetch)
;; x-and-y-ingredients (transfer-contents)
;; x-comma-y-ingredients (transfer-contents)
;; mix-Xs-in-Container-cxn (mix)

#|
; (transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-large-bowl-1 ?large-bowl-1 ?chopped-broccoli ?quantity-a ?unit-a)
(def-fcg-cxn x-and-y-ingredients
             ((?x-and-y-ingredients-unit
               (subunits (?x-ingredient-unit ?and-unit ?y-ingredient-unit))
               (boundaries (left ?ingredient-x-unit-left)
                           (right ?ingredient-y-unit-right))
               (meaning ((transfer-contents ?output-container-y ?rest-y ?output-ks-y
                                            ?input-ks-y ?new-container ?input-container-y ?quantity-y ?unit-y)
                         (transfer-contents 
               )
              <-
              (?x-ingredient-unit
               --
               (referent (args (?input-container-x)))
               (ontology (ontological-types (?property-x)))
               (boundaries (left ?ingredient-x-unit-left)
                           (right ?ingredient-x-unit-right)))
              (?and-unit
               --
               (HASH form ((string ?and-unit "and")
                           (meets ?ingredient-x-unit-right ?and-unit)
                           (meets ?and-unit ?ingredient-y-unit-left))))
              (?y-ingredient-unit
               --
               (referent (args (?input-container-y)))
               (ontology (ontological-types (?property-y)))
               (boundaries (left ?ingredient-y-unit-left)
                           (right ?ingredient-y-unit-right)))))
               |#


;; in-separate-Container-combine-Ingredients (mix)

;; pour-implicit-over-explicit (transfer-contents)
;; toss-to-coat (mingle)
;; X-and-y-actions

;; Running the recipe
(activate-monitor trace-fcg)
(activate-monitor trace-irl)

(clear-output)

(defparameter *init-op* `((get-kitchen ,(make-var 'kitchen-state))))

(defparameter *pdm* (initialise-personal-dynamic-memory
                    *fcg-constructions*
                    *init-op*))

(defparameter *output*
  (multiple-value-bind (final-set-of-bindings meaning-network)
      (process-utterances '(;;;; Ingredients
                            "1 head fresh broccoli"
                            "50 grams red onion , chopped"
                            "450 grams cooked bacon"
                            "2.5 tablespoons cider vinegar"   ;"2 1/2 tablespoons cider vinegar"
                            "230 grams mayonnaise"
                            "70 grams sugar"
                            "170 grams grated mozzarella cheese"

                            ;;;; Instructions
                            "cut cooked bacon into pieces"
                            "chop up broccoli into bite size pieces"
                            ;"mix broccoli , onions , bacon and mozzarella in large bowl"
                            ;"in separate large bowl combine vinegar , sugar and mayo"
                            ;"pour over broccoli mixture and toss to coat"
                            ;"best if made a day ahead and stored in the refrigerator"
                            
                            "end"
                            )
                            ; *pdm*
                          (initialise-personal-dynamic-memory
                           *fcg-constructions*
                           *init-op*)
                          )
    (append *init-op* meaning-network)))