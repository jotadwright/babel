;(ql:quickload :muhai-cookingbot)
(in-package :muhai-cookingbot)

(load (babel-pathname :directory '("applications" "muhai-cookingbot" "recipes") :name "broccoli-salad-kitchen-state" :type "lisp"))

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

(def-fcg-cxn grams-cxn
             ((?grams-unit
               (ontology g)
               (boundaries (left ?grams-unit)
                           (right ?grams-unit)))
              <-
              (?grams-unit
               --
               (HASH form ((string ?grams-unit "grams")))))
             :feature-types ((ontology default :lookup-in-ontology)))

(def-fcg-cxn tablespoons-cxn
             ((?tablespoons-unit
               (ontology tablespoon)
               (boundaries (left ?tablespoons-unit)
                           (right ?tablespoons-unit)))
              <-
              (?tablespoons-unit
               --
               (HASH form ((string ?tablespoons-unit "tablespoons")))))
             :feature-types ((ontology default :lookup-in-ontology)))

(def-fcg-cxn head-cxn
             ((?head-unit
               (ontology piece)
               (boundaries (left ?head-unit)
                           (right ?head-unit)))
              <-
              (?head-unit
               --
               (HASH form ((string ?head-unit "head")))))
             :feature-types ((ontology default :lookup-in-ontology)))

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

(def-fcg-cxn broccoli-cxn
             ((?broccoli-unit
               (ontology broccoli))
              <-
              (?broccoli-unit
               --
               (lex-id broccoli)))
             :feature-types ((ontology default :lookup-in-ontology)))

(def-fcg-cxn fresh-broccoli-morph-cxn
             ((?fresh-broccoli-unit
               (lex-id broccoli)
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
               (HASH form ((meets ?fresh-unit ?broccoli-unit))))))

(def-fcg-cxn broccoli-morph-cxn
             ((?broccoli-unit
               (lex-id broccoli)
               (boundaries (left ?broccoli-unit)
                           (right ?broccoli-unit)))
              <-
              (?broccoli-unit
               --
               (HASH form ((string ?broccoli-unit "broccoli"))))))


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


(def-fcg-cxn onions-cxn
             ((?onions-unit
               (ontology onion)
               (boundaries (left ?onions-unit)
                           (right ?onions-unit)))
              <-
              (?onions-unit
               --
               (HASH form ((string ?onions-unit "onions")))))
             :feature-types ((ontology default :lookup-in-ontology)))


(def-fcg-cxn bacon-cxn
             ((?bacon-unit
               (ontology cooked-bacon))
              <-
              (?bacon-unit
               --
               (lex-id bacon)))
             :feature-types ((ontology default :lookup-in-ontology)))

(def-fcg-cxn cooked-bacon-morph-cxn
             ((?cooked-bacon-unit
               (boundaries (left ?cooked-unit)
                           (right ?bacon-unit))
               (lex-id bacon)
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
               (HASH form ((meets ?cooked-unit ?bacon-unit))))))

(def-fcg-cxn bacon-morph-cxn
             ((?bacon-unit
               (boundaries (left ?bacon-unit)
                           (right ?bacon-unit))
               (lex-id bacon))
              <-
              (?bacon-unit
               --
               (HASH form ((string ?bacon-unit "bacon"))))))


(def-fcg-cxn vinegar-cxn
             ((?vinegar-unit
               (ontology vinegar)
               (boundaries (left ?vinegar-unit)
                           (right ?vinegar-unit)))
              <-
              (?vinegar-unit
               --
               (HASH form ((string ?vinegar-unit "vinegar")))))
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
               (ontology mayonnaise))
              <-
              (?mayonnaise-unit
               --
               (lex-id mayo)))
             :feature-types ((ontology default :lookup-in-ontology)))

(def-fcg-cxn mayonnaise-morph-cxn
             ((?mayonnaise-unit
               (lex-id mayo)
               (boundaries (left ?mayonnaise-unit)
                           (right ?mayonnaise-unit)))
              <-
              (?mayonnaise-unit
               --
               (HASH form ((string ?mayonnaise-unit "mayonnaise"))))))

(def-fcg-cxn mayo-morph-cxn
             ((?mayonnaise-unit
               (lex-id mayo)
               (boundaries (left ?mayonnaise-unit)
                           (right ?mayonnaise-unit)))
              <-
              (?mayonnaise-unit
               --
               (HASH form ((string ?mayonnaise-unit "mayo"))))))


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


(def-fcg-cxn mozzarella-cxn
             ((?mozzarella-unit
               (ontology grated-mozzarella))
              <-
              (?mozzarella-unit
               --
               (lex-id mozza)))
             :feature-types ((ontology default :lookup-in-ontology)))

(def-fcg-cxn grated-mozzarella-cheese-morph-cxn
             ((?grated-mozzarella-unit
               (lex-id mozza)
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
                           (meets ?mozzarella-unit ?cheese-unit))))))

(def-fcg-cxn mozzarella-morph-cxn
             ((?mozzarella-unit
               (lex-id mozza)
               (boundaries (left ?mozzarella-unit)
                           (right ?mozzarella-unit)))
              <-
              (?mozzarella-unit
               --
               (HASH form ((string ?mozzarella-unit "mozzarella"))))))


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


;; instructions 1-2 cxns ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(def-fcg-cxn cutting-verb-cxn
             ((?cut-unit
               (ontology cuttable))
              <-
              (?cut-unit
               --
               (lex-id cut)))
             :feature-types ((ontology default :lookup-in-ontology)))

(def-fcg-cxn pieces-morph-cxn
             ((?pieces-unit
               (lex-id chopped-pattern)
               (boundaries (left ?pieces-unit)
                           (right ?pieces-unit)))
              <-
              (?pieces-unit
               --
               (HASH form ((string ?pieces-unit "pieces"))))))

(def-fcg-cxn bite-size-pieces-morph-cxn
             ((?bite-size-pieces-unit
               (lex-id chopped-pattern)
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
                           (meets ?size-unit ?pieces-unit))))))

(def-fcg-cxn chopped-pattern-cxn
             ((?chopped-pattern
               (ontology chopped))
              <-
              (?chopped-pattern
               --
               (lex-id chopped-pattern)))
             :feature-types ((ontology default :lookup-in-ontology)))

(def-fcg-cxn cutting-verb-ingredient-into-pattern-cxn
             ;; match ingredient in utterance to ingredient in world
             ;; make sure that ingredient in world has cuttable property (from the verb)
             ;; connect the kitchen state to the input of CUT
             ((?clause-unit
               (meaning ((cut ?output-container ?output-kitchen-state ?kitchen-state-in
                              ?container-with-x ?pattern-class ?cutting-tool ?cutting-surface)))
               (boundaries (left ?verb-unit-left)
                           (right ?pattern-unit-right))
               (subunits (?cut-verb-unit ?x-in-utterance-unit ?into-unit ?pattern-unit)))
              <-
              (?ks-unit
               --
               (ontological-class kitchen-state)
               (binding-variable ?kitchen-state-in))
              (?cut-verb-unit
               --
               (ontology (ontological-class ?property))
               (boundaries (left ?verb-unit-left)
                           (right ?verb-unit-right)))
              (?x-in-utterance-unit
               --
               (boundaries (left ?x-unit-in-utterance-left)
                           (right ?x-unit-in-utterance-right))
               (ontology (ontological-class ?ontological-class-utterance)
                         (ontological-types (?property))))
              (?x-in-world-unit
               --
               (properties
                (contents ((ontological-class ?ontological-class-world)
                           (ontological-types (?ontological-class-utterance ?property)))))
               (binding-variable ?container-with-x))
              (?into-unit
               --
               (HASH form ((string ?into-unit "into")
                           (meets ?verb-unit-right ?x-unit-in-utterance-left)
                           (meets ?x-unit-in-utterance-right ?into-unit)
                           (meets ?into-unit ?pattern-unit-left))))
              (?pattern-unit
               --
               (ontology (ontological-class ?pattern-class)
                         (ontological-types (pattern)))
               (boundaries (left ?pattern-unit-left)
                           (right ?pattern-unit-right)))
              )
             :feature-types ((ontological-class default :compare-ontological-vectors)))


;; instructions 3-4 cxns ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(def-fcg-cxn base-ingredients-list-cxn
             ;; ingredients X 'and' Y
             ;; match ingredient in utterance to ingredient in world
             ;; => have to adapt 'compare-ontological-vectors' to handle multiple ontological vectors in one cxn?
             ;; meaning: transfer contents
             ;; make ingredient-list-unit
             ;; keep track of input and output container
             )

(def-fcg-cxn ingredients-list-cxn
             ;; match on ingredient + ingredient-list-unit
             ;; match ingredient in utterance to ingredient in world
             ;; meaning: transfer contents
             ;; keep track of input and output container
             )

(def-fcg-cxn large-bowl-cxn
             ;; meaning: fetch
             ;; connect the kitchen state to the input of FETCH
             ;; keep track of the output container
             )

(def-fcg-cxn mix-ingredient-list-into-container-cxn
             ;; connect everything together
             ;; add MINGLE
             )

(def-fcg-cxn in-seperate-container-mix-ingredient-list-cxn
             ;; connect everything together
             ;; add MIX
             )
|#


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
                            "chop up broccoli into bite size pieces" ;; => CUTTING-VERB INGREDIENT into PATTERN
                            ;"mix broccoli , onions , bacon and mozzarella in large bowl"  ;; => mix INGREDIENT-LIST in CONTAINER
                            ;"in separate large bowl combine vinegar , sugar and mayo"  ;; => in separate CONTAINER combine INGREDIENT-LIST
                            ;"pour over broccoli mixture and toss to coat"  ;; => pour IMPLICIT over 
                            ;"best if made a day ahead and stored in the refrigerator"

                            ;; INGREDIENT-LIST cxn has transfer-contents as meaning
                            
                            "end"
                            )
                            ; *pdm*
                          (initialise-personal-dynamic-memory
                           *fcg-constructions*
                           *init-op*)
                          )
    (append *init-op* meaning-network)))