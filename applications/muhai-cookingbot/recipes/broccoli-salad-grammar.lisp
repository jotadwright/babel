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
                  (components set-of-feature-value-pairs)
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
             ;; add the cutting pattern to CUT
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


(def-fcg-cxn base-ingredients-list-cxn
             ;; ingredients X 'and' Y
             ;; match ingredient in utterance to ingredient in world
             ;; meaning: 2x transfer contents
             ;; make ingredient-list-unit
             ;; keep track of input and output container
             ((?ingredient-list-unit
               (boundaries (left ?x-unit-in-utterance-left)
                           (right ?y-unit-in-utterance-right))
               (subunits (?x-in-utterance-unit ?and-unit ?y-in-utterance-unit))
               (input-args (kitchen-state ?ks-in)
                           (arg1 ?new-container))
               (output-args (kitchen-state ?ks-out)
                            (arg1 ?output-x))
               (meaning ((transfer-contents ?output-y ?rest-y ?ks-mid ?ks-in ?new-container ?container-with-y ?quantity-y ?unit-y)
                         (transfer-contents ?output-x ?rest-x ?ks-out ?ks-mid ?output-y ?container-with-x ?quantity-x ?unit-x)))
               )
              <-
              (?x-in-utterance-unit
               --
               (boundaries (left ?x-unit-in-utterance-left)
                           (right ?x-unit-in-utterance-right))
               (ontology (ontological-class ?ontological-class-utterance-x)))
              (?x-in-world-unit
               --
               (properties
                (contents ((ontological-class ?ontological-class-world-x))))
               (binding-variable ?container-with-x))
              (?and-unit
               --
               (HASH form ((string ?and-unit "and")
                           (meets ?x-unit-in-utterance-right ?and-unit)
                           (meets ?and-unit ?y-unit-in-utterance-left))))
              (?y-in-utterance-unit
               --
               (boundaries (left ?y-unit-in-utterance-left)
                           (right ?y-unit-in-utterance-right))
               (ontology (ontological-class ?ontological-class-utterance-y)))
              (?y-in-world-unit
               --
               (properties
                (contents ((ontological-class ?ontological-class-world-y))))
               (binding-variable ?container-with-y)))
             :feature-types ((ontological-class default :compare-ontological-vectors)))



(def-fcg-cxn ingredients-list-cxn
             ;; match on ingredient + ingredient-list-unit
             ;; match ingredient in utterance to ingredient in world
             ;; meaning: 1x transfer contents
             ;; keep track of input and output container
             ((?new-ingredient-list
               (boundaries (left ?x-unit-in-utterance-left)
                           (right ?ingredient-list-right))
               (subunits (?x-in-utterance-unit ?comma-unit ?ingredient-list-unit))
               (input-args (kitchen-state ?ks-in)
                           (arg1 ?new-container))
               (output-args (kitchen-state ?ks-out)
                            (arg1 ?output-container))
               (meaning ((transfer-contents ?container-mid ?rest-x ?ks-mid ?ks-in ?new-container ?container-with-x ?quantity-x ?unit-x))))
              (?ingredient-list-unit
               (superunit ?new-ingredient-list))
              <-
              (?x-in-utterance-unit
               --
               (boundaries (left ?x-unit-in-utterance-left)
                           (right ?x-unit-in-utterance-right))
               (ontology (ontological-class ?ontological-class-utterance)))
              (?x-in-world-unit
               --
               (properties
                (contents ((ontological-class ?ontological-class-world))))
               (binding-variable ?container-with-x))
              (?comma-unit
               --
               (HASH form ((string ?comma-unit ",")
                           (meets ?x-unit-in-utterance-right ?comma-unit)
                           (meets ?comma-unit ?ingredient-list-left))))
              (?ingredient-list-unit
               --
               (superunit nil)
               (boundaries (left ?ingredient-list-left)
                           (right ?ingredient-list-right))
               (input-args (kitchen-state ?ks-mid)
                           (arg1 ?container-mid))
               (output-args (kitchen-state ?ks-out)
                            (arg1 ?output-container))))
             :feature-types ((ontological-class default :compare-ontological-vectors)))


(def-fcg-cxn large-bowl-cxn
             ;; meaning: fetch
             ;; connect the kitchen state to the input of FETCH
             ;; keep track of the output container
             ((?large-bowl-unit
               (ontology large-bowl)
               (boundaries (left ?large-unit)
                           (right ?bowl-unit))
               (subunits (?large-unit ?bowl-unit))
               (meaning ((fetch ?large-bowl ?ks-out ?ks-in large-bowl 1)))
               (input-args (kitchen-state ?ks-in))
               (output-args (kitchen-state ?ks-out)
                            (arg1 ?large-bowl)))
              <-
              (?ks-unit
               --
               (ontological-class kitchen-state)
               (binding-variable ?ks-in))
              (?large-unit
               --
               (HASH form ((string ?large-unit "large"))))
              (?bowl-unit
               --
               (HASH form ((string ?bowl-unit "bowl"))))
              (?large-bowl-unit
               --
               (HASH form ((meets ?large-unit ?bowl-unit)))))
             :feature-types ((ontology default :lookup-in-ontology)))


(def-fcg-cxn mix-ingredient-list-into-container-cxn
             ;; connect FETCH from container to input of ingredient list
             ;; add MINGLE and connect to output of ingredient list
             ((?clause-unit
               (boundaries (left ?mix-unit)
                           (right ?container-right))
               (subunits (?mix-unit ?ingredient-list-unit ?in-unit ?container-unit))
               (meaning ((mingle ?bowl-with-mixture ?ks-with-mixture ?ks-il-out ?container-il-out ?mingling-tool))))
              <-
              (?mix-unit
               --
               (HASH form ((string ?mix-unit "mix")
                           (meets ?mix-unit ?ingredient-list-left))))
              (?ingredient-list-unit
               --
               (superunit nil)
               (boundaries (left ?ingredient-list-left)
                           (right ?ingredient-list-right))
               (input-args (kitchen-state ?ks-connect)
                           (arg1 ?container-connect))
               (output-args (kitchen-state ?ks-il-out)
                            (arg1 ?container-il-out)))
              (?in-unit
               --
               (HASH form ((string ?into-unit "in")
                           (meets ?ingredient-list-right ?in-unit)
                           (meets ?in-unit ?container-left))))
              (?container-unit
               --
               (ontology (ontological-types (container)))
               (boundaries (left ?container-left)
                           (right ?container-right))
               (input-args (kitchen-state ?ks-container-in))
               (output-args (kitchen-state ?ks-connect)
                            (arg1 ?container-connect)))))



(def-fcg-cxn in-seperate-container-combine-ingredient-list-cxn
             ;; connect FETCH from container to input of ingredient list
             ;; add MIX and connect to output of ingredient list
             ((?clause-unit
               (boundaries (left ?in-unit)
                           (right ?ingredient-list-right))
               (subunits (?in-unit ?separate-unit ?container-unit ?combine-unit ?ingredient-list-unit))
               (meaning ((mix ?dressing ?ks-with-dressing ?ks-il-out ?container-il-out ?mixing-tool))))
              <-
              (?in-unit
               --
               (HASH form ((string ?in-unit "in")
                           (meets ?in-unit ?separate-unit))))
              (?separate-unit
               --
               (HASH form ((string ?separate-unit "separate")
                           (meets ?separate-unit ?container-left))))
              (?container-unit
               --
               (ontology (ontological-types (container)))
               (boundaries (left ?container-left)
                           (right ?container-right))
               (input-args (kitchen-state ?ks-container-in))
               (output-args (kitchen-state ?ks-connect)
                            (arg1 ?container-connect)))
              (?combine-unit
               --
               (HASH form ((string ?combine-unit "combine")
                           (meets ?container-right ?combine-unit)
                           (meets ?combine-unit ?ingredient-list-left))))
              (?ingredient-list-unit
               --
               (superunit nil)
               (boundaries (left ?ingredient-list-left)
                           (right ?ingredient-list-right))
               (input-args (kitchen-state ?ks-connect)
                           (arg1 ?container-connect))
               (output-args (kitchen-state ?ks-il-out)
                            (arg1 ?container-il-out)))))

;; instructions 5 cxns ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(def-fcg-cxn x-mixture-cxn
             ;; find a mixture that contains ingredient x (using ontological vectors)
             ;; match x-in-utterance with one of the contents of a mixture
             ;; percolate the binding variable
             ((?x-mixture-unit
               (boundaries (left ?x-unit-in-utterance-left)
                           (right ?mixture-unit))
               (subunits (?x-in-utterance-unit ?mixture-unit))
               (binding-variable ?container-with-x))
              <-
              (?x-in-utterance-unit
               --
               (boundaries (left ?x-unit-in-utterance-left)
                           (right ?x-unit-in-utterance-right))
               (ontology (ontological-class ?ontological-class-utterance)))
              (?mixture-unit
               --
               (HASH form ((string ?mixture-unit "mixture")
                           (meets ?x-unit-in-utterance-right ?mixture-unit))))
              (?x-mixture-in-world-unit
               --
               (properties
                (contents
                 ((ontological-types (mixture))
                  (properties
                   (components
                    ((ontological-class ?ontological-class-world)))))))
               (binding-variable ?container-with-x)))
              :feature-types ((ontological-class default :compare-ontological-vectors)))


(def-fcg-cxn toss-to-coat-cxn
             ;; add MINGLE and percolate the input/output args
             ((?toss-to-coat-unit
               (boundaries (left ?toss-unit)
                           (right ?coat-unit))
               (subunits (?toss-unit ?to-unit ?coat-unit))
               (meaning ((mingle ?mingled-mix ?ks-out ?ks-in ?container ?tool)))
               (input-args (kitchen-state ?ks-in)
                           (arg1 ?container))
               (output-args (kitchen-state ?ks-out)
                            (arg1 ?mingled-mix)))
              <-
              (?toss-unit
               --
               (HASH form ((string ?toss-unit "toss"))))
              (?to-unit
               --
               (HASH form ((string ?to-unit "to"))))
              (?coat-unit
               --
               (HASH form ((string ?coat-unit "coat"))))
              (?toss-to-coat-unit
               --
               (HASH form ((meets ?toss-unit ?to-unit)
                           (meets ?to-unit ?coat-unit))))))


(def-fcg-cxn pour-implicit-over-x-mixture
             ;; resolve implicit as a homogeneous mixture
             ;; pour over other ingredient x
             ((?pour-over-x-unit
               (boundaries (left ?pour-unit)
                           (right ?x-mixture-right))
               (subunits (?pour-unit ?over-unit ?x-mixture-unit))
               (meaning ((transfer-contents ?output-container ?rest ?ks-out ?ks-in ?container-with-x ?container-with-ellipsis ?quantity ?unit)))
               (input-args (kitchen-state ?ks-in)
                           (arg1 ?container-with-x))
               (output-args (kitchen-state ?ks-out)
                            (arg1 ?output-container)))
              <-
              (?ks-unit
               --
               (ontological-class kitchen-state)
               (binding-variable ?ks-in))
              (?pour-unit
               --
               (HASH form ((string ?pour-unit "pour")
                           (meets ?pour-unit ?over-unit))))
              (?ellipsis-in-world-unit
               --
               (properties
                (contents
                 ((ontological-class homogeneous-mixture))))
               (binding-variable ?container-with-ellipsis))
              (?over-unit
               --
               (HASH form ((string ?over-unit "over")
                           (meets ?over-unit ?x-mixture-left))))
              (?x-mixture-unit
               --
               (boundaries (left ?x-mixture-left)
                           (right ?x-mixture-right))
               (binding-variable ?container-with-x))))



(def-fcg-cxn X-and-Y-actions-cxn
             ;; connect actions X and Y together in terms of kitchen states
             ((?clause-unit
               (boundaries (left ?clause-x-left)
                           (right ?clause-y-right))
               (subunits (?clause-X-unit ?and-unit ?clause-Y-unit)))
              <-
              (?clause-X-unit
               --
               (boundaries (left ?clause-x-left)
                           (right ?clause-x-right))
               (output-args (kitchen-state ?ks-connect)
                            (arg1 ?container-connect)))
              (?and-unit
               --
               (HASH form ((string ?and-unit "and")
                           (meets ?clause-x-right ?and-unit)
                           (meets ?and-unit ?clause-y-left))))
              (?clause-Y-unit
               --
               (boundaries (left ?clause-y-left)
                           (right ?clause-y-right))
               (input-args (kitchen-state ?ks-connect)
                           (arg1 ?container-connect)))))

;; instruction 6 cxns ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(def-fcg-cxn store-in-the-refrigerator-cxn
             ((?clause-unit
               (boundaries (left ?store)
                           (right ?refrigerator))
               (meaning ((refrigerate ?container-out ?ks-out ?ks-in ?container-with-ellipsis ?fridge 24 hour))))
              <-
              (?ks-unit
               --
               (ontological-class kitchen-state)
               (binding-variable ?ks-in))
              (?clause-unit
               --
               (HASH form ((string ?best-unit-1 "best")
                           (string ?if-unit-1 "if")
                           (string ?made-unit-1 "made")
                           (string ?a-unit-1 "a")
                           (string ?day-unit-1 "day")
                           (string ?ahead-unit-1 "ahead")
                           (string ?and-unit-1 "and")
                           (string ?stored-unit-1 "stored")
                           (string ?in-unit-1 "in")
                           (string ?the-unit-1 "the")
                           (string ?refrigerator-unit-1 "refrigerator")
                           (meets ?best-unit-1 ?if-unit-1)
                           (meets ?if-unit-1 ?made-unit-1)
                           (meets ?made-unit-1 ?a-unit-1)
                           (meets ?a-unit-1 ?day-unit-1)
                           (meets ?day-unit-1 ?ahead-unit-1)
                           (meets ?ahead-unit-1 ?and-unit-1)
                           (meets ?and-unit-1 ?stored-unit-1)
                           (meets ?stored-unit-1 ?in-unit-1)
                           (meets ?in-unit-1 ?the-unit-1)
                           (meets ?the-unit-1 ?refrigerator-unit-1))))
              (?ellipsis-in-world
               --
               (properties
                (contents
                 ((ontological-types (mixture)))))
               (binding-variable ?container-with-ellipsis))))


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
                            "mix broccoli , onions , bacon and mozzarella in large bowl"  ;; => mix INGREDIENT-LIST in CONTAINER
                            "in separate large bowl combine vinegar , sugar and mayo"  ;; => in separate CONTAINER combine INGREDIENT-LIST
                            "pour over broccoli mixture and toss to coat"  ;; => pour IMPLICIT over 
                            "best if made a day ahead and stored in the refrigerator"
                            
                            "end"
                            )
                            ; *pdm*
                          (initialise-personal-dynamic-memory
                           *fcg-constructions*
                           *init-op*)
                          )
    (append *init-op* meaning-network)))