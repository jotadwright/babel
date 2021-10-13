;(ql:quickload :aipp-cookingbot)

(in-package :aipp-cookingbot)

(setf *initial-kitchen-state*
      (make-instance 'kitchen-state
                     :contents
                     (list
                      (make-instance 'counter-top
                                     :contents (list (make-instance 'large-bowl)))
                      (make-instance 'pantry
                                     :contents (list (make-instance 'white-sugar :amount
                                                                    (make-instance 'amount
                                                                                   :unit (make-instance 'g)
                                                                                   :quantity (make-instance 'quantity
                                                                                                            :value 250)))
                                                     (make-instance 'powdered-white-sugar :amount
                                                                    (make-instance 'amount
                                                                                   :unit (make-instance 'g)
                                                                                   :quantity (make-instance 'quantity
                                                                                                            :value 20)))
                                                     (make-instance 'all-purpose-flour :amount
                                                                    (make-instance 'amount
                                                                                   :unit (make-instance 'g)
                                                                                   :quantity (make-instance 'quantity
                                                                                                            :value 250)))))
                      (make-instance 'fridge
                                     :contents (list (make-instance 'butter :amount
                                                                    (make-instance 'amount
                                                                                   :unit (make-instance 'g)
                                                                                   :quantity (make-instance 'quantity
                                                                                                            :value 250)))
                                                     (make-instance 'egg :amount
                                                                    (make-instance 'amount
                                                                                   :unit (make-instance 'piece)
                                                                                   :quantity (make-instance 'quantity
                                                                                                            :value 4))))))))
      

(add-element (make-html *initial-kitchen-state* :expand-initially nil))

(def-irl-primitives muhai-irl-tutorial)







;; Primitives ;;
;;;;;;;;;;;;;;;;

(defprimitive get-kitchen-state ((kitchen-state kitchen-state))
  ((=> kitchen-state)
   (bind (kitchen-state 1.0 *initial-kitchen-state*))))

(activate-monitor trace-irl)
;; (evaluate-irl-program '((get-kitchen-state ?kitchen-state)) nil)

(defprimitive fetch ((fetched-ingredient ingredient)
                     (kitchen-after-fetching kitchen-state)
                     (kitchen-before-fetching kitchen-state)
                     (concept-of-ingredient-to-fetch t))
  ((kitchen-before-fetching concept-of-ingredient-to-fetch => kitchen-after-fetching fetched-ingredient)
   (loop with new-kitchen-state = (copy-object kitchen-before-fetching)
         for ingredient in (append (contents (fridge new-kitchen-state))
                                   (contents (pantry new-kitchen-state)))
         when (typep ingredient (type-of concept-of-ingredient-to-fetch))
         do
         (setf (contents (counter-top new-kitchen-state))
               (cons ingredient (contents (counter-top new-kitchen-state))))
         (setf (contents (fridge new-kitchen-state))
               (remove ingredient (contents (fridge new-kitchen-state))))
         (return
          (bind (fetched-ingredient 1.0 ingredient)
                (kitchen-after-fetching 1.0 new-kitchen-state))))))

(defprimitive melt ((bowl-with-melted-ingredient container)
                    (kitchen-after-melting kitchen-state)
                    (kitchen-before-melting kitchen-state)
                    (ingredient-to-melt ingredient))
  ((kitchen-before-melting ingredient-to-melt => kitchen-after-melting bowl-with-melted-ingredient)
   (let* ((new-kitchen-state (copy-object kitchen-before-melting))
          (ingredient (find-object-by-persistent-id ingredient-to-melt (counter-top new-kitchen-state)))
          (bowl (find 'large-bowl (contents (counter-top new-kitchen-state))
                      :test #'(lambda (symbol object)
                                (subtypep object symbol))
                      :key #'type-of)))

     (setf (contents bowl) (cons ingredient (contents bowl)))
     (setf (contents (counter-top new-kitchen-state))
           (remove ingredient (contents (counter-top new-kitchen-state))))
     (setf (melted ingredient) t)
     (bind (bowl-with-melted-ingredient 1.0 bowl)
           (kitchen-after-melting 1.0 new-kitchen-state)))))

(defprimitive add ((output-container container)
                   (kitchen-after-adding kitchen-state)
                   (kitchen-before-adding kitchen-state)
                   (ingredient-to-be-added ingredient)
                   (input-container container))
  ((kitchen-before-adding ingredient-to-be-added input-container => output-container kitchen-after-adding )
   (let* ((new-kitchen-state (copy-object kitchen-before-adding))
          (ingredient (find-object-by-persistent-id ingredient-to-be-added (counter-top new-kitchen-state)))
          (bowl (find-object-by-persistent-id input-container (counter-top new-kitchen-state))))
     (setf (contents bowl)
           (append (contents bowl) (list ingredient)))
     (setf (contents (counter-top new-kitchen-state))
           (remove ingredient (contents (counter-top new-kitchen-state))))
     (bind (output-container 1.0 bowl)
           (kitchen-after-adding 1.0 new-kitchen-state)))))


(defprimitive mix ((output-container container)
                   (kitchen-after-mixing kitchen-state)
                   (kitchen-before-mixing kitchen-state)
                   (input-container container))
  ((kitchen-before-mixing input-container => output-container kitchen-after-mixing)
   (let* ((new-kitchen-state (copy-object kitchen-before-mixing))
          (bowl (find-object-by-persistent-id input-container (counter-top new-kitchen-state))))
     (create-homogeneous-mixture-in-container bowl)
     (bind (output-container 1.0 bowl)
           (kitchen-after-mixing 1.0 new-kitchen-state)))))

(defprimitive bake ((output-container container)
                   (kitchen-after-baking kitchen-state)
                   (kitchen-before-baking kitchen-state)
                   (input-container container)
                   (time-quantity quantity)
                   (time-unit unit)
                   (temp-quantity quantity)
                   (temp-unit unit))
  ((kitchen-before-baking input-container time-quantity time-unit
                          => output-container kitchen-after-baking temp-quantity temp-unit)
   (let* ((new-kitchen-state (copy-object kitchen-before-baking))
          (bowl (find-object-by-persistent-id input-container (counter-top new-kitchen-state)))
          (quantity-temp (make-instance 'quantity :value 180))
          (unit-temp (make-instance 'degrees-celsius))
          (temperature (make-instance 'amount
                                      :quantity quantity-temp
                                      :unit unit-temp)))

     (fast-forward)
     (setf (baked (first (contents bowl))) t)
     (setf (temperature (first (contents bowl))) temperature)
     (bind (output-container 1.0 bowl)
           (kitchen-after-baking 1.0 new-kitchen-state)
           (temp-quantity 0.0 quantity-temp)
           (temp-unit 0.0 unit-temp)))))


(defun fast-forward ()
  (format nil "Ready!"))


(draw-recipe `((get-kitchen-state ?ks-1)
               (fetch ?butter ?ks-2
                      ?ks-1 ,butter-concept)
               (fetch ?eggs ?ks-3
                      ?ks-2 ,egg-concept)
               (fetch ?sugar ?ks-4
                      ?ks-3 ,sugar-concept)
               (fetch ?flour ?ks-5
                      ?ks-4 ,flour-concept)
               (melt ?bowl-1 ?ks-6 ?ks-5
                     ?butter)
               (add ?bowl-2 ?ks-7
                    ?ks-6 ?sugar
                    ?bowl-1)
               (add ?bowl-3 ?ks-8
                    ?ks-7 ?eggs
                    ?bowl-2)
               (add ?bowl-4 ?ks-9
                    ?ks-8 ?flour
                    ?bowl-3)
               (mix ?bowl-5 ?ks-10 ?ks-9 ?bowl-4)
               (bake ?bowl-6 ?ks-11 ?ks-10 ?bowl-5 45 minute ?temp-quantity ?temp-unit)))



#|
 (progn
 (defparameter butter-concept (make-instance 'butter :is-concept t))
 (defparameter egg-concept (make-instance 'egg :is-concept t))
 (defparameter sugar-concept (make-instance 'white-sugar :is-concept t))
 (defparameter flour-concept (make-instance 'all-purpose-flour :is-concept t))
 (defparameter minute-concept (make-instance 'minute :is-concept t))
 (defparameter quantity-45-concept (make-instance 'quantity :value 45))
 
 (evaluate-irl-program `((get-kitchen-state ?ks-1)
                         (fetch ?butter ?ks-2
                                ?ks-1 ,butter-concept)
                         (fetch ?eggs ?ks-3
                                ?ks-2 ,egg-concept)
                         (fetch ?sugar ?ks-4
                                ?ks-3 ,sugar-concept)
                         (fetch ?flour ?ks-5
                                ?ks-4 ,flour-concept)
                         (melt ?bowl-1 ?ks-6 ?ks-5
                               ?butter)
                         (add ?bowl-2 ?ks-7
                              ?ks-6 ?sugar
                              ?bowl-1)
                         (add ?bowl-3 ?ks-8
                              ?ks-7 ?eggs
                              ?bowl-2)
                         (add ?bowl-4 ?ks-9
                              ?ks-8 ?flour
                              ?bowl-3)
                         (mix ?bowl-5 ?ks-10 ?ks-9 ?bowl-4)
                         (bake ?bowl-6 ?ks-11 ?ks-10 ?bowl-5 ,quantity-45-concept ,minute-concept ?temp-quantity ?temp-unit))
                       nil))
             
|#





(def-fcg-constructions cake-grammar
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
                  (footprints set))
  :fcg-configurations ((:de-render-mode . :de-render-recipe-utterance)
                       (:parse-goal-tests :no-applicable-cxns :no-strings-in-root :connected-structure)
                       (:construction-inventory-processor-mode . :heuristic-search)
                       (:node-expansion-mode . :full-expansion)
                       (:cxn-supplier-mode . :all-cxns)
                       (:search-algorithm . :best-first)
                       (:heuristics :nr-of-applied-cxns :ontological-distance :nr-of-units-matched)
                       (:heuristic-value-mode . :sum-heuristics-and-parent))
  :visualization-configurations ((:hide-features nil)
                                 (:show-constructional-dependencies . nil))

  ;; Units ;;
  ;;;;;;;;;;;


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

  (def-fcg-cxn butter-cxn
               ((?butter-unit
                 (ontology butter)
                 (boundaries (left ?butter-unit)
                             (right ?butter-unit)))
                <-
                (?butter-unit
                 --
                 (HASH form ((string ?butter-unit "butter")))))
               :feature-types ((ontology default :lookup-in-ontology)))

   (def-fcg-cxn sugar-cxn
               ((?sugar-unit
                 (ontology white-sugar)
                 (boundaries (left ?sugar-unit)
                             (right ?sugar-unit)))
                <-
                (?sugar-unit
                 --
                 (HASH form ((string ?sugar-unit "sugar")))))
               :feature-types ((ontology default :lookup-in-ontology)))

   (def-fcg-cxn powdered-sugar-cxn
                ((?powdered-sugar-unit
                  (ontology powdered-white-sugar)
                  (subunits (?powdered-unit ?sugar-unit))
                  (boundaries (left ?powdered-unit)
                              (right ?sugar-unit)))
                 <-
                 (?powdered-unit
                  --
                  (HASH form ((string ?powdered-unit "powdered"))))
                 (?sugar-unit
                  --
                  (HASH form ((string ?sugar-unit "sugar"))))
                 (?powdered-sugar-unit
                  --
                  (HASH form ((meets ?powdered-unit ?sugar-unit)))))
                :feature-types ((ontology default :lookup-in-ontology)))

  (def-fcg-cxn flour-cxn
               ((?flour-unit
                 (ontology all-purpose-flour)
                 (boundaries (left ?flour-unit)
                             (right ?flour-unit)))
                <-
                (?flour-unit
                 --
                 (HASH form ((string ?flour-unit "flour")))))
               :feature-types ((ontology default :lookup-in-ontology)))

  (def-fcg-cxn eggs-cxn
               ((?eggs-unit
                 (ontology egg)
                 (boundaries (left ?eggs-unit)
                             (right ?eggs-unit)))
                <-
                (?eggs-unit
                 --
                 (HASH form ((string ?eggs-unit "eggs")))))
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
                 (boundaries (left ?unit-unit-left)
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
                 (meaning ((fetch ?ingredient-out ?kitchen-state-out ?kitchen-state-in ?ingredient-in)))
                 --
                 (HASH form ((meets ?quantity-unit-right ?unit-unit-left)
                             (meets ?unit-unit-right ?ingredient-unit-left))))))

  (def-fcg-cxn quantity-ingredient-cxn
               ((?noun-phrase-unit
                 (phrase-type noun-phrase)
                 (subunits (?quantity-unit ?ingredient-unit))
                 (input-args (kitchen-state ?kitchen-state-in)
                             (args (?ingredient-in)))
                 (output-args (kitchen-state ?kitchen-state-out)
                              (args (?ingredient-out)))
                 (ontology (ontological-class ?ingredient)
                           (ontological-types (ingredient)))
                 (boundaries (left ?unit-unit-left)
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
                (?ingredient-unit
                 --
                 (ontology (ontological-class ?ingredient-in)
                           (ontological-types (ingredient)))
                 (boundaries (left ?ingredient-unit-left)
                             (right ?ingredient-unit-right)))
                (?noun-phrase-unit
                 (meaning ((fetch ?ingredient-out ?kitchen-state-out ?kitchen-state-in ?ingredient-in)))
                 --
                 (HASH form ((meets ?quantity-unit-right ?ingredient-unit-left))))))


  (def-fcg-cxn melt-cxn
               ((?melt-unit
                 (ontology meltable)
                 (input-args (kitchen-state ?input-kitchen-state)
                             (arg1 ?input-container))
                 (output-args (kitchen-state ?output-kitchen-state)
                              (arg1 ?output-container))
                 (boundaries (left ?melt-unit)
                             (right ?melt-unit)))
                <-
                (?melt-unit
                 (HASH meaning ((melt ?output-container ?output-kitchen-state ?input-kitchen-state ?input-container)))
                 --
                 (HASH form ((string ?melt-unit "melt")))))
               :feature-types ((ontology default :lookup-in-ontology)))

  (def-fcg-cxn the-x-perfect-match-cxn
               ((?the-x-unit
                 (referent (args (?ingredient-in-world)))
                 (ontology (ontological-class ?ontological-class-utterance)
                           (ontological-types ?ontological-types))
                 (subunits (?the-unit ?x-unit-in-utterance))
                 (boundaries (left ?the-unit)
                             (right ?x-unit-in-utterance-right)))
                <-
                (?the-unit
                 --
                 (HASH form ((string ?the-unit "the"))))
                (?the-x-unit
                 --
                 (HASH form ((meets ?the-unit ?x-unit-in-utterance-left))))
                (?x-unit-in-utterance
                 --
                 (boundaries (left ?x-unit-in-utterance-left)
                             (right ?x-unit-in-utterance-right))
                 (ontology (ontological-class ?ontological-class-utterance)
                           (ontological-types ?ontological-types)))
                (?x-unit-in-world
                 --
                 (ontological-class ?ontological-class-utterance)
                 (binding-variable ?ingredient-in-world)))
               )

  (def-fcg-cxn the-x-approximate-match-cxn
               ((?the-x-unit
                 (referent (args (?container-with-x)))
                 (ontology (ontological-class ?ontological-class-utterance)
                           (ontological-types ?ontological-types))
                 (subunits (?the-unit ?x-unit-in-utterance))
                 (boundaries (left ?the-unit)
                             (right ?x-unit-in-utterance-right)))
                <-
                (?the-unit
                 --
                 (HASH form ((string ?the-unit "the"))))
                (?x-unit-in-utterance
                 --
                 (boundaries (left ?x-unit-in-utterance-left)
                             (right ?x-unit-in-utterance-right))
                 (ontology (ontological-class ?ontological-class-utterance)
                           (ontological-types ?ontological-types)))
                (?x-unit-in-world
                 --
                 (binding-variable ?container-with-x)
                 (properties (contents ((ontological-class ?ontological-class-world)))))
                (?the-x-unit
                 --
                 (HASH form ((meets ?the-unit ?x-unit-in-utterance-left)))))
               :feature-types ((ontological-class default :compare-ontological-vectors)))

  (def-fcg-cxn verb-x-imperative-transitive-cxn
               ((?imperative-transitive-unit
                 (lex-class clause)
                 (subunits (?verb-unit ?ingredient-unit))
                 (input-args (args (?input-container))
                             (kitchen-state ?input-kitchen-state))
                 (output-args (args (?output-container))
                              (kitchen-state ?output-kitchen-state))
                 (boundaries (left ?verb-unit)
                             (right ?ingredient-unit-right)))
                 
                <-
                (?verb-unit
                 --
                 (ontology (ontological-class ?property))
                 (boundaries (left ?verb-unit-left)
                             (right ?verb-unit-right))
                 (input-args (kitchen-state ?input-kitchen-state)
                             (arg1 ?input-container))
                 (output-args (kitchen-state ?output-kitchen-state)
                             (arg1 ?output-container)))
                (?ingredient-unit
                 --
                 (referent (args (?input-container)))
                 (ontology (ontological-types (?property)))
                 (boundaries (left ?ingredient-unit-left)
                             (right ?ingredient-unit-right)))
                (?kitchen-state
                 --
                 (ontological-class kitchen-state)
                 (binding-variable ?input-kitchen-state))
                (?imperative-transitive-unit
                 --
                 (HASH form ((meets ?verb-unit-right ?ingredient-unit-left))))))



  (def-fcg-cxn add-x-comma-y-and-z-imperative-transitive-cxn
               ((?imperative-transitive-unit
                 (lex-class clause)
                 (subunits (?add-unit ?ingredient-x-unit ?comma-unit ?ingredient-y-unit ?and-unit ?ingredient-z-unit))
                 (input-args (args (?ingredient-x ?ingredient-y ?ingredient-z ?bowl-1))
                             (kitchen-state ?input-kitchen-state))
                 (output-args (args (?output-container))
                              (kitchen-state ?output-kitchen-state))
                 (boundaries (left ?add-unit)
                             (right ?ingredient-unit-right-z))
                 (meaning ((add ?bowl-2 ?ks-7 ?input-kitchen-state ?ingredient-x ?bowl-1)
                           (add ?bowl-3 ?ks-8 ?ks-7 ?ingredient-y ?bowl-2)
                           (add ?output-container ?output-kitchen-state ?ks-8 ?ingredient-z ?bowl-3))))
                <-
                (?kitchen-state
                 --
                 (ontological-class kitchen-state)
                 (binding-variable ?input-kitchen-state))
                (?bowl-unit-in-world
                 --
                 (ontological-class large-bowl)
                 (binding-variable ?bowl-1))
                (?add-unit
                 --
                 (HASH form ((string ?add-unit "add"))))                
                (?ingredient-x-unit
                 --
                 (referent (args (?ingredient-x)))
                 (ontology (ontological-types (ingredient)))
                 (boundaries (left ?ingredient-unit-left-x)
                             (right ?ingredient-unit-right-x)))
                (?comma-unit
                 --
                 (HASH form ((string ?comma-unit ","))))
                (?ingredient-y-unit
                 --
                 (referent (args (?ingredient-y)))
                 (ontology (ontological-types (ingredient)))
                 (boundaries (left ?ingredient-unit-left-y)
                             (right ?ingredient-unit-right-y)))
                (?and-unit
                 --
                 (HASH form ((string ?and-unit "and"))))
                (?ingredient-z-unit
                 --
                 (referent (args (?ingredient-z)))
                 (ontology (ontological-types (ingredient)))
                 (boundaries (left ?ingredient-unit-left-z)
                             (right ?ingredient-unit-right-z)))
                
                (?imperative-transitive-unit
                 --
                 (HASH form ((meets ?add-unit ?ingredient-unit-left-x)
                             (meets ?ingredient-unit-right-x ?comma-unit)
                             (meets ?comma-unit ?ingredient-unit-left-y)
                             (meets ?ingredient-unit-right-y ?and-unit)
                             (meets ?and-unit ?ingredient-unit-left-z))))))

  (def-fcg-cxn dough-cxn
               ((?dough-unit
                 (ontology dough)
                 (boundaries (left ?dough-unit)
                             (right ?dough-unit)))
                <-
                (?dough-unit
                 --
                 (HASH form ((string ?dough-unit "dough")))))
               :feature-types ((ontology default :lookup-in-ontology)))

  (def-fcg-cxn mix-cxn
               ((?mix-unit
                 (ontology mixable)
                 (input-args (kitchen-state ?input-kitchen-state)
                             (arg1 ?input-container))
                 (output-args (kitchen-state ?output-kitchen-state)
                              (arg1 ?output-container))
                 (boundaries (left ?mix-unit)
                             (right ?mix-unit)))
                <-
                (?mix-unit
                 (HASH meaning ((mix ?output-container ?output-kitchen-state
                                     ?input-kitchen-state ?input-container)))
                 --
                 (HASH form ((string ?mix-unit "mix")))))
               :feature-types ((ontology default :lookup-in-ontology)))


  (def-fcg-cxn bake-cxn
               ((?bake-unit
                 (ontology bakeable)
                 (input-args (kitchen-state ?input-kitchen-state)
                             (arg1 ?input-container)
                             (arg-time-quantity ?time-quantity)
                             (arg-time-unit ?time-unit)
                             (arg-temp-quantity ?temp-quantity)
                             (arg-temp-unit ?temp-unit)
                             )
                 (output-args (kitchen-state ?output-kitchen-state)
                              (arg1 ?output-container))
                 (boundaries (left ?bake-unit)
                             (right ?bake-unit)))
                <-
                (?bake-unit
                 (HASH meaning ((bake ?output-container ?output-kitchen-state ?input-kitchen-state
                                      ?input-container ?time-quantity ?time-unit
                                      ?temp-quantity ?temp-unit)))
                 --
                 (HASH form ((string ?bake-unit "bake")))))
               :feature-types ((ontology default :lookup-in-ontology)))

  (def-fcg-cxn verb-well-cxn
               ((?clause-unit
                 (subunits (?verb-unit ?well-unit)))
                <-
                (?verb-unit
                 --
                 (input-args (kitchen-state ?input-kitchen-state)
                             (arg1 ?input-container))
                 (output-args (kitchen-state ?output-kitchen-state)
                              (arg1 ?output-container))
                 (boundaries (left ?verb-unit-left)
                             (right ?verb-unit-right)))
                (?bowl-unit-in-world
                 --
                 (ontological-class large-bowl)
                 (binding-variable ?input-container))
                (?kitchen-state
                 --
                 (ontological-class kitchen-state)
                 (binding-variable ?input-kitchen-state))
                
                (?well-unit
                 --
                 (HASH form ((string ?well-unit "well"))))
                (?clause-unit
                 --
                 (HASH form ((meets ?verb-unit-right ?well-unit))))))

  (def-fcg-cxn for-x-minutes-cxn
               ((?pp-unit
                 (referent (time-quantity ?quantity)
                           (time-unit minute))
                 (subunits (?for-unit ?quantity-unit ?minutes-unit))
                 (boundaries (left ?for-unit)
                             (right ?minutes-unit)))
                <-
                (?kitchen-state
                 --
                 (ontological-class kitchen-state)
                 (binding-variable ?input-kitchen-state))
                (?for-unit
                 --
                 (HASH form ((string ?for-unit "for"))))
                (?quantity-unit
                 --
                 (value ?quantity)
                 (ontology (ontological-class quantity))
                 (boundaries (left ?quantity-unit-left)
                             (right ?quantity-unit-right)))
                (?minutes-unit
                 --
                 (HASH form ((string ?minutes-unit "minutes"))))
                (?pp-unit
                 --
                 (HASH form ((meets ?for-unit ?quantity-unit-left)
                             (meets ?quantity-unit-right ?minutes-unit))))))


  (def-fcg-cxn temporal-modification-cxn
               ((?clause-unit
                 (subunits (?pp-unit)))
                <-
                (?clause-unit
                 --
                 (subunits (?verb-unit))
                 (boundaries (right ?clause-unit-right))
                 (HASH form ((meets ?clause-unit-right ?pp-unit-left))))
                (?verb-unit
                 --
                 (input-args (arg-time-quantity ?time-quantity)
                             (arg-time-unit ?time-unit)))
                (?pp-unit
                 --
                 (referent (time-quantity ?time-quantity)
                           (time-unit ?time-unit))
                 (boundaries (left ?pp-unit-left)
                             (right ?pp-unit-right)))))
                
  )

  

 

(activate-monitor trace-fcg)
(clear-page)
  
(process-uterances '(;; Ingredients
                     "250 grams butter"
                     "4 eggs"
                     "250 grams sugar"
                     "250 grams flour"
                     "20 grams powdered sugar"

                     ;; Instructions
                     "melt the butter"
                     "add the sugar , the eggs and the flour"
                     "mix well"
                     "bake the dough for 45 minutes"
                     "end"
                     )
                   (initialise-personal-dynamic-memory
                    *fcg-constructions*
                    `((get-kitchen-state ,(make-var 'kitchen-state)))))


