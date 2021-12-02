;(ql:quickload :muhai-cookingbot)

(in-package :muhai-cookingbot)


(defparameter *initial-kitchen-state* 
  (make-instance 
   'kitchen-state
   :contents
   (list (make-instance 'fridge
                        :contents (list (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'butter
                                                                                      :temperature
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'degrees-celsius)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 5))
                                                                                      :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 500)))))))
         (make-instance 'pantry
                        :contents (list (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'white-sugar :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 1000)))))
                                        (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'vanilla-extract :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 100)))))
                                        (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'almond-extract :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 100)))))
                                        (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'all-purpose-flour :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 1000)))))
                                        (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'almond-flour :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 1000)))))
                                        (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'powdered-white-sugar :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 500)))))))
         (make-instance 'kitchen-cabinet
                        :contents (list
                                   ;; bowls
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)

                                   ;; tools
                                   (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                   (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                   (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)

                                   ;; baking equipment
                                   (make-instance 'baking-tray)
                                   (make-instance 'baking-paper))))))

(def-fcg-constructions almond-cookies-grammar
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
                       (:heuristic-value-mode . :sum-heuristics-and-parent)
                       
                       )
  :visualization-configurations  ((:hide-features nil)
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

  (def-fcg-cxn gram-morph-cxn
               ((?gram-unit
                 (lex-id gram))
                <-
                (?gram-unit
                 --
                 (HASH form ((string ?gram-unit "gram"))))))

  (def-fcg-cxn g-morph-cxn
               ((?gram-unit
                 (lex-id gram))
                <-
                (?gram-unit
                 --
                 (HASH form ((string ?gram-unit "g"))))))


 (def-fcg-cxn degrees-celsius-cxn
               ((?degrees-celsius-unit
                 (ontology degrees-celsius)
                 (boundaries (left ?degrees-celsius-unit)
                             (right ?degrees-celsius-unit)))
                <-
                (?degrees-celsius-unit
                 --
                 (lex-id degrees-celsius)))
               :feature-types ((ontology default :lookup-in-ontology)))

 (def-fcg-cxn °C-morph-cxn
               ((?degrees-celsius-unit
                 (lex-id degrees-celsius))
                <-
                (?degrees-celsius-unit
                 --
                 (HASH form ((string ?degrees-celsius-unit "°C"))))))
 

  (def-fcg-cxn cup-cxn
               ((?cup-unit
                 (ontology cup)
                 (boundaries (left ?cup-unit)
                             (right ?cup-unit)))
                <-
                (?cup-unit
                 --
                 (lex-id cup)))
               :feature-types ((ontology default :lookup-in-ontology)))

  (def-fcg-cxn cups-morph-cxn
               ((?cups-unit
                 (lex-id cup))
                <-
                (?cups-unit
                 --
                 (HASH form ((string ?cups-unit "cups"))))))

  (def-fcg-cxn cup-morph-cxn
               ((?cup-unit
                 (lex-id cup))
                <-
                (?cup-unit
                 --
                 (HASH form ((string ?cup-unit "cup"))))))


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


  (def-fcg-cxn minute-cxn
               ((?minute-unit
                 (ontology minute)
                 (boundaries (left ?minute-unit)
                             (right ?minute-unit)))
                <-
                (?minute-unit
                 --
                 (lex-id minute)))
               :feature-types ((ontology default :lookup-in-ontology)))

  (def-fcg-cxn minutes-morph-cxn
               ((?minutes-unit
                 (lex-id minute))
                <-
                (?minutes-unit
                 --
                 (HASH form ((string ?minutes-unit "minutes"))))))

  (def-fcg-cxn minute-morph-cxn
               ((?minute-unit
                 (lex-id minute))
                <-
                (?minute-unit
                 --
                 (HASH form ((string ?minute-unit "minute"))))))

  
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



  
;; Ingredients ;;
;;;;;;;;;;;;;;;;;


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

(def-fcg-cxn white-sugar-cxn
             ((?white-sugar-unit
               (ontology white-sugar)
               (boundaries (left ?white-unit)
                             (right ?sugar-unit))
               (subunits (?white-unit ?sugar-unit)))
              <-
              (?white-unit
               --
               (HASH form ((string ?white-unit "white"))))
              (?sugar-unit
               --
               (HASH form ((string ?sugar-unit "sugar"))))
              (?white-sugar-unit
               --
               (HASH form ((meets ?white-unit ?sugar-unit)))))
             :feature-types ((ontology default :lookup-in-ontology)))

(def-fcg-cxn powdered-sugar-cxn
             ((?powdered-sugar-unit
               (ontology powdered-white-sugar)
               (boundaries (left ?powdered-unit)
                           (right ?sugar-unit))
               (subunits (?powdered-unit ?sugar-unit)))
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

  (def-fcg-cxn almond-extract-lex-cxn
               ((?almond-extract-unit
                 (ontology almond-extract))
                <-
                (?almond-extract-unit
                 --
                 (lex-id almond-extract)))
               :feature-types ((ontology default :lookup-in-ontology)))

  (def-fcg-cxn almond-extract-morph-cxn
               ((?almond-extract-unit
                 (lex-id almond-extract)
                 (boundaries (left ?almond-unit)
                             (right ?extract-unit))
                 (subunits (?almond-unit ?extract-unit)))
                <-
                (?almond-unit
                 --
                 (HASH form ((string ?almond-unit "almond"))))
                (?extract-unit
                 --
                 (HASH form ((string ?extract-unit "extract"))))
                (?almond-extract-unit
                 --
                 (HASH form ((meets ?almond-unit ?extract-unit))))))

  (def-fcg-cxn almond-flour-cxn
               ((?almond-flour-unit
                 (ontology almond-flour)
                 (boundaries (left ?almond-unit)
                             (right ?flour-unit))
                 (subunits (?almond-unit ?flour-unit)))
                <-
                (?almond-unit
                 --
                 (HASH form ((string ?almond-unit "almond"))))
                (?flour-unit
                 --
                 (HASH form ((string ?flour-unit "flour"))))
                (?almond-flour-unit
                 --
                 (HASH form ((meets ?almond-unit ?flour-unit)))))
               :feature-types ((ontology default :lookup-in-ontology)))

  (def-fcg-cxn vanilla-cxn
               ((?vanilla-unit
                 (ontology vanilla)
                 (boundaries (left ?vanilla-unit)
                             (right ?vanilla-unit)))
                <-
                (?vanilla-unit
                 --
                 (HASH form ((string ?vanilla-unit "vanilla")))))
               :feature-types ((ontology default :lookup-in-ontology)))

  (def-fcg-cxn almond-cxn
               ((?almond-unit
                 (ontology almond)
                 (boundaries (left ?almond-unit)
                             (right ?almond-unit)))
                <-
                (?almond-unit
                 --
                 (HASH form ((string ?almond-unit "almond")))))
               :feature-types ((ontology default :lookup-in-ontology)))
  

  (def-fcg-cxn vanilla-extract-lex-cxn
               ((?vanilla-extract-unit
                 (ontology vanilla-extract))
                <-
                (?vanilla-extract-unit
                 --
                 (lex-id vanilla-extract)))
               :feature-types ((ontology default :lookup-in-ontology)))

  (def-fcg-cxn vanilla-extract-morph-cxn
               ((?vanilla-extract-unit
                 (lex-id vanilla-extract)
                 (subunits (?vanilla-unit ?extract-unit))
                 (boundaries (left ?vanilla-unit)
                             (right ?extract-unit)))
                <-
                (?vanilla-unit
                 --
                 (HASH form ((string ?vanilla-unit "vanilla"))))
                (?extract-unit
                 --
                 (HASH form ((string ?extract-unit "extract"))))
                (?vanilla-extract-unit
                 --
                 (HASH form ((meets ?vanilla-unit ?extract-unit))))))


  (def-fcg-cxn extract-cxn
               ((?extract-unit
                 (ontology flavoring-extract)
                 (boundaries (left ?extract-unit)
                             (right ?extract-unit)))
                <-
                (?extract-unit
                 --
                 (lex-id extract)))
               :feature-types ((ontology default :lookup-in-ontology)))

  (def-fcg-cxn extracts-morph-cxn
               ((?extracts-unit
                 (lex-id extract))
                <-
                (?extracts-unit
                 --
                 (HASH form ((string ?extracts-unit "extracts"))))))

  (def-fcg-cxn extract-morph-cxn
               ((?extract-unit
                 (lex-id extract))
                <-
                (?extract-unit
                 --
                 (HASH form ((string ?extract-unit "extract"))))))

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


;; Actions ;;
;;;;;;;;;;;;;


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
                 (HASH meaning ((to-melt ?output-container ?output-kitchen-state ?input-kitchen-state ?input-container)))
                 --
                 (HASH form ((string ?melt-unit "melt")))))
               :feature-types ((ontology default :lookup-in-ontology)))

  (def-fcg-cxn beat-cxn
               ((?beat-unit
                 (ontology beatable)
                 (input-args (kitchen-state ?input-kitchen-state)
                             (arg1 ?input-container)
                             (arg3 ?tool))
                 (output-args (kitchen-state ?output-kitchen-state)
                              (arg1 ?output-container))
                 (boundaries (left ?beat-unit)
                             (right ?beat-unit)))
                <-
                (?beat-unit
                 (HASH meaning ((beat ?output-container ?output-kitchen-state ?input-kitchen-state ?input-container ?tool)))
                 --
                 (HASH form ((string ?beat-unit "beat")))))
               :feature-types ((ontology default :lookup-in-ontology)))


  (def-fcg-cxn mix-cxn
               ((?mix-unit
                 (ontology mixable)
                 (input-args (kitchen-state ?input-kitchen-state)
                             (arg1 ?input-container)
                             (arg2 ?tool))
                 (output-args (kitchen-state ?output-kitchen-state)
                              (arg1 ?output-container))
                 (boundaries (left ?mix-unit)
                             (right ?mix-unit)))
                <-
                (?mix-unit
                 (HASH meaning ((mix ?output-container ?output-kitchen-state ?input-kitchen-state ?input-container ?tool)))
                 --
                 (HASH form ((string ?mix-unit "mix")))))
               :feature-types ((ontology default :lookup-in-ontology)))

  (def-fcg-cxn add-cxn
               ((?add-unit
                 (ontology ingredient)
                 (input-args (kitchen-state ?input-kitchen-state)
                             (args (?output-container-before-adding ?input-container ?quantity ?unit)))
                 (output-args (kitchen-state ?output-kitchen-state)
                              (args (?output-container ?rest)))
                 (boundaries (left ?add-unit)
                             (right ?add-unit)))
                <-
                (?add-unit
                 (HASH meaning ((transfer-contents ?output-container ?rest ?output-kitchen-state
                                                   ?input-kitchen-state ?output-container-before-adding ?input-container
                                                   ?quantity ?unit)))
                 --
                 (HASH form ((string ?add-unit "add")))))
               :feature-types ((ontology default :lookup-in-ontology)))

  
               

  

;; Noun Phrases ;;
;;;;;;;;;;;;;;;;;;
  

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

  (def-fcg-cxn quantity-unit-cxn
               ((?noun-phrase-unit
                 (phrase-type noun-phrase)
                 (subunits (?quantity-unit ?unit-unit))
                 (value ?quantity)
                 (ontology (ontological-class ?unit)) ;;time!!
                 (boundaries (left ?quantity-unit-left)
                             (right ?unit-unit-right)))
                <-
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
                (?noun-phrase-unit
                 --
                 (HASH form ((meets ?quantity-unit-right ?unit-unit-left))))))


 

  (def-fcg-cxn number-range-cxn
               ((?number-range-unit
                 (boundaries (left ?number-1-unit)
                             (right ?number-2-unit))
                 (subunits (?number-1-unit ?dash-unit ?number-2-unit))
                 (ontology (ontological-class quantity))
                 (value ?value-1))
                <-
                (?number-1-unit
                 --
                 (ontology (ontological-class quantity))
                 (value ?value-1))
                (?dash-unit
                 --
                 (HASH form ((string ?dash-unit "-"))))
                (?number-2-unit
                 --
                 (ontology (ontological-class quantity))
                 (value ?value-2))
                (?number-range-unit
                 --
                 (HASH form ((meets ?number-1-unit ?dash-unit)
                             (meets ?dash-unit ?number-2-unit))))))


  (def-fcg-cxn the-x-cxn
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
                 (ontological-types (not kitchen-state))
                 (properties (contents ((ontological-class ?ontological-class-world))))
                 (binding-variable ?container-with-x)))
               :feature-types ((ontological-class default :compare-ontological-vectors))
               )
  

  (def-fcg-cxn definite-coordinate-np-with-ellipsis-cxn
               ((?noun-phrase-unit
                 (subunits (?the-unit ?x-unit-in-utterance ?and-unit ?y-unit-in-utterance ?z-unit))
                 (referent (args (?x-referent ?y-referent)))
                 (lex-class noun-phrase)
                 (boundaries (left ?the-unit)
                             (right ?z-unit)))
               <-
               (?the-unit
                --
                (HASH form ((string ?the-unit "the"))))
               (?x-unit-in-utterance
                --
                (ontology (ontological-class ?class-x-in-utterance))
                (boundaries (left ?x-left)
                            (right ?x-right)))
               (?x-unit-in-world
                --
                (binding-variable ?x-referent)
                (properties (contents ((ontological-types (?class-x-in-utterance ?property))))))
               (?and-unit
                --
                (HASH form ((string ?and-unit "and"))))
               (?y-unit-in-utterance
                --
                (ontology (ontological-class ?class-y-in-utterance))
                (boundaries (left ?y-left)
                            (right ?y-right)))
               (?y-unit-in-world
                --
                (binding-variable ?y-referent)
                (properties (contents ((ontological-types (?class-y-in-utterance ?property))))))
               (?z-unit
                --
                (ontology (ontological-class ?property)))
               (?noun-phrase-unit
                --
                (HASH form ((meets ?the-unit ?x-left)
                            (meets ?x-right ?and-unit)
                            (meets ?and-unit ?y-left)
                            (meets ?y-right ?z-unit))))))

  (def-fcg-cxn definite-coordinate-np-cxn
               ((?noun-phrase-unit
                 (subunits (?the-unit ?x-unit-in-utterance ?and-unit ?y-unit-in-utterance))
                 (referent (args (?x-referent ?y-referent)))
                 (lex-class noun-phrase)
                 (boundaries (left ?the-unit)
                             (right ?y-right)))
               <-
               (?the-unit
                --
                (HASH form ((string ?the-unit "the"))))
               (?x-unit-in-utterance
                --
                (ontology (ontological-class ?class-x-in-utterance))
                (boundaries (left ?x-left)
                            (right ?x-right)))
               (?x-unit-in-world
                --
                (binding-variable ?x-referent)
                (properties (contents ((ontological-types (?class-x-in-utterance))))))
               (?and-unit
                --
                (HASH form ((string ?and-unit "and"))))
               (?y-unit-in-utterance
                --
                (ontology (ontological-class ?class-y-in-utterance))
                (boundaries (left ?y-left)
                            (right ?y-right)))
               (?y-unit-in-world
                --
                (binding-variable ?y-referent)
                (properties (contents ((ontological-types (?class-y-in-utterance))))))
               (?noun-phrase-unit
                --
                (HASH form ((meets ?the-unit ?x-left)
                            (meets ?x-right ?and-unit)
                            (meets ?and-unit ?y-left))))))



  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Argument Structure Constructions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  
  (def-fcg-cxn verb-x-and-y-together-imperative-transitive-cxn
               ((?imperative-transitive-unit
                 (lex-class clause)
                 (subunits (?verb-unit ?ingredient-x-unit ?and-unit ?ingredient-y-unit ?together-unit))
                 (output-args (args (?output-container-y ?rest-x ?rest-y))
                              (kitchen-state ?output-kitchen-state-y))
                 (meaning ((transfer-contents ?output-container-?x ?rest-x ?output-kitchen-state-x
                                              ?input-kitchen-state-x ?empty-container ?input-container-x ?quantity-x ?unit-x)
                           (transfer-contents ?output-container-y ?rest-y ?output-kitchen-state-y ?output-kitchen-state-x
                                              ?output-container-?x  ?input-container-y ?quantity-y ?unit-y)))
                 (boundaries (left ?verb-unit)
                             (right ?together-unit)))
                 
                <-
                (?verb-unit
                 --
                 (ontology (ontological-class ?property))
                 (boundaries (left ?verb-unit-left)
                             (right ?verb-unit-right))
                 (input-args (kitchen-state ?output-kitchen-state-y)
                             (arg1 ?output-container-y)))
                (?ingredient-x-unit
                 --
                 (referent (args (?input-container-x)))
                 (ontology (ontological-types (?property)))
                 (boundaries (left ?ingredient-x-unit-left)
                             (right ?ingredient-x-unit-right)))
                (?and-unit
                 --
                 (HASH form ((string ?and-unit "and"))))
                (?ingredient-y-unit
                 --
                 (referent (args (?input-container-y)))
                 (ontology (ontological-types (?property)))
                 (boundaries (left ?ingredient-y-unit-left)
                             (right ?ingredient-y-unit-right)))
                (?together-unit
                 --
                 (HASH form ((string ?together-unit "together"))))
                (?kitchen-state
                 --
                 (ontological-class kitchen-state)
                 (binding-variable ?input-kitchen-state-x))
                (?imperative-transitive-unit
                 --
                 (HASH form ((meets ?verb-unit-right ?ingredient-x-unit-left)
                             (meets ?ingredient-x-unit-right ?and-unit)
                             (meets ?and-unit ?ingredient-y-unit-left)
                             (meets ?ingredient-y-unit-right ?together-unit))))))
  
 (def-fcg-cxn resultative-cxn
               ((?resultative-phrase
                 (lex-class clause)
                 (subunits (?resultative-unit ?clause-unit)))
                <-
                (?clause-unit
                 --
                 (lex-class clause)
                 (boundaries (left ?clause-unit-left)
                             (right ?clause-unit-right)))
                (?resultative-unit
                 --
                 (lex-class resultative-phrase)
                 (boundaries (left ?resultative-unit-left)
                             (right ?resultative-unit-right)))
                (?resultative-phrase
                 --
                 (HASH form ((meets ?clause-unit-right ?resultative-unit-left))))))

  
  (def-fcg-cxn x-room-temperature-cxn
               ((?x-room-temperature-unit
                 (subunits (?x-unit-in-utterance ?comma-unit ?room-string-unit ?temperature-string-unit))
                 (boundaries (left ?x-unit-in-utterance-left)
                             (right ?temperature-string-unit))
                 (input-args (args (?container-with-ingredients))
                             (kitchen-state ?input-kitchen-state))
                 (output-args (args (?ingredient-at-room-temperature))
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
                (?room-string-unit
                 --
                 (HASH form ((string ?room-string-unit "room"))))
                (?temperature-string-unit
                 --
                 (HASH form ((string ?temperature-string-unit "temperature"))))
                (?x-room-temperature-unit
                 (HASH meaning ((bring-up-to-temperature ?ingredient-at-room-temperature ?output-kitchen-state
                                                              ?input-kitchen-state ?container-with-ingredients 18 degrees-celsius)))
                 --
                 (HASH form ((meets ?x-unit-in-utterance-right ?comma-unit)
                             (meets ?comma-unit ?room-string-unit)
                             (meets ?room-string-unit ?temperature-string-unit))))))

  
  (def-fcg-cxn until-light-and-fluffy-cxn
             ((?until-light-and-fluffy-unit
               (lex-class resultative-phrase)
               (boundaries (left ?until-unit-1)
                           (right ?fluffy-unit-1))
               (subunits (?until-unit-1 ?light-unit-1 ?and-unit-797 ?fluffy-unit-1)))
              <-
              (?until-unit-1
                --
                (HASH form ((string ?until-unit-1 "until"))))
              (?light-unit-1
                --
                (HASH form ((string ?light-unit-1 "light"))))
              (?and-unit-797
                --
                (HASH form ((string ?and-unit-797 "and"))))
              (?fluffy-unit-1
                --
                (HASH form ((string ?fluffy-unit-1 "fluffy"))))
              (?until-light-and-fluffy-unit
               --
               (HASH form ((meets ?until-unit-1 ?light-unit-1)
                           (meets ?light-unit-1 ?and-unit-797)
                           (meets ?and-unit-797 ?fluffy-unit-1))))))


  (def-fcg-cxn add-X-2args-cxn
               ((?clause-unit
                 (subunits (?verb-unit ?noun-phrase-unit))
                 (boundaries (left ?verb-unit)
                             (right ?noun-phrase-unit-right))
                 (output-args (kitchen-state ?output-kitchen-state)
                              (arg1 ?output-container)
                              (args (?output-container ?rest-y)))
                 (meaning (;;add first ingredient x
                           (transfer-contents ?output-container-after-adding-x ?rest-x ?intermediate-ks
                                              ?input-ks ?output-container-before-adding-x ?container-x ?quantity-x ?unit-x)
                           ;;add then ingredient y
                           (transfer-contents ?output-container ?rest-y ?output-kitchen-state
                                              ?intermediate-ks ?output-container-after-adding-x ?container-y ?quantity-y ?unit-y))))
                <-
                (?ks-unit
                 --
                 (ontological-class kitchen-state)
                 (binding-variable ?input-ks))
                (?target-container-in-world-unit
                 --
                 (ontological-types (container)) ;;container => recency heuristic needed!!!!!!!
                 (properties (contents ((ontological-types (mixture))))) ;;ingredient
                 (binding-variable ?output-container-before-adding-x))
                (?verb-unit
                 --
                 (HASH form ((string ?verb-unit "add"))))
                (?noun-phrase-unit
                 --
                 (referent (args (?container-x ?container-y)))
                 (lex-class noun-phrase)
                 (boundaries (left ?noun-phrase-unit-left)
                             (right ?noun-phrase-unit-right)))
                (?clause-unit
                 --
                 (HASH form ((meets ?verb-unit ?noun-phrase-unit-left))))))

  (def-fcg-cxn X-and-Y-cxn ;;do X and do Y onto a container
               ((?clause-unit
                 (subunits (?x-unit ?and-unit ?y-unit))
                 (boundaries (left ?x-unit-left)
                             (right ?y-unit-right)))
                <-
                (?x-unit
                 --
                 (output-args (kitchen-state ?output-ks-action-x)
                              (arg1 ?output-container-action-x))
                 (boundaries (left ?x-unit-left)
                             (right ?x-unit-right)))
                (?and-unit
                 --
                 (HASH form ((string ?and-unit "and"))))
                (?y-unit
                 --
                 (input-args (kitchen-state ?output-ks-action-x)
                             (arg1 ?output-container-action-x))
                 (boundaries (left ?y-unit-left)
                             (right ?y-unit-right)))
                (?clause-unit
                 --
                 (HASH form ((meets ?x-unit-right ?and-unit)
                             (meets ?and-unit ?y-unit-left))))))


  (def-fcg-cxn mix-thoroughly-cxn ;;mix, shake, etc.
               ((?clause-unit
                 (subunits (?mix-unit ?thoroughly-unit))
                 (ontology mixable)
                 (input-args (kitchen-state ?input-kitchen-state)
                             (arg1 ?input-container)
                             (arg2 ?tool))
                 (output-args (kitchen-state ?output-kitchen-state)
                              (arg1 ?output-container))
                 (boundaries (left ?mix-unit)
                             (right ?thoroughly-unit))
                 (meaning ((mix ?output-container ?output-kitchen-state ?input-kitchen-state ?input-container ?tool))))
                <-
                (?ks-unit
                 --
                 (binding-variable ?input-kitchen-state)
                 (ontological-class kitchen-state))
                (?target-container-in-world-unit
                 --
                 (ontological-types (container)) ;;container => recency heuristic needed!!!!!!!
                 (properties (contents ((ontological-types (ingredient))))) ;;ingredient
                 (binding-variable ?input-container))
                (?mix-unit
                 --
                 (HASH form ((string ?mix-unit "mix"))))
                (?thoroughly-unit
                 --
                 (HASH form ((string ?thoroughly-unit "thoroughly"))))
                (?clause-unit
                 --
                 (HASH form ((meets ?mix-unit ?thoroughly-unit))))))

  (def-fcg-cxn take-generous-tablespoons-of-X-cxn
               ((?clause-unit
                 (subunits (?take-unit ?generous-unit ?tablespoons-unit ?of-unit ?x-unit-in-utterance))
                 
                 (boundaries (left ?take-unit)
                             (right ?x-unit-right))
                 (input-args (kitchen-state ?kitchen-state-with-lined-baking-tray)
                             (arg1 ?dough))
                 (output-args (kitchen-state ?kitchen-state-with-portions-on-tray)
                              (arg1 ?portioned-dough))
                 (meaning ((portion-and-arrange ?portioned-dough ?kitchen-state-with-portions-on-tray
                                    ?kitchen-state-with-lined-baking-tray ?dough 25 g ?pattern ?lined-baking-tray))))
                <-
                (?take-unit
                 --
                 (HASH form ((string ?take-unit "take"))))
                (?generous-unit
                 --
                 (HASH form ((string ?generous-unit "generous"))))
                (?tablespoons-unit
                 --
                 (HASH form ((string ?tablespoons-unit "tablespoons"))))
                (?of-unit
                 --
                 (HASH form ((string ?of-unit "of"))))
                (?x-unit-in-utterance
                 --
                 (boundaries (left ?x-unit-left)
                             (right ?x-unit-right))
                 (ontology (ontological-class ?ontological-class-utterance)
                           (ontological-types ?ontological-types)))
                (?x-unit-in-world
                 --
                 (ontological-types (not kitchen-state))
                 (properties (contents ((ontological-class ?ontological-class-world))))
                 (binding-variable ?dough))
                (?ks-unit
                 --
                 (ontological-class kitchen-state)
                 (binding-variable ?kitchen-state-with-lined-baking-tray))
               (?clause-unit
                 --
                 (HASH form ((meets ?take-unit ?generous-unit)
                             (meets ?generous-unit ?tablespoons-unit)
                             (meets ?tablespoons-unit ?of-unit)
                             (meets ?of-unit ?x-unit-left))))))


  (def-fcg-cxn and-roll-it-into-a-small-ball-cxn
             ((?clause-unit
               (subunits (?x-unit-in-utterance ?and-unit-2 ?roll-unit-1
                                               ?it-unit-2 ?into-unit-2 ?a-unit-2 ?small-unit-1 ?ball-unit-3))
               (lex-class clause)
               (boundaries (left ?x-unit-left)
                           (right ?ball-unit-3))
               (output-args (kitchen-state ?kitchen-state-out)
                            (arg1 ?shaped-bakeables))
               (meaning ((shape ?shaped-bakeables
                                ?kitchen-state-out
                                ?kitchen-state-in
                                ?unshaped-bakeables
                                ball-shape))))
              <-
              (?x-unit-in-utterance
               --
               (output-args (kitchen-state ?kitchen-state-in)
                            (arg1 ?unshaped-bakeables))
               (boundaries (left ?x-unit-left)
                           (right ?x-unit-right)))
              (?and-unit-2
                --
                (HASH form ((string ?and-unit-2 "and"))))
              (?roll-unit-1
                --
                (HASH form ((string ?roll-unit-1 "roll"))))
              (?it-unit-2
                --
                (HASH form ((string ?it-unit-2 "it"))))
              (?into-unit-2
                --
                (HASH form ((string ?into-unit-2 "into"))))
              (?a-unit-2
                --
                (HASH form ((string ?a-unit-2 "a"))))
              (?small-unit-1
                --
                (HASH form ((string ?small-unit-1 "small"))))
              (?ball-unit-3
                --
                (HASH form ((string ?ball-unit-3 "ball"))))
              (?clause-unit
               --
               (HASH form ((meets ?and-unit-2 ?roll-unit-1)
                           (meets ?roll-unit-1 ?it-unit-2)
                           (meets ?it-unit-2 ?into-unit-2)
                           (meets ?into-unit-2 ?a-unit-2)
                           (meets ?a-unit-2 ?small-unit-1)
                           (meets ?small-unit-1 ?ball-unit-3))))))


 (def-fcg-cxn shape-it-into-a-crescent-shape-cxn
             ((?clause-unit
               (subunits (?x-unit-in-utterance ?and-unit-2 ?then-unit-2 ?shape-unit-1
                                               ?it-unit-3 ?into-unit-3 ?a-unit-3 ?crescent-unit-1 ?shape-unit-2))
               (lex-class clause)
               (boundaries (left ?x-unit-left)
                           (right ?shape-unit-2))
               (meaning ((shape ?shaped-bakeables
                                ?kitchen-state-out
                                ?kitchen-state-in
                                ?unshaped-bakeables
                                crescent-shape))))
              <-
              (?x-unit-in-utterance
               --
               (lex-class clause)
               (output-args (kitchen-state ?kitchen-state-in)
                            (arg1 ?unshaped-bakeables))
               (boundaries (left ?x-unit-left)
                           (right ?x-unit-right)))
              (?and-unit-2
                --
                (HASH form ((string ?and-unit-2 "and"))))

              (?then-unit-2
                --
                (HASH form ((string ?then-unit-2 "then"))))
              
              (?shape-unit-1
                --
                (HASH form ((string ?shape-unit-1 "shape"))))
              (?it-unit-3
                --
                (HASH form ((string ?it-unit-3 "it"))))
              (?into-unit-3
                --
                (HASH form ((string ?into-unit-3 "into"))))
              (?a-unit-3
                --
                (HASH form ((string ?a-unit-3 "a"))))
              (?crescent-unit-1
                --
                (HASH form ((string ?crescent-unit-1 "crescent"))))
              (?shape-unit-2
                --
                (HASH form ((string ?shape-unit-2 "shape"))))
              (?clause-unit
               --
               (HASH form (
                           (meets ?shape-unit-1 ?it-unit-3)
                           (meets ?it-unit-3 ?into-unit-3)
                           (meets ?into-unit-3 ?a-unit-3)
                           (meets ?a-unit-3 ?crescent-unit-1)
                           (meets ?crescent-unit-1 ?shape-unit-2))))))

 (def-fcg-cxn about-an-inch-in-diameter-cxn
             ((?clause-unit
               (subunits (?comma-unit-1 ?about-unit-1 ?an-unit-1 ?inch-unit-1 ?in-unit-1 ?diameter-unit ?comma-unit-2)))
              <-
              (?comma-unit-1
                --
                (HASH form ((string ?comma-unit-1 ","))))
              (?about-unit-1
                --
                (HASH form ((string ?about-unit-1 "about"))))
              (?an-unit-1
                --
                (HASH form ((string ?an-unit-1 "an"))))
              (?inch-unit-1
                --
                (HASH form ((string ?inch-unit-1 "inch"))))
              (?in-unit-1
                --
                (HASH form ((string ?in-unit-1 "in"))))
              (?diameter-unit
                --
                (HASH form ((string ?diameter-unit "diameter"))))
              (?comma-unit-2
                --
                (HASH form ((string ?comma-unit-2 ","))))
              (?clause-unit
               --
               (lex-class clause)
               (HASH form ((meets ?comma-unit-1 ?about-unit-1)
                           (meets ?about-unit-1 ?an-unit-1)
                           (meets ?an-unit-1 ?inch-unit-1)
                           (meets ?inch-unit-1 ?in-unit-1)
                           (meets ?in-unit-1 ?diameter-unit)
                           (meets ?diameter-unit ?comma-unit-2))))))


             
(def-fcg-cxn a-parchment-paper-lined-baking-sheet-cxn
             ((?a-parchment-paper-lined-baking-sheet-unit
               (subunits (?a-unit-4 ?parchment-unit-1 ?paper-unit-1
                                    ?lined-unit-1 ?baking-unit-1 ?sheet-unit-1))
               (sem-cat supporting-surface)
               (lex-class noun-phrase)
               (boundaries (left ?a-unit-4)
                           (right ?sheet-unit-1))
               (output-args (kitchen-state ?kitchen-state-out)
                            (arg1 ?lined-baking-tray))
               (meaning ((line ?lined-baking-tray
                               ?kitchen-state-out 
                               ?kitchen-state-in
                               baking-tray
                               baking-paper))))
              <-
              (?ks-unit
                 --
                 (ontological-class kitchen-state)
                 (binding-variable ?kitchen-state-in))
              (?a-unit-4
                --
                (HASH form ((string ?a-unit-4 "a"))))
              (?parchment-unit-1
                --
                (HASH form ((string ?parchment-unit-1 "parchment"))))
              (?paper-unit-1
                --
                (HASH form ((string ?paper-unit-1 "paper"))))
              (?lined-unit-1
                --
                (HASH form ((string ?lined-unit-1 "lined"))))
              (?baking-unit-1
                --
                (HASH form ((string ?baking-unit-1 "baking"))))
              (?sheet-unit-1
                --
                (HASH form ((string ?sheet-unit-1 "sheet"))))
              (?a-parchment-paper-lined-baking-sheet-unit
               --
               (HASH form ((meets ?a-unit-4 ?parchment-unit-1)
                           (meets ?parchment-unit-1 ?paper-unit-1)
                           (meets ?paper-unit-1 ?lined-unit-1)
                           (meets ?lined-unit-1 ?baking-unit-1)
                           (meets ?baking-unit-1 ?sheet-unit-1))))))


(def-fcg-cxn place-X-onto-Y-cxn
             ((?clause-unit
               (subunits (?place-unit ?onto-unit ?arg2-unit))
               (lex-class clause)
               (boundaries (left ?place-unit)
                           (right ?x-unit-right))
               (meaning ((transfer-items ?things-placed ?kitchen-out ;;times nr of arg1s
                                         ?kitchen-in ?things-to-transfer ?where-to-transfer-it))))
              <-
              (?arg1-unit-in-world
               --
               (ontological-class list-of-kitchen-entities)
               (binding-variable ?things-to-transfer))
              (?place-unit
               --
               (HASH form ((string ?place-unit "place"))))
              (?onto-unit
               --
               (HASH form ((string ?onto-unit "onto"))))
              (?arg2-unit
               --
               (boundaries (left ?x-unit-left)
                           (right ?x-unit-right))
               (output-args (kitchen-state ?kitchen-in)
                            (arg1 ?where-to-transfer-it))
               (sem-cat supporting-surface))
              (?clause-unit
               --
               (HASH form ((meets ?place-unit ?onto-unit)
                           (meets ?onto-unit ?x-unit-left))))))


(def-fcg-cxn bake-at-temperature-for-duration-cxn
             ((?instruction-unit
               (phrase-type clause)
               (subunits (?bake-unit ?at-unit ?temperature-unit ?for-unit ?duration-unit))
               (meaning ((bake ?thing-baked
                               ?kitchen-state-out
                               ?kitchen-state-in
                               ?thing-to-bake
                               ?time-to-bake-quantity
                               minute
                               ?target-temperature-quantity
                               degrees-celsius))))
              <-
              (?ks-unit
               --
               (ontological-class kitchen-state)
               (binding-variable ?kitchen-state-in))
              (?thing-to-bake-unit
               --
               (ontological-class baking-tray)
               (binding-variable ?thing-to-bake)) 
              (?bake-unit
               --
               (HASH form ((string ?bake-unit "bake"))))
              (?at-unit
               --
               (HASH form ((string ?at-unit "at"))))
              (?temperature-unit
               --
               (value ?target-temperature-quantity)
               (ontology (ontological-class degrees-celsius))
               (phrase-type noun-phrase)
               (boundaries (left ?temperature-unit-left)
                           (right ?temperature-unit-right)))
              (?for-unit
               --
               (HASH form ((string ?for-unit "for"))))
              (?duration-unit
               --
               (value ?time-to-bake-quantity)
               (ontology (ontological-class minute))
               (phrase-type noun-phrase)
               (boundaries (left ?duration-unit-left)
                           (right ?duration-unit-right)))
              (?instruction-unit
               --
               (HASH form ((meets ?at-unit ?temperature-unit-left)
                           (meets ?for-unit ?duration-unit-left))))))


(def-fcg-cxn dust-with-x-cxn
             ((?instruction-unit
               (subunits (?dust-unit ?with-unit ?x-unit-in-utterance))
               (boundaries (left ?dust-unit)
                           (right ?x-unit-right))
               (meaning ((sprinkle ?sprinkled-object
                                   ?kitchen-state-out
                                   ?kitchen-state-in
                                   ?thing-to-dust
                                   ?topping-container))))
              <-
              (?ks-unit
               --
               (ontological-class kitchen-state)
               (binding-variable ?kitchen-state-in))
              (?thing-to-dust-unit
               --
               (ontological-class baking-tray)
               (binding-variable ?thing-to-dust))
              (?dust-unit
               --
               (HASH form ((string ?dust-unit "dust"))))
              (?with-unit
               --
               (HASH form ((string ?with-unit "with"))) )
              (?x-unit-in-utterance
               --
               (ontology (ontological-class ?ontological-class))
               (boundaries (left ?x-unit-left)
                           (right ?x-unit-right)))
              (?x-unit-in-world
               --
               (binding-variable ?topping-container)
               (ontological-types (bowl))
               (properties (contents ((ontological-class ?ontological-class)))))
              (?instruction-unit
               --
               (HASH form ((meets ?dust-unit ?with-unit)
                           (meets ?with-unit ?x-unit-left)))))))


;(activate-monitor trace-fcg)
;(activate-monitor trace-irl)

;(clear-output)

(defparameter *pdm* (initialise-personal-dynamic-memory
                    *fcg-constructions*
                    `((get-kitchen ,(make-var 'kitchen-state)))))

(process-uterances '(;;;; Ingredients
                     "226 grams butter , room temperature"
                     "116 grams sugar"    
                     "4 grams vanilla extract"
                     "4 grams almond extract"
                     "340 grams flour"
                     "112 grams almond flour"
                     "29 grams powdered sugar"
                     
                     ;;;; Instructions
                     "beat the butter and the sugar together until light and fluffy"
                     "add the vanilla and almond extracts and mix"
                     "add the flour and the almond flour"
                     "mix thoroughly"
                     "take generous tablespoons of the dough and roll it into a small ball , about an inch in diameter , and then shape it into a crescent shape"
                     "place onto a parchment paper lined baking sheet"
                     "bake at 175 °C for 15 - 20 minutes" ; or until a light golden brown
                     "dust with powdered sugar"
                     "end"
                     )
                   *pdm*
                   ;(initialise-personal-dynamic-memory
                   ; *fcg-constructions*
                   ; `((get-kitchen ,(make-var 'kitchen-state))))
                   )