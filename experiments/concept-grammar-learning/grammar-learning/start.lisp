

(in-package :cl-user)
(ql:quickload :category-hierarchies) ;; fix for circular import in babel/systems/fcg/construction-inventory-processor/construction-inventory-processor.lisp ln. 931
(ql:quickload :grammar-learning)
(in-package :cgl)

(activate-monitor trace-fcg)
;; (deactivate-monitor trace-fcg-search-process)
;; (activate-monitor trace-learning)

(def-fcg-constructions-with-type-hierarchy grammar-learning
  :feature-types ((args set)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits set)
                  (footprints set))
  :fcg-configurations ((:parse-goal-tests :non-gold-standard-meaning)
                       (:production-goal-tests :non-gold-standard-utterance)
                       (:de-render-mode . :de-render-string-meets)
                       (:render-mode . :generate-and-test)
                       (:consolidate-repairs . t))
  :diagnostics (diagnose-non-gold-standard-meaning diagnose-non-gold-standard-utterance)
  ;;:repairs (add-lexical-cxn add-item-based-cxn add-holophrase-cxn) ;; this is the order of application!
  :repairs (repair-holophrase-missing-word add-holophrase-cxn)
  )

;; (set-configuration *fcg-constructions* :use-meta-layer nil)
;; (set-configuration *fcg-constructions* :consolidate-repairs nil)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the red cube ==> the cube
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(comprehend "the red cube" :gold-standard-meaning '((get-context ?source)
                                                    (filter ?cubes ?source ?cube) ;; returns cubes set, filters source by shape
                                                    (filter ?red-cubes ?cubes ?red) ;; returns set of red cubes
                                                    (unique ?the-red-cube ?red-cubes);; checks if there is only one elem in set, fails if there are more
                                                    (bind shape-category ?cube cube) ;; bind ?cube object to prototype cube from the ontology
                                                    (bind color-category ?red red))) ;; bind ?red object to prototype red


(comprehend "the cube" :gold-standard-meaning '((get-context ?source)
                                                    (filter ?cubes ?source ?cube) ;; returns cubes set, filters source by shape
                                                    (unique ?the-cube ?cubes) ;; checks if there is only one elem in set, fails if there are more
                                                    (bind shape-category ?cube cube))) ;; bind ?cube object to prototype cube from the ontology
                                                    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the red cylinder
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(comprehend "the red cylinder" :gold-standard-meaning '((get-context ?source)
                                                    (filter ?cylinders ?source ?cylinder) ;; returns cylinders set, filters source by shape
                                                    (filter ?red-cylinders ?cylinders ?red) ;; returns set of red cylinders
                                                    (unique ?the-red-cylinder ?red-cylinders);; checks if there is only one elem in set, fails if there are more
                                                    (bind shape-category ?cylinder cylinder) ;; bind ?cylinder object to prototype cylinder from the ontology
                                                    (bind color-category ?red red))) ;; bind ?red object to prototype red



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the red cube ==> the cube
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(comprehend "the red cube" :gold-standard-meaning '((get-context ?source)
                                                    (filter ?cubes ?source ?cube) ;; returns cubes set, filters source by shape
                                                    (filter ?red-cubes ?cubes ?red) ;; returns set of red cubes
                                                    (unique ?the-red-cube ?red-cubes);; checks if there is only one elem in set, fails if there are more
                                                    (bind shape-category ?cube cube) ;; bind ?cube object to prototype cube from the ontology
                                                    (bind color-category ?red red))) ;; bind ?red object to prototype red


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the blue cube
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(comprehend "the blue cube" :gold-standard-meaning '((get-context ?source)
                                                    (filter ?cubes ?source ?cube) ;; returns cubes set, filters source by shape
                                                    (filter ?blue-cubes ?cubes ?blue) ;; returns set of blue cubes
                                                    (unique ?the-blue-cube ?blue-cubes);; checks if there is only one elem in set, fails if there are more
                                                    (bind shape-category ?cube cube) ;; bind ?cube object to prototype cube from the ontology
                                                    (bind color-category ?blue blue))) ;; bind ?red object to prototype blue
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the green cube
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(comprehend "the green cube" :gold-standard-meaning '((get-context ?source)
                                                    (filter ?cubes ?source ?cube) ;; returns cubes set, filters source by shape
                                                    (filter ?green-cubes ?cubes ?green) ;; returns set of green cubes
                                                    (unique ?the-green-cube ?green-cubes);; checks if there is only one elem in set, fails if there are more
                                                    (bind shape-category ?cube cube) ;; bind ?cube object to prototype cube from the ontology
                                                    (bind color-category ?green green))) ;; bind ?red object to prototype green






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; What is the color of the cube ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(comprehend "what is the color of the cube" :gold-standard-meaning '((get-context ?source)
                                                                     (bind attribute-category ?attribute color)
                                                                     (bind shape-category ?shape cube)
                                                                     (unique ?object ?cubes)
                                                                     (filter ?cubes ?source ?shape)
                                                                     (query ?response ?object ?attribute)))

(formulate '((get-context source)
             (bind attribute-category attribute color)
             (bind shape-category shape cube)
             (unique object cubes)
             (filter cubes source shape)
             (query response object attribute))
           :gold-standard-utterance "what is the color of the cube")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; What is the color of the sphere ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comprehend "what is the color of the cylinder"
            :gold-standard-meaning '((get-context ?source)
                                     (bind attribute-category ?attribute color)
                                     (bind shape-category ?shape cylinder)
                                     (unique ?object ?spheres)
                                     (filter ?spheres ?source ?shape)
                                     (query ?response ?object ?attribute)))
(wi:add-element (make-html (categorial-network *fcg-constructions*)))
 
(formulate '((get-context source)
             (bind attribute-category attribute color)
             (bind shape-category shape sphere)
             (unique object spheres)
             (filter spheres source shape)
             (query response object attribute))
           :gold-standard-utterance "what is the color of the sphere")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; What is the color of the cylinder ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comprehend "what is the color of the cylinder"
            :gold-standard-meaning '((get-context ?source)
                                     (bind attribute-category ?attribute color)
                                     (bind shape-category ?shape cylinder)
                                     (unique ?object ?spheres)
                                     (filter ?spheres ?source ?shape)
                                     (query ?response ?object ?attribute)))

(formulate '((get-context source)
             (bind attribute-category attribute color)
             (bind shape-category shape cylinder)
             (unique object spheres)
             (filter spheres source shape)
             (query response object attribute))
           :gold-standard-utterance "what is the color of the cylinder")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; What is the color of the ball   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comprehend "what is the color of the ball"
            :gold-standard-meaning '((get-context ?source)
                                     (bind attribute-category ?attribute color)
                                     (bind shape-category ?shape sphere)
                                     (unique ?object ?spheres)
                                     (filter ?spheres ?source ?shape)
                                     (query ?response ?object ?attribute)))

(formulate '((get-context source)
             (bind attribute-category attribute color)
             (bind shape-category shape sphere)
             (unique object spheres)
             (filter spheres source shape)
             (query response object attribute))
           :gold-standard-utterance '("what is the color of the ball" "what is the color of the sphere"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; What is the material of the cube ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comprehend "what is the material of the cube"
            :gold-standard-meaning '((get-context ?source)
                                     (bind attribute-category ?attribute material)
                                     (bind shape-category ?shape cube)
                                     (unique ?object ?spheres)
                                     (filter ?spheres ?source ?shape)
                                     (query ?response ?object ?attribute)))

(formulate '((get-context source)
             (bind attribute-category attribute material)
             (bind shape-category shape cube)
             (unique object spheres)
             (filter spheres source shape)
             (query ?response object attribute))
           :gold-standard-utterance "what is the material of the cube")


(wi:add-element (make-html *fcg-constructions*))

(def-fcg-cxn cylinder-cxn
             ((?cylinder-unit
               (args (?cylinder-unit))
               (syn-cat (phrase-type lexical)
                        (lex-class cat-94)))
              <-
             (?cylinder-unit
              (HASH meaning ((bind shape ?cylinder-unit cylinder)))
              --
              (HASH form ((string ?cylinder-unit "cylinder"))))))






(comprehend "Give me the ball") :gold-standard-meaning '((give-01 ?g)
                                                        (you ?y)
                                                        (me ?m)
                                                        (ball ?b)
                                                        (:arg0 ?g ?y)
                                                        (:arg1 ?g ?b)
                                                        (:arg2 ?g ?m)
                                                        ;(:mode ?g imperative)
                                                        ))

(comprehend "Give me the cup" :gold-standard-meaning '((give-01 ?g)
                                                        (you ?y)
                                                        (me ?m)
                                                        (cup ?c)
                                                        (:arg0 ?g ?y)
                                                        (:arg1 ?g ?c)
                                                        (:arg2 ?g ?m)
                                                        ;(:mode ?g imperative)
                                                        ))


(activate-monitor trace-fcg)

(def-fcg-constructions-with-type-hierarchy grammar-learning
  :feature-types ((args set)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits set)
                  (footprints set))
  :fcg-configurations (
                       
                       (:de-render-mode . :de-render-string-meets)
                       (:render-mode . :generate-and-test)
                       (:consolidate-repairs . t))


  
  (def-fcg-cxn give-me-the-ball-cxn
               ((?holophrase-unit
                 (phrase-type holophrase))
                <-
                (?holpohrase-unit
                 (HASH meaning ((give-01 ?g)
                                (you ?y)
                                (me ?m)
                                (cup ?p)
                                (:arg0 ?g ?y)
                                (:arg1 ?g ?p)
                                (:arg2 ?g ?m)))
                 --
                 (HASH form ((string ?give-string "give")
                             (string ?me-string "me")
                             (string ?the-string "the")
                             (string ?ball-string "ball")
                             (meets ?give-string ?me-string)
                             (meets ?me-string ?the-string)
                             (meets ?the-string ?ball-string))))))
  
  (def-fcg-cxn give-me-the-X-cxn
               ((?item-based-unit
                 (phrase-type item-based)
                 (subunits (?X-unit)))
                <-
                (?item-based-unit
                 (HASH meaning ((give-01 ?g)
                                (you ?y)
                                (me ?m)
                                (:arg0 ?g ?y)
                                (:arg1 ?g ?X)
                                (:arg2 ?g ?m)))
                 --
                 (HASH form ((string ?give-string "give")
                             (string ?me-string "me")
                             (string ?the-string "the")
                             (string ?ball-string "ball")
                             (meets ?give-string ?me-string)
                             (meets ?me-string ?the-string)
                             (meets ?the-string ?X-stirng))))
                (?X-unit
                 (category ?category-1)
                 --
                 )))

  (def-fcg-cxn ball-cxn
               ((?lexical-unit
                 (category ?category-1))
                <-
                (?item-based-unit
                 (HASH meaning ((ball ?b)))
                 --
                 (HASH form ((string ?ball-string "ball"))))))
  )


(comprehend "give me the ball")



