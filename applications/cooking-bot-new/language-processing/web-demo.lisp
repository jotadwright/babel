(ql:quickload :cooking-bot-new)

(in-package :cooking-bot-new)


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


;; Larger font for text in <p> tags
;(define-css 'main "p {font-size: 10pt}")

(defun header ()
  (clear-page)
  (deactivate-all-monitors) ; just to be sure that there is no other stuff 
  (activate-monitor trace-fcg)
  (activate-monitor trace-irl)
  (with-disabled-monitor-notifications
    (load (babel-pathname :directory '("applications" "muhai-cookingbot" "language-processing")
                          :name "almond-cookies-grammar"
                          :type "lisp")))
  
  (add-element '((hr)))
  (add-element
   '((h1) "Meaning and Understanding in Everyday Activities: Recipe Execution"))
  
  (add-element '((p)  "Katrien Beuls and Paul Van Eecke"))
  (add-element '((p)  ((i) ((a :href "https://ehai.ai.vub.ac.be/" :target "_blank") "Evolutionary &amp; Hybrid AI")
                       ", Artificial Intelligence Laboratory, Vrije Universiteit Brussel")))
  
  (add-element '((hr))))

;(header)


(defun section-1 ()
  (add-element '((h2 :id "section-3") "Almond Crescent Cookies"))
  (add-element '((p) "This demonstration shows how instructions from a recipe in natural language, in this case a recipe to bake "
                 ((a :href "https://www.simplyrecipes.com/recipes/almond_crescent_cookies/" :target "_blank") "almond crescent cookies")
                 ", can be understood by an intelligent agent. The recipe is included here below:" ))

  
  
  (add-element '((h3) "Ingredients: "))
  (add-element '((ul)
                 ((li) "225 grams butter, room temperature")
                 ((li) "115 grams sugar")
                 ((li) "4 grams vanilla extract")
                 ((li) "4 grams almond extract")
                 ((li) "340 grams flour")
                 ((li) "110 grams almond flour")
                 ((li) "30 grams powdered sugar for sprinkling")))
  
  (add-element '((h3) "Instructions "))
  (add-element '((ol)
                 ((li) "Preheat the oven to 350 degrees Fahrenheit.")
                 ((li) "Beat the butter and the sugar together until light and fluffy.")
                 ((li) "Add the vanilla and almond extracts and mix.")
                 ((li) "Add the flour and almond flour.")
                 ((li) "Mix thoroughly.")
                 ((li) "Take generous tablespoons of the dough and roll it into a small ball, about an inch in diameter, and then shape into a crescent shape.")
                 ((li) "Place onto a parchment paper lined baking sheet.")
                 ((li) "Bake at 350 degrees Fahrenheit for 15 to 20 minutes.")
                 ((li) "Dust each cookie with powdered sugar.")))

  (add-element '((p) "To understand the instructions from the recipe, information from three sources needs to be combined: (i) knowledge about the ingredients from the ingredient list, (ii) knowledge built up during the execution of the recipe, and (iii) knowledge of the world to infer actions or tools that are not explicitly mentioned. For instance, in order to execute the third instruction &quot;" ((i) "Add the vanilla and almond extracts and mix") "&quot;, the agent needs to find the two types of extract that were mentioned in the ingredient list, add these to a bowl that is currently on the counter top and mix everything using a whisk."))
  
  (add-element '((h3) "Initial kitchen state:"))

  (add-element '((p) "The representation of the kitchen before cooking starts can be inspected by clicking on the box here below. Ingredients can be found in the fridge or the pantry. Cooking utensils are stored in the kitchen cabinet. The counter top is empty at the start of cooking."))
  
  (add-element (make-html *initial-kitchen-state* :expand-initially nil))

  (add-element '((h3 :id "fcg-irl") "Recipe Execution:"))

  (add-element '((p) "The rest of this demonstration includes a trace of the FCG and IRL web monitors when executing the first steps of the recipe (and the ingredient list). FCG takes care of the language processing and maps the instruction into a procedural semantic representation. This meaning representation is then completed and executed by the IRL engine. The resulting bindings are made accessible to FCG by creating units for them in the initial transient structure of the next comprehension step. In this way, the constructions have access to the information present in the current kitchen state."))
  
  (process-utterances '(;;;; Ingredients 
                       "226 grams butter , room temperature"
                       "116 grams sugar"
                       "4 grams vanilla extract"
                       "4 grams almond extract"
                       "340 grams flour"
                       "112 grams almond flour"
                       ;"29 grams powdered sugar"
                     
                       ;;;; Instructions
                       "beat the butter and the sugar together until light and fluffy"
                       "add the vanilla and almond extracts and mix"
                       "add the flour and almond flour"
                       "mix thoroughly"
                       )
                   (initialise-personal-dynamic-memory
                    *fcg-constructions*
                    `((get-kitchen ,(make-var 'kitchen-state)))))
  
  (add-element '((p) ""))
  )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Full Demo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun full-demo ()
  (header)
  (section-1)
  )

;(full-demo)


;;;; Static web page
; (web-interface:create-static-html-page "MUHAI Recipe Execution" (full-demo))




