(ql:quickload :muhai-cookingbot)

(in-package :muhai-cookingbot)

;; The 'trace-irl' monitor will make sure that
;; the IRL evaluation process is shown on the web
;; interface (which can be found at localhost:8000).
;; We need to activate it:
(activate-monitor trace-irl)

;; ##################################################################
;; Easy oatmeal recipe
;; https://www.allrecipes.com/recipe/9627/easy-oatmeal-cookies/
;; ##################################################################

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
                                                                                                                              :value 500)))))
                                        (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'water
                                                                                      :temperature
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'degrees-celsius)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 5))
                                                                                      :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'ml)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 1000)))))
                                        (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'egg
                                                                                      :temperature
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'degrees-celsius)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 5))
                                                                                      :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'piece)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 12)))))))
         (make-instance 'pantry
                        :contents (list (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'raisin :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 200)))))
                                        (make-instance 'large-bowl
                                                       :used T
                                                       :contents (list (make-instance 'vegetable-oil :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'ml)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 1000)))))
                                                   
                                        
                                        (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'all-purpose-flour :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 1000)))))
                                        (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'brown-sugar :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 1000)))))

                                        (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'oats :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 500)))))
                                        (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'walnut :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 500)))))
                                        (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'baking-soda :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 50)))))
                                        (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'vanilla-extract :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 50)))))
                                        (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'salt :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 500)))))
                                        (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'ground-cinnamon :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 50)))))
                                        (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'ground-nutmeg :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 50)))))))
         (make-instance 'kitchen-cabinet
                        :contents (list
                                   ;; bowls
                                   (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)


                                   ;; tools
                                   (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                   (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                   (make-instance 'fork) (make-instance 'whisk) (make-instance 'whisk)
                                   (make-instance 'spatula) (make-instance 'knife) (make-instance 'sift)

                                   ;; baking equipment
                                   (make-instance 'baking-tray)
                                   (make-instance 'cookie-sheet)
                                   (make-instance 'pan)
                                   (make-instance 'baking-paper))))))

;; 'make-html' makes an HTML representation of the kitchen state
;; and 'add-element' transfers that to the web interface
;(add-element (make-html *initial-kitchen-state* :expand-initially t))


(defparameter *easy-oatmeal-cookies-recipe* 
  `((get-kitchen ?kitchen)

    ;;"150 grams raisins"
    (fetch-and-proportion ?proportioned-raisins ?kitchen-state-with-raisins ?kitchen ?target-container-1 raisin 150 g)

    ;; "125 ml hot water (60 degrees C)"
    (fetch-and-proportion ?proportioned-water ?kitchen-state-with-water ?kitchen-state-with-raisins ?target-container-2 water 125 ml)
    (bring-to-temperature ?hot-water ?kitchen-state-with-hot-water ?kitchen-state-with-water ?proportioned-water 60 degrees-celsius)

    ;; "280 grams all-purpose flour"
    (fetch-and-proportion ?proportioned-flour ?kitchen-state-with-flour ?kitchen-state-with-hot-water ?target-container-3 all-purpose-flour 280 g)

    ;; "1 teaspoon of baking soda"
    (fetch-and-proportion ?proportioned-baking-soda ?kitchen-state-with-baking-soda ?kitchen-state-with-flour ?target-container-4 baking-soda 1 teaspoon)

    ;; "1 teaspoon of salt"
    (fetch-and-proportion ?proportioned-salt ?kitchen-state-with-salt ?kitchen-state-with-baking-soda ?target-container-5 salt 1 teaspoon)

    ;; "160 grams quick cooking oats"
    (fetch-and-proportion ?proportioned-oats ?kitchen-state-with-oats ?kitchen-state-with-salt ?target-container-6 oats 160 g)
    
    ;; "1 teaspoon ground cinnamon"
    (fetch-and-proportion ?proportioned-cinnamon ?kitchen-state-with-cinnamon ?kitchen-state-with-oats ?target-container-7 ground-cinnamon 1 teaspoon)

    ;; "1 teaspoon ground nutmeg"
    (fetch-and-proportion ?proportioned-nutmeg ?kitchen-state-with-nutmeg ?kitchen-state-with-cinnamon ?target-container-8 ground-nutmeg 1 teaspoon)

    ;; "200 grams packed brown sugar"
    (fetch-and-proportion ?proportioned-sugar ?kitchen-state-with-sugar ?kitchen-state-with-nutmeg ?target-container-9 brown-sugar 200 g)

    ;; "75 grams chopped walnuts"
    (fetch-and-proportion ?proportioned-walnuts ?kitchen-state-with-walnuts ?kitchen-state-with-sugar ?target-container-10 walnut 75 g)
    (cut ?chopped-walnuts ?kitchen-state-with-chopped-walnuts ?kitchen-state-with-walnuts ?proportioned-walnuts chopped ?knife)

    ;; "2 eggs"
    (fetch-and-proportion ?proportioned-eggs ?kitchen-state-with-eggs ?kitchen-state-with-chopped-walnuts ?target-container-11 egg 2 piece)

    ;; "200 ml vegetable oil"
    (fetch-and-proportion ?proportioned-oil ?kitchen-state-with-oil ?kitchen-state-with-eggs ?target-container-12 vegetable-oil 200 ml)

    ;; "1 teaspoon vanilla extract"
    (fetch-and-proportion ?proportioned-vanilla ?kitchen-state-with-vanilla ?kitchen-state-with-oil ?target-container-13 vanilla-extract 1 teaspoon)

    ;; "Preheat the oven to 175 degrees C."
    (preheat-oven ?preheated-oven ?kitchen-state-with-preheating-oven ?kitchen-state-with-vanilla ?oven 175 degrees-celsius)

    ;; "Soak raisins in hot water; set aside."
    (transfer-contents ?container-with-soaked-raisins ?empty-raisin-bowl ?kitchen-state-with-soaking-raisins ?kitchen-state-with-preheating-oven
                       ?hot-water ?proportioned-raisins ?quantity ?unit)

    ;; "In a large bowl, sift flour with soda, salt and spices." ; TODO RD: was zijn de spices    
    (transfer-contents ?flour-with-soda ?empty-soda-bowl ?kitchen-state-with-flour-and-soda ?kitchen-state-with-soaking-raisins
                       ?proportioned-flour ?proportioned-baking-soda ?quantity-1 ?unit-1)
    (transfer-contents ?flour-with-soda-and-salt ?empty-salt-bowl ?kitchen-state-with-flour-soda-and-salt ?kitchen-state-with-flour-and-soda
                       ?flour-with-soda ?proportioned-salt ?quantity-2 ?unit-2)
    (transfer-contents ?flour-soda-salt-cinnamon ?empty-cinnamon-bowl ?kitchen-state-with-flour-soda-salt-cinnamon ?kitchen-state-with-flour-soda-and-salt
                       ?flour-with-soda-and-salt ?proportioned-cinnamon ?quantity-3 ?unit-3)
    (transfer-contents ?flour-soda-salt-cinnamon-nutmeg ?empty-nutmeg-bowl ?kitchen-state-with-flour-soda-salt-cinnamon-nutmeg ?kitchen-state-with-flour-soda-salt-cinnamon ?flour-soda-salt-cinnamon ?proportioned-nutmeg ?quantity-4 ?unit-4) ;; Resolve that spices is the cinnamon and nutmeg
    
    (fetch ?large-bowl ?kitchen-state-with-fetched-bowl-for-sifting ?kitchen-state-with-flour-soda-salt-cinnamon-nutmeg large-bowl 1) ;; IMPLICIT
    (sift ?bowl-with-sifted-ingredients ?kitchen-state-after-sifting ?kitchen-state-with-fetched-bowl-for-sifting ?large-bowl ?flour-soda-salt-cinnamon-nutmeg ?sifting-tool)

    ;; "Blend in rolled oats, sugar and nuts."    
    (transfer-contents ?output-container-x ?rest-x ?output-kitchen-state-x ?kitchen-state-after-sifting ?bowl-with-sifted-ingredients ?proportioned-oats ?quantity-x ?unit-x)
    (transfer-contents ?output-container-y ?rest-y ?output-kitchen-state-y ?output-kitchen-state-x ?output-container-x ?proportioned-sugar ?quantity-y ?unit-y)
    (transfer-contents ?output-container-z ?rest-z ?output-kitchen-state-z ?output-kitchen-state-y ?output-container-y ?chopped-walnuts ?quantity-z ?unit-z)
    (mix ?blended-in-oats-mixture ?kitchen-state-with-blended-oats-in-mixture ?output-kitchen-state-z ?output-container-z ?mixing-tool)

    ;; "In a separate bowl, beat eggs with a fork and add oil, vanilla, raisins, and water mixture;"
    (fetch ?bowl-for-eggs ?kitchen-state-with-fetched-bowl-for-eggs ?kitchen-state-with-blended-oats-in-mixture medium-bowl 1) ;; IMPLICIT
    (crack ?container-w-cracked-eggs ?kitchen-state-with-cracked-eggs ?kitchen-state-with-fetched-bowl-for-eggs ?proportioned-eggs ?bowl-for-eggs) ;; IMPLICIT
    (fetch ?fetched-fork ?kitchen-state-with-fetched-fork ?kitchen-state-with-cracked-eggs fork 1) ;; IMPLICIT
    (beat ?container-w-beaten-eggs ?kitchen-state-w-beaten-eggs ?kitchen-state-with-fetched-fork ?container-w-cracked-eggs ?fetched-fork)

    (transfer-contents ?output-container-a ?rest-a ?output-kitchen-state-a ?kitchen-state-w-beaten-eggs ?container-w-beaten-eggs ?proportioned-oil ?quantity-a ?unit-a)
    (transfer-contents ?output-container-b ?rest-b ?output-kitchen-state-b ?output-kitchen-state-a ?output-container-a ?proportioned-vanilla ?quantity-b ?unit-b)
    (transfer-contents ?container-w-eggs-oil-vanilla-raisins ?rest-c ?output-kitchen-state-c ?output-kitchen-state-b ?output-container-b ?container-with-soaked-raisins ?quantity-c ?unit-c)

    ;; "pour into flour mixture, stirring until well mixed."
    (transfer-contents ?container-with-flour-and-mixture ?rest-d ?output-kitchen-state-d ?output-kitchen-state-c ?flour-soda-salt-cinnamon-nutmeg ?container-w-eggs-oil-vanilla-raisins ?quantity-d ?unit-d)
    (mix ?dough ?kitchen-state-with-dough ?output-kitchen-state-d ?container-with-flour-and-mixture ?mixing-tool) ;; reuse the same tool

    ; TODO RD: rounded spoonfuls
    ;; "Drop by rounded spoonfuls 5 cm apart onto ungreased cookie sheets."
    (fetch ?cookie-sheet ?kitchen-state-with-cookie-sheet ?kitchen-state-with-dough cookie-sheet 1) ;; IMPLICIT
    (portion-and-arrange ?portioned-dough ?kitchen-state-with-portions ?kitchen-state-with-cookie-sheet ?dough 5 g ?pattern ?countertop)
    (transfer-items ?sheet-with-dough ?kitchen-state-with-dough-on-sheet ?kitchen-state-with-portions ?portioned-dough 5-cm-apart ?cookie-sheet)

    ;; "Bake for 10 minutes in the preheated oven."
    (bake ?baked-cookies ?kitchen-state-with-baked-cookies ?kitchen-state-with-dough-on-sheet ?sheet-with-dough ?preheated-oven 10 minute ?bake-quantity ?bake-unit)))


;; ======================
;; Append bindings to the recipe
;; ======================

(defparameter *extended-recipe*
  (append-meaning-and-irl-bindings *easy-oatmeal-cookies-recipe* nil))

;; ======================
;; Evaluate the recipe
;; ======================

;(activate-monitor trace-irl)

;(clear-output)

;(evaluate-irl-program *extended-recipe* nil)

;; ======================
;; Visualise the recipe
;; ======================

;(draw-recipe *easy-oatmeal-cookies-recipe*)
;(draw-recipe *extended-recipe*)
