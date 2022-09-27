;(ql:quickload :muhai-cookingbot)
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
                                                       :contents (list (make-instance 'raisin :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 200)))))
                                        (make-instance 'large-bowl
                                                       :contents (list (make-instance 'vegetable-oil :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'ml)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 1000)))))
                                                   
                                        
                                        (make-instance 'medium-bowl
                                                       :contents (list (make-instance 'all-purpose-flour :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 1000)))))
                                        (make-instance 'medium-bowl
                                                       :contents (list (make-instance 'brown-sugar :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 1000)))))

                                        (make-instance 'medium-bowl
                                                       :contents (list (make-instance 'oats :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 500)))))
                                        (make-instance 'medium-bowl
                                                       :contents (list (make-instance 'chopped-walnut :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 500)))))
                                        (make-instance 'medium-bowl
                                                       :contents (list (make-instance 'baking-soda :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 50)))))
                                        (make-instance 'medium-bowl
                                                       :contents (list (make-instance 'vanilla-extract :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 50)))))
                                        (make-instance 'medium-bowl
                                                       :contents (list (make-instance 'salt :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 500)))))
                                        (make-instance 'small-bowl
                                                       :contents (list (make-instance 'ground-cinnamon :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 50)))))
                                        (make-instance 'small-bowl
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

5                                   ;; baking equipment
                                   (make-instance 'baking-tray)
                                   (make-instance 'pan)
                                   (make-instance 'baking-paper))))))
                                          

(setf *easy-oatmeal-cookies-recipe* 
  `((get-kitchen ?kitchen)

    ;;"1 cup raisins"
    (fetch-and-proportion ?proportioned-raisins ?kitchen-state-with-raisins ?kitchen ?target-container-1 raisin 150 g)

    ;; "1/2 cups of hot water"
    (fetch-and-proportion ?proportioned-water ?kitchen-state-with-water ?kitchen-state-with-raisins ?target-container-2 water 125 ml)
    (bring-to-temperature ?hot-water ?kitchen-state-with-hot-water ?kitchen-state-with-water ?proportioned-water 60 degrees-celsius)

    ;; "2 cups of all-purpose flour"
    (fetch-and-proportion ?proportioned-flour ?kitchen-state-with-flour ?kitchen-state-with-hot-water ?target-container-3 all-purpose-flour 272 g)

    ;; "1 teaspoon of baking soda"
    (fetch-and-proportion ?proportioned-baking-soda ?kitchen-state-with-baking-soda ?kitchen-state-with-flour ?target-container-4 baking-soda 4.8 g)

    ;; "1 teaspoon of salt"
    (fetch-and-proportion ?proportioned-salt ?kitchen-state-with-salt ?kitchen-state-with-baking-soda ?target-container-5 salt 6 g)

    ;; "2 cups of quick cooking oats"
    (fetch-and-proportion ?proportioned-oats ?kitchen-state-with-oats ?kitchen-state-with-salt ?target-container-6 oats 162 g)
    
    ;; "1 teaspoon of ground cinnamon"
    (fetch-and-proportion ?proportioned-cinnamon ?kitchen-state-with-cinnamon ?kitchen-state-with-oats ?target-container-7 ground-cinnamon 2.64 g)

    ;; "1 teaspoon of ground nutmeg"
    (fetch-and-proportion ?proportioned-nutmeg ?kitchen-state-with-nutmeg ?kitchen-state-with-cinnamon ?target-container-8 ground-nutmeg 2.37 g)

    ;; "1 cup of packed brown sugar"
    (fetch-and-proportion ?proportioned-sugar ?kitchen-state-with-sugar ?kitchen-state-with-nutmeg ?target-container-9 brown-sugar 200 g)

    ;; "1/2 cup chopped walnuts"
    (fetch-and-proportion ?proportioned-walnuts ?kitchen-state-with-walnuts ?kitchen-state-with-sugar ?target-container-10 chopped-walnut 75 g)

    ;; "2 eggs"
    (fetch-and-proportion ?proportioned-eggs ?kitchen-state-with-eggs ?kitchen-state-with-walnuts ?target-container-11 egg 2 piece)

    ;; "3/4 cup vegetable oil"
    (fetch-and-proportion ?proportioned-oil ?kitchen-state-with-oil ?kitchen-state-with-eggs ?target-container-12 vegetable-oil 128 ml)

    ;; "1 teaspoon vanilla extract"
    (fetch-and-proportion ?proportioned-vanilla ?kitchen-state-with-vanilla ?kitchen-state-with-oil ?target-container-13 vanilla-extract 5 ml)


       ;; "Preheat oven to 350 degrees F (175 degrees C)."
    (preheat-oven ?preheated-oven ?kitchen-state-with-preheating-oven ?kitchen-state-with-vanilla 175 degrees-celsius)

    ;; "Soak raisins in hot water and set aside."
    (transfer-contents ?container-with-soaked-raisins ?empty-raisin-bowl ?kitchen-state-with-soaking-raisins ?kitchen-state-with-preheating-oven
                       ?hot-water ?proportioned-raisins ?quantity ?unit)
    
    ;; "In a large bowl, sift flour with soda, salt and spices."
    (transfer-contents ?flour-with-soda ?empty-soda-bowl ?kitchen-state-with-flour-and-soda ?kitchen-state-with-soaking-raisins
                       ?proportioned-flour ?proportioned-baking-soda ?quantity-1 ?unit-1)
    (transfer-contents ?flour-with-soda-and-salt ?empty-salt-bowl ?kitchen-state-with-flour-soda-and-salt ?kitchen-state-with-flour-and-soda
                       ?flour-with-soda ?proportioned-salt ?quantity-2 ?unit-2)
    (transfer-contents ?flour-soda-salt-cinnamon ?empty-cinnamon-bowl ?kitchen-state-with-flour-soda-salt-cinnamon ?kitchen-state-with-flour-soda-and-salt
                       ?flour-with-soda-and-salt ?proportioned-cinnamon ?quantity-3 ?unit-3)
    (transfer-contents ?flour-soda-salt-cinnamon-nutmeg ?empty-nutmeg-bowl ?kitchen-state-with-flour-soda-salt-cinnamon-nutmeg ?kitchen-state-with-flour-soda-salt-cinnamon
                       ?flour-soda-salt-cinnamon ?proportioned-nutmeg ?quantity-4 ?unit-4)
    (fetch ?large-bowl ?kitchen-state-with-fetched-bowl-for-sifting ?kitchen-state-with-flour-soda-salt-cinnamon-nutmeg large-bowl 1)
    (sift ?bowl-with-sifted-ingredients ?kitchen-state-after-sifting ?kitchen-state-with-fetched-bowl-for-sifting ?large-bowl ?flour-soda-salt-cinnamon-nutmeg ?sifting-tool)
    
    ;; "Blend in rolled oats, sugar and nuts"
    (transfer-contents ?output-container-x ?rest-x ?output-kitchen-state-x ?kitchen-state-after-sifting ?bowl-with-sifted-ingredients ?proportioned-oats ?quantity-x ?unit-x)
    (transfer-contents ?output-container-y ?rest-y ?output-kitchen-state-y ?output-kitchen-state-x ?output-container-x ?proportioned-sugar ?quantity-y ?unit-y)
    (transfer-contents ?output-container-z ?rest-z ?output-kitchen-state-z ?output-kitchen-state-y ?output-container-y ?proportioned-walnuts ?quantity-z ?unit-z)
    (mix ?blended-in-oats-mixture ?kitchen-state-with-blended-oats-in-mixture ?output-kitchen-state-z ?output-container-z ?mixing-tool)
    
    ;; "In a separate bowl, beat eggs with fork and add oil, vanilla and raisins and water mixture."
    (fetch ?bowl-for-eggs ?kitchen-state-with-fetched-bowl-for-eggs ?kitchen-state-with-blended-oats-in-mixture medium-bowl 1)
    (crack ?container-w-cracked-eggs ?kitchen-state-with-cracked-eggs ?kitchen-state-with-fetched-bowl-for-eggs ?proportioned-eggs ?bowl-for-eggs)
    (fetch ?fetched-fork ?kitchen-state-with-fetched-fork ?kitchen-state-with-cracked-eggs fork 1)
    (beat ?container-w-beaten-eggs ?kitchen-state-w-beaten-eggs ?kitchen-state-with-fetched-fork ?container-w-cracked-eggs ?fetched-fork)

    (transfer-contents ?output-container-a ?rest-a ?output-kitchen-state-a ?kitchen-state-w-beaten-eggs ?container-w-beaten-eggs ?proportioned-oil ?quantity-a ?unit-a)
    (transfer-contents ?output-container-b ?rest-b ?output-kitchen-state-b ?output-kitchen-state-a ?output-container-a ?proportioned-vanilla ?quantity-b ?unit-b)
    (transfer-contents ?container-w-eggs-oil-vanilla-raisins ?rest-c ?output-kitchen-state-c ?output-kitchen-state-b ?output-container-b ?container-with-soaked-raisins ?quantity-c ?unit-c)
    
    
    
    ))



;; ======================
;; Append bindings to the recipe
;; ======================

(defparameter *extended-recipe*
  (append-meaning-and-irl-bindings *easy-oatmeal-cookies-recipe* nil))

;; ======================
;; Evaluate the recipe
;; ======================

;(evaluate-irl-program *extended-recipe* nil)


;; ======================
;; Visualise the recipe
;; ======================

(draw-recipe *easy-oatmeal-cookies-recipe*)
(draw-recipe *extended-recipe*)


#|

    ;; -------------------------------------------------------------------------------------------------------------

 

  

  

   

    ;; "Pour into dry ingredients, stirring until well mixed."
    (combine-homogeneous ?complete-mixture 
             ?kitchen-state-with-complete-mixture ?kitchen-state-with-eggs-oil-vanilla-raisins-mixture
             ?blended-in-oats-mixture
             ?eggs-oil-vanilla-raisins-mixture)

    ;; "Drop by teaspoonfuls about two inches apart onto ungreased cookie sheets."
    (bind-and-fetch ?baking-sheet ?kitchen-state-with-baking-sheet ?kitchen-state-with-complete-mixture baking-tray)
    
    (define-amount ?teaspoonful 5 g)
    (bind two-inch ?pattern ,(make-instance 'two-inch))
    
    (to-portion-and-arrange ?baking-sheet-with-cookies
                            ?kitchen-state-with-arranged-cookies-on-sheet ?kitchen-state-with-baking-sheet
                            ?complete-mixture ?teaspoonful ?pattern ?baking-sheet)

    ;; "Bake 10 to 13 minutes in the preheated oven, until the edges are golden."
    (to-transfer ?oven-with-sheet ?sheet-in-oven
                 ?kitchen-state-with-sheet-in-oven ?kitchen-state-with-arranged-cookies-on-sheet
                 ?baking-sheet-with-cookies ?preheated-oven)
    (define-amount ?time-to-bake 10 minute)
    (to-bake ?sheet-with-cookies
          ?kitchen-state-with-baked-cookies-in-oven ?kitchen-state-with-sheet-in-oven
          ?sheet-in-oven  ?time-to-bake)
    (to-fetch ?sheet-with-baked-cookies ?kitchen-state-with-cookies-on-counter ?kitchen-state-with-baked-cookies-in-oven ?sheet-with-cookies)))

|#

