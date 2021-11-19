(ql:quickload :aipp-cookingbot)
(in-package :aipp-cookingbot)

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
   :id 'kitchen-state
   :fridge (make-instance 'fridge
                          :contents (list (make-instance 'medium-bowl
                                                         :contents (list (make-instance 'egg :amount
                                                                                        (make-instance 'amount
                                                                                                       :unit (make-instance 'piece)
                                                                                                       :quantity (make-instance 'quantity
                                                                                                                                :value 2)))))))
   :pantry (make-instance 'pantry
                          :contents (list (make-instance 'medium-bowl
                                                         :contents (list (make-instance 'raisin :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 150)))))
                                          (make-instance 'medium-bowl
                                                         :contents (list (make-instance 'water :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 125)))))
                                          (make-instance 'medium-bowl
                                                         :contents (list (make-instance 'all-purpose-flour :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 272)))))
                                          (make-instance 'medium-bowl
                                                         :contents (list (make-instance 'baking-soda :amount
                                                                                        (make-instance 'amount
                                                                                                       :unit (make-instance 'g)
                                                                                                       :quantity (make-instance 'quantity
                                                                                                                                :value 4.8)))))
                                          (make-instance 'medium-bowl
                                                         :contents (list (make-instance 'salt :amount
                                                                                        (make-instance 'amount
                                                                                                       :unit (make-instance 'g)
                                                                                                       :quantity (make-instance 'quantity
                                                                                                                                :value 6)))))
                                          (make-instance 'medium-bowl
                                                         :contents (list (make-instance 'quick-cooking-oats :amount
                                                                                        (make-instance 'amount
                                                                                                       :unit (make-instance 'g)
                                                                                                       :quantity (make-instance 'quantity
                                                                                                                                :value 162)))))
                                          (make-instance 'medium-bowl
                                                         :contents (list (make-instance 'ground-cinnamon :amount
                                                                                        (make-instance 'amount
                                                                                                       :unit (make-instance 'g)
                                                                                                       :quantity (make-instance 'quantity
                                                                                                                                :value 2.64)))))
                                          (make-instance 'medium-bowl
                                                         :contents (list (make-instance 'ground-nutmeg :amount
                                                                                        (make-instance 'amount
                                                                                                       :unit (make-instance 'g)
                                                                                                       :quantity (make-instance 'quantity
                                                                                                                                :value 2.37)))))
                                          (make-instance 'medium-bowl
                                                         :contents (list (make-instance 'brown-sugar :amount
                                                                                        (make-instance 'amount
                                                                                                       :unit (make-instance 'g)
                                                                                                       :quantity (make-instance 'quantity
                                                                                                                                :value 200)))))
                                          (make-instance 'medium-bowl
                                                         :contents (list (make-instance 'chopped-walnut :amount
                                                                                        (make-instance 'amount
                                                                                                       :unit (make-instance 'g)
                                                                                                       :quantity (make-instance 'quantity
                                                                                                                                :value 75)))))
                                          (make-instance 'medium-bowl
                                                         :contents (list (make-instance 'vegetable-oil :amount
                                                                                        (make-instance 'amount
                                                                                                       :unit (make-instance 'g)
                                                                                                       :quantity (make-instance 'quantity
                                                                                                                                :value 128)))))
                                          (make-instance 'medium-bowl
                                                         :contents (list (make-instance 'vanilla-extract :amount
                                                                                        (make-instance 'amount
                                                                                                       :unit (make-instance 'g)
                                                                                                       :quantity (make-instance 'quantity
                                                                                                                                :value 5)))))))

   :kitchen-cabinet (make-instance 'kitchen-cabinet
                                   :contents (list
                                              ;; bowls
                                              (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                              (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                              (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                              (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                              (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                              (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                              (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                              (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                              (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                              (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                              (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                              (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                              (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)

                                              ;; tools
                                              (make-instance 'fork) (make-instance 'sift)
                                              (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)

                                              ;; baking equipment
                                              (make-instance 'baking-tray)))))
                                          

(setf *easy-oatmeal-cookies-recipe* 
  `((to-get-kitchen ?kitchen)

    ;; "1 cup raisins"
    (fetch-and-proportion ?proportioned-raisins ?kitchen-state-with-raisins ?kitchen raisin 150 g)

    ;; "1/2 cups of hot water"
    (fetch-and-proportion ?proportioned-water ?kitchen-state-with-water ?kitchen-state-with-raisins water 125 ml)
    (define-amount ?hot-water-temperature 40 degrees-celsius)
    (to-bring-up-to-temperature ?hot-water ?kitchen-state-with-hot-water ?kitchen-state-with-water ?proportioned-water ?hot-water-temperature)

    ;; "2 cups of all-purpose flour"
    (fetch-and-proportion ?proportioned-flour ?kitchen-state-with-flour ?kitchen-state-with-hot-water all-purpose-flour 272 g)

    ;; "1 teaspoon of baking soda"
    (fetch-and-proportion ?proportioned-baking-soda ?kitchen-state-with-baking-soda ?kitchen-state-with-flour baking-soda 4.8 g)

    ;; "1 teaspoon of salt"
    (fetch-and-proportion ?proportioned-salt ?kitchen-state-with-salt ?kitchen-state-with-baking-soda salt 6 g)

    ;; "2 cups of quick cooking oats"
    (fetch-and-proportion ?proportioned-oats ?kitchen-state-with-oats ?kitchen-state-with-salt quick-cooking-oats 162 g)

    ;; "1 teaspoon of ground cinnamon"
    (fetch-and-proportion ?proportioned-cinnamon ?kitchen-state-with-cinnamon ?kitchen-state-with-oats ground-cinnamon 2.64 g)

    ;; "1 teaspoon of ground nutmeg"
    (fetch-and-proportion ?proportioned-nutmeg ?kitchen-state-with-nutmeg ?kitchen-state-with-cinnamon ground-nutmeg 2.37 g)

    ;; "1 cup of packed brown sugar"
    (fetch-and-proportion ?proportioned-sugar ?kitchen-state-with-sugar ?kitchen-state-with-nutmeg brown-sugar 200 g)

    ;; "1/2 cup chopped walnuts"
    (fetch-and-proportion ?proportioned-walnuts ?kitchen-state-with-walnuts ?kitchen-state-with-sugar chopped-walnut 75 g)

    ;; "2 eggs"
    (fetch-and-proportion ?proportioned-eggs ?kitchen-state-with-eggs ?kitchen-state-with-walnuts egg 2 piece)

    ;; "3/4 cup vegetable oil"
    (fetch-and-proportion ?proportioned-oil ?kitchen-state-with-oil ?kitchen-state-with-eggs vegetable-oil 128 ml)

    ;; "1 teaspoon vanilla extract"
    (fetch-and-proportion ?proportioned-vanilla ?kitchen-state-with-vanilla ?kitchen-state-with-oil vanilla-extract 5 ml)

    ;; -------------------------------------------------------------------------------------------------------------

    ;; "Preheat oven to 350 degrees F (175 degrees C)."
    (preheat-oven ?preheated-oven ?kitchen-state-with-preheated-oven ?kitchen-state-with-vanilla 175 degrees-celsius)

    ;; "Soak raisins in hot water and set aside."
    (bind-and-fetch ?medium-bowl ?kitchen-state-with-medium-bowl ?kitchen-state-with-preheated-oven medium-bowl)
    (transfer-all-contents ?bowl-with-hot-water-with-raisins 
                           ?kitchen-state-with-hot-water-in-bowl ?kitchen-state-with-medium-bowl
                           ?medium-bowl
                           ?hot-water
                           ?proportioned-raisins)

    ;; "In a large bowl, sift flour with soda, salt and spices."  
    (bind-and-fetch ?large-bowl ?kitchen-state-with-large-bowl ?kitchen-state-with-hot-water-in-bowl large-bowl)
    (combine-homogeneous ?flour-soda-salt-spices-mixture
             ?kitchen-state-with-flour-soda-salt-spices-mixture ?kitchen-state-with-large-bowl
             ?large-bowl
             ?proportioned-baking-soda ?proportioned-flour
             ?proportioned-salt ?proportioned-nutmeg ?proportioned-cinnamon)

    (bind-and-fetch ?sift ?kitchen-state-with-sift ?kitchen-state-with-flour-soda-salt-spices-mixture sift)
    (to-sift ?sifted-flour-mix
             ?kitchen-state-with-sifted-flour-mix ?kitchen-state-with-sift
             ?flour-soda-salt-spices-mixture ?sift)

    ;; "Blend in rolled oats, sugar and nuts"
    (combine-homogeneous ?blended-in-oats-mixture
             ?kitchen-state-with-blended-oats-in-mixture ?kitchen-state-with-sifted-flour-mix
             ?sifted-flour-mix
             ?proportioned-oats ?proportioned-sugar ?proportioned-walnuts)

    ;; "In a separate bowl, beat eggs with fork and add oil, vailla and raisins and water mixture."
    (bind-and-fetch ?medium-bowl-2 ?kitchen-state-with-bowl ?kitchen-state-with-blended-oats-in-mixture medium-bowl)
    (bind-and-fetch ?fork ?kitchen-state-with-fork ?kitchen-state-with-bowl fork)
    (to-crack ?bowl-with-cracked-eggs
           ?kitchen-state-with-cracked-eggs ?kitchen-state-with-fork ?proportioned-eggs ?medium-bowl-2)
    (to-beat ?beaten-eggs
             ?kitchen-state-with-beaten-eggs-in-bowl ?kitchen-state-with-cracked-eggs 
             ?bowl-with-cracked-eggs ?fork)

    (combine-homogeneous ?eggs-oil-vanilla-raisins-mixture
             ?kitchen-state-with-eggs-oil-vanilla-raisins-mixture ?kitchen-state-with-beaten-eggs-in-bowl
             ?beaten-eggs
             ?proportioned-oil ?proportioned-vanilla
             ?bowl-with-hot-water-with-raisins)

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


(evaluate-irl-program (expand-macros *easy-oatmeal-cookies-recipe*) nil)

;; ======================
;; Visualise the recipe
;; ======================

;; High-level recipe notation:
(draw-recipe *easy-oatmeal-cookies-recipe* :expand nil)

;; All primitives expanded:
(draw-recipe *easy-oatmeal-cookies-recipe* :expand t)

