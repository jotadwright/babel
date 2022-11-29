(ql:quickload :muhai-cookingbot)

(in-package :muhai-cookingbot)

;; The 'trace-irl' monitor will make sure that
;; the IRL evaluation process is shown on the web
;; interface (which can be found at localhost:8000).
;; We need to activate it:
;(activate-monitor trace-irl)

;; ##################################################################
;; Whole Wheat Ginger Snaps
;; https://www.allrecipes.com/recipe/79512/whole-wheat-ginger-snaps/
;; ##################################################################


;; Defining the initial kitchen state
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
                                                                                                                              :value 250)))))
                                        (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'egg
                                                                                      :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'piece)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 2)))))))
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
                                                       :contents (list (make-instance 'whole-wheat-flour :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 1000)))))
                                        (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'baking-soda :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 250)))))
                                        (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'baking-powder :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 250)))))
                                        (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'ground-ginger :amount
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
                                                                                                                              :value 50)))))
                                        (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'ground-cinnamon :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 50)))))
                                        (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'ground-cloves :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 50)))))
                                        (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'ground-allspice :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 50)))))
                                       (make-instance 'medium-bowl
                                                      :used T
                                                      :contents (list (make-instance 'molasses :amount
                                                                                     (make-instance 'amount
                                                                                                    :unit (make-instance 'g)
                                                                                                    :quantity (make-instance 'quantity
                                                                                                                             :value 900)))))))
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
                                   (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                   
                                   ;; baking equipment
                                   (make-instance 'wire-rack)
                                   (make-instance 'baking-tray)
                                   (make-instance 'cookie-sheet)
                                   (make-instance 'pan)
                                   (make-instance 'baking-paper))))))

;; 'make-html' makes an HTML representation of the kitchen state
;; and 'add-element' transfers that to the web interface
;(add-element (make-html *initial-kitchen-state* :expand-initially t))

(defparameter *whole-wheat-ginger-snaps-recipe*
      `((get-kitchen ?kitchen-state)
        
         ;; 225 grams butter
         (fetch-and-proportion ?proportioned-butter ?kitchen-state-with-butter ?kitchen-state ?target-container-1 butter 225 g)

         ;; 300 grams white sugar
         (fetch-and-proportion ?proportioned-sugar ?kitchen-state-with-sugar ?kitchen-state-with-butter ?target-container-2 white-sugar 300 g)

         ;; 2 eggs, beaten
         (fetch-and-proportion ?proportioned-eggs ?kitchen-state-with-eggs ?kitchen-state-with-sugar ?target-container-3 egg 2 piece)
         (crack ?container-w-cracked-eggs ?kitchen-state-with-cracked-eggs ?kitchen-state-with-eggs ?proportioned-eggs ?bowl-for-eggs) ;; IMPLICIT
         (beat ?beaten-eggs ?kitchen-state-with-beaten-eggs ?kitchen-state-with-cracked-eggs ?container-w-cracked-eggs ?beating-tool)

         ;; 280 grams molasses
         (fetch-and-proportion ?proportioned-molasses ?kitchen-state-with-molasses ?kitchen-state-with-beaten-eggs ?target-container-4 molasses 280 g)

         ;; 480 grams whole wheat flour
         (fetch-and-proportion ?proportioned-whole-wheat-flour ?kitchen-state-with-whole-wheat-flour ?kitchen-state-with-molasses ?target-container-5  whole-wheat-flour 480 g)
    
         ;; 1 tablespoon baking soda
         (fetch-and-proportion ?proportioned-baking-soda ?kitchen-state-with-baking-soda ?kitchen-state-with-whole-wheat-flour ?target-container-6 baking-soda 1 tablespoon)

         ;; 2 teaspoons baking powder
         (fetch-and-proportion ?proportioned-baking-powder ?kitchen-state-with-baking-powder ?kitchen-state-with-baking-soda ?target-container-7 baking-powder 2 teaspoon)

         ;; 1 tablespoon ground ginger
         (fetch-and-proportion ?proportioned-ground-ginger ?kitchen-state-with-ground-ginger ?kitchen-state-with-baking-powder ?target-container-8 ground-ginger 1 tablespoon)
    
         ;; 1 1/2 teaspoons ground nutmeg
         (fetch-and-proportion ?proportioned-ground-nutmeg ?kitchen-state-with-ground-nutmeg ?kitchen-state-with-ground-ginger ?target-container-9  ground-nutmeg 1.5 teaspoon)

         ;; 1 1/2 teaspoons ground cinnamon
         (fetch-and-proportion ?proportioned-ground-cinnamon ?kitchen-state-with-ground-cinnamon  ?kitchen-state-with-ground-nutmeg ?target-container-10  ground-cinnamon 1.5 teaspoon)
    
         ;; 1 1/2 teaspoons ground cloves
         (fetch-and-proportion ?proportioned-ground-cloves ?kitchen-state-with-ground-cloves ?kitchen-state-with-ground-cinnamon ?target-container-11  ground-cloves 1.5 teaspoon)

         ;; 1 1/2 teaspoons ground allspice
         (fetch-and-proportion ?proportioned-ground-allspice ?kitchen-state-with-ground-allspice ?kitchen-state-with-ground-cloves ?target-container-12  ground-allspice 1.5 teaspoon)
    
         ;; 300 grams white sugar for decoration
         (fetch-and-proportion ?proportioned-decoration-sugar ?kitchen-state-with-sugar-for-decoration ?kitchen-state-with-ground-allspice ?target-container-13  white-sugar 300 g)

         ;; "Preheat the oven to 175 degrees C."
         (preheat-oven ?preheated-oven ?kitchen-state-with-preheated-oven ?kitchen-state-with-sugar-for-decoration ?oven 175 degrees-celsius)
    
         ;; "Grease cookie sheets."
         (fetch ?cookie-sheet ?kitchen-state-with-cookie-sheet ?kitchen-state-with-preheated-oven cookie-sheet 1) ;; IMPLICIT
         (grease ?greased-sheet ?kitchen-state-with-greased-sheet ?kitchen-state-with-cookie-sheet ?cookie-sheet ?grease)

          ;; "In a large bowl, cream together the butter and 3OO grams of sugar until smooth."
          (fetch ?large-bowl ?kitchen-state-with-fetched-bowl ?kitchen-state-with-greased-sheet large-bowl 1) ;; IMPLICIT    
          (transfer-contents ?butter-in-large-bowl ?empty-butter-bowl ?kitchen-state-with-butter-in-large-bowl ?kitchen-state-with-fetched-bowl ?large-bowl ?proportioned-butter ?quantity-butter ?unit-butter)
          (transfer-contents ?butter-sugar-bowl ?empty-sugar-bowl ?kitchen-state-with-butter-sugar-mix ?kitchen-state-with-butter-in-large-bowl ?butter-in-large-bowl ?proportioned-sugar ?quantity-sugar ?unit-sugar)      
          (mix ?butter-sugar-cream ?kitchen-state-with-creamed-mix ?kitchen-state-with-butter-sugar-mix ?butter-sugar-bowl ?mixing-tool)

         ;; "Mix in the eggs, and then the molasses."
         (transfer-contents ?mix-and-eggs ?empty-egg-bowl ?kitchen-state-with-eggs-in-mix ?kitchen-state-with-creamed-mix ?butter-sugar-cream ?beaten-eggs ?quantity-eggs ?unit-eggs)
         (transfer-contents ?mix-eggs-and-molasses ?empty-molasses-bowl ?kitchen-state-with-molasses-in-mix ?kitchen-state-with-eggs-in-mix ?mix-and-eggs ?proportioned-molasses ?quantity-molasses ?unit-molasses)
         (mix ?cream-eggs-molasses-mix ?kitchen-state-with-cream-eggs-molasses-mix ?kitchen-state-with-molasses-in-mix ?mix-eggs-and-molasses ?beating-tool) ;; reuse the same whisk

         ;; "Combine the whole wheat flour, baking soda, baking powder, ginger, nutmeg, cinnamon, cloves, and allspice, heaping the measures if you like a lot of spice."
         (transfer-contents ?output-container-a ?rest-a ?output-kitchen-state-a ?kitchen-state-with-cream-eggs-molasses-mix ?empty-container ?proportioned-whole-wheat-flour ?quantity-a ?unit-a)
         (transfer-contents ?output-container-b ?rest-b ?output-kitchen-state-b ?output-kitchen-state-a ?output-container-a ?proportioned-baking-soda ?quantity-b ?unit-b)
         (transfer-contents ?output-container-c ?rest-c ?output-kitchen-state-c ?output-kitchen-state-b ?output-container-b ?proportioned-baking-powder ?quantity-c ?unit-c)
         (transfer-contents ?output-container-d ?rest-d ?output-kitchen-state-d ?output-kitchen-state-c ?output-container-c ?proportioned-ground-ginger ?quantity-d ?unit-d)
         (transfer-contents ?output-container-e ?rest-e ?output-kitchen-state-e ?output-kitchen-state-d ?output-container-d ?proportioned-ground-nutmeg ?quantity-e ?unit-e)
         (transfer-contents ?output-container-f ?rest-f ?output-kitchen-state-f ?output-kitchen-state-e ?output-container-e ?proportioned-ground-cinnamon ?quantity-f ?unit-f)
         (transfer-contents ?output-container-g ?rest-g ?output-kitchen-state-g ?output-kitchen-state-f ?output-container-f ?proportioned-ground-cloves ?quantity-g ?unit-g)
         (transfer-contents ?output-container-h ?rest-h ?output-kitchen-state-h ?output-kitchen-state-g ?output-container-g ?proportioned-ground-allspice ?quantity-h ?unit-h)

         (mix ?dry-mixture ?kitchen-state-with-dry-mixture ?output-kitchen-state-h ?output-container-h ?mixing-tool) ;; reuse the same whisk
         
         ;; "Stir the dry ingredients into the molasses mixture until blended."
         (transfer-contents ?molasses-dry-mixture ?rest-molasses-dry-mix ?kitchen-state-with-molasses-dry-mixture ?kitchen-state-with-dry-mixture ?cream-eggs-molasses-mix ?dry-mixture ?quantity-stir ?unit-sir)
         (mix ?dough ?kitchen-state-with-dough ?kitchen-state-with-molasses-dry-mixture ?molasses-dry-mixture ?mixing-tool) ;; reuse the same whisk


         ;TODO RD: should transfer-items and dip actually be switched? Because now the dipped cookies are placed on the countertop
         
         ;; "Roll the dough into small balls (25 grams), and dip the top of each ball into the remaining white sugar."
         (portion-and-arrange ?portioned-dough ?kitchen-state-with-portions ?kitchen-state-with-dough ?dough 25 g ?pattern ?countertop)
         (shape ?shaped-bakeables ?kitchen-state-with-shaped-bakeables ?kitchen-state-with-portions ?portioned-dough ball-shape)
         (dip ?dipped-bakeables ?kitchen-state-with-dipped-bakeables ?kitchen-state-with-shaped-bakeables ?shaped-bakeables ?proportioned-decoration-sugar)

         ;; "Place the cookies about 5 cm apart on the cookie sheets."
         (transfer-items ?bakeables-on-sheet ?kitchen-out-bakeables-on-sheet ?kitchen-state-with-dipped-bakeables ?dipped-bakeables 5-cm-apart ?greased-sheet)

         ;; "Bake for 10 minutes in the preheated oven, until the tops are cracked."
         (bake ?baked-snaps ?kitchen-out-with-baked-snaps ?kitchen-out-bakeables-on-sheet ?bakeables-on-sheet ?preheated-oven 10 minute ?preheated-quantity ?preheated-unit)

         ;; "Cool on wire racks."
         (fetch ?wire-rack ?kitchen-state-with-wire-rack ?kitchen-out-with-baked-snaps wire-rack 1) ;; IMPLICIT
         (transfer-items ?snaps-on-wire-rack ?kitchen-state-with-snaps-on-wire-rack ?kitchen-state-with-wire-rack ?baked-snaps ?default-pattern ?wire-rack)
         (bring-to-temperature ?cooled-snaps ?kitchen-state-with-cooled-snaps ?kitchen-state-with-snaps-on-wire-rack ?snaps-on-wire-rack 18 degrees-celsius)))
         
;; ======================
;; Append bindings to the recipe
;; ======================

(defparameter *extended-recipe*
  (append-meaning-and-irl-bindings *whole-wheat-ginger-snaps-recipe* nil))

;; ======================
;; Evaluate the recipe
;; ======================

;(activate-monitor trace-irl)

;(clear-output)

;(evaluate-irl-program *extended-recipe* nil)

;; ======================
;; Visualise the recipe
;; ======================

;(draw-recipe *whole-wheat-ginger-snaps-recipe*)
;(draw-recipe *extended-recipe*)
