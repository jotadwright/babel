(ql:quickload :aipp-cookingbot)
(in-package :aipp-cookingbot)

;; The 'trace-irl' monitor will make sure that
;; the IRL evaluation process is shown on the web
;; interface (which can be found at localhost:8000).
;; We need to activate it:
(activate-monitor trace-irl)

;; ##################################################################
;; Whole Wheat Ginger Snaps
;; https://www.allrecipes.com/recipe/79512/whole-wheat-ginger-snaps/
;; ##################################################################


;; Defining the initial kitchen state
(defparameter *initial-kitchen-state* 
  (make-instance 
   'kitchen-state
   :id 'kitchen-state
   :fridge (make-instance 'fridge
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
   :pantry (make-instance 'pantry
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
                                                 (make-instance 'large-bowl)

                                                 ;; tools
                                                 (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk) (make-instance 'brush)
                                                 (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk) 
                                                 (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                                 
                                                 ;; baking equipment
                                                 (make-instance 'wire-rack)
                                                 (make-instance 'cookie-sheet)))
      :oven (make-instance 'oven)))


(setf *whole-wheat-ginger-snaps*
      `((to-get-kitchen ?kitchen-state)

         ;; 1 cup butter or margarine  
         ;; " Or " : just take the first ingredient
         (fetch-and-proportion ?proportioned-butter ?kitchen-state-with-butter ?kitchen-state butter 225 g)

         ;; 1 1/2 cups white sugar
         (fetch-and-proportion ?proportioned-sugar ?kitchen-state-with-sugar ?kitchen-state-with-butter white-sugar 300 g)

         ;; 2 eggs, beaten
         (fetch-and-proportion ?proportioned-eggs ?kitchen-state-with-eggs ?kitchen-state-with-sugar egg 2 piece)
         (bind-and-fetch ?bowl-for-eggs ?kitchen-state-with-bowl-for-eggs ?kitchen-state-with-eggs medium-bowl)
         (to-crack ?cracked-eggs ?kitchen-state-with-cracked-eggs ?kitchen-state-with-bowl-for-eggs ?proportioned-eggs ?bowl-for-eggs)
         (beat ?beaten-eggs ?kitchen-state-with-beaten-eggs ?kitchen-state-with-cracked-eggs ?cracked-eggs)

                  ;; 1 cup molasses
         (fetch-and-proportion ?proportioned-molasses ?kitchen-state-with-molasses ?kitchen-state-with-beaten-eggs  molasses   280 g)

         ;; 4 cups whole wheat flour
         (fetch-and-proportion ?proportioned-whole-wheat-flour ?kitchen-state-with-whole-wheat-flour ?kitchen-state-with-molasses  whole-wheat-flour 480 g)
    
         ;; 1 tablespoon baking soda
         (fetch-and-proportion ?proportioned-baking-soda ?kitchen-state-with-baking-soda ?kitchen-state-with-whole-wheat-flour baking-soda  14.40 g)

         ;; 2 teaspoon baking powder
         (fetch-and-proportion ?proportioned-baking-powder ?kitchen-state-with-baking-powder ?kitchen-state-with-baking-soda baking-powder 8 g)

         ;; 1 tablespoon ground ginger
         (fetch-and-proportion ?proportioned-ground-ginger ?kitchen-state-with-ground-ginger ?kitchen-state-with-baking-powder ground-ginger  5.27 g)
    
         ;; 1 1/2 teaspoons ground nutmeg
         (fetch-and-proportion ?proportioned-ground-nutmeg ?kitchen-state-with-ground-nutmeg ?kitchen-state-with-ground-ginger ground-nutmeg 3.5 g)

         ;; 1 1/2 teaspoons ground cinnamon
         (fetch-and-proportion ?proportioned-ground-cinnamon ?kitchen-state-with-ground-cinnamon  ?kitchen-state-with-ground-nutmeg ground-cinnamon 3.96 g)
    
         ;; 1 1/2 teaspoons ground cloves
         (fetch-and-proportion ?proportioned-ground-cloves ?kitchen-state-with-ground-cloves ?kitchen-state-with-ground-cinnamon ground-cloves 3.3 g)

         ;; 1 1/2 teaspoons ground allspice
         (fetch-and-proportion ?proportioned-ground-allspice ?kitchen-state-with-ground-allspice ?kitchen-state-with-ground-cloves ground-allspice 3 g)
    
         ;; 1 cup white sugar for decoration
         (fetch-and-proportion ?proportioned-decoration-sugar ?kitchen-state-with-sugar-for-decoration ?kitchen-state-with-ground-allspice  white-sugar 300 g)
         
         ;; "Preheat oven to 350 degrees F (175 degrees C)."
         (preheat-oven ?preheated-oven ?kitchen-state-with-preheated-oven ?kitchen-state-with-sugar-for-decoration 175 degrees-celsius)


         ;; "Grease cookie sheets."
         (bind-and-fetch ?cookie-sheets ?kitchen-state-with-cookie-sheets ?kitchen-state-with-preheated-oven cookie-sheet)
         (grease  ?greased-cookie-sheets ?kitchen-state-with-greased-cookie-sheets ?kitchen-state-with-cookie-sheets ?cookie-sheets butter)

          ;; "In a large bowl, cream together the butter and 1 1/2 cup of sugar until smooth."
         (bind-and-fetch ?large-bowl  ?kitchen-state-with-bowl ?kitchen-state-with-greased-cookie-sheets large-bowl)
         (combine-homogeneous ?sugar-butter-mixture-bowl ?kitchen-state-with-sugar-butter-mixture ?kitchen-state-with-bowl ?large-bowl ?proportioned-butter ?proportioned-sugar)

         ;; "Mix in the eggs, and then the molasses."
         (combine-homogeneous ?molasses-mixture-bowl ?kitchen-state-with-molasses-mixture ?kitchen-state-with-sugar-butter-mixture ?sugar-butter-mixture-bowl ?beaten-eggs ?proportioned-molasses)

         ;; "Combine the whole wheat flour, baking soda, baking powder, ginger, nutmeg, cinnamon, cloves, and allspice, heaping the measures if you like a lot of spice."
         (bind-and-fetch ?bowl-for-mixture ?kitchen-state-with-bowl-for-mixture ?kitchen-state-with-molasses-mixture medium-bowl)
         (combine-homogeneous ?dry-mixture-bowl ?kitchen-state-with-dry-mixture ?kitchen-state-with-bowl-for-mixture ?bowl-for-mixture ?proportioned-whole-wheat-flour ?proportioned-baking-soda ?proportioned-baking-powder ?proportioned-ground-ginger ?proportioned-ground-nutmeg ?proportioned-ground-cinnamon ?proportioned-ground-cloves ?proportioned-ground-allspice)

         ;; "Stir the dry ingredients into the molasses mixture until blended."
         (combine-homogeneous ?dough ?kitchen-state-with-dough ?kitchen-state-with-dry-mixture ?molasses-mixture-bowl ?dry-mixture-bowl)


         ;; "Roll the dough into small balls, and dip the top of each ball into the remaining white sugar."
         ;; "Place the cookies about 2 inches apart on the cookie sheets."
         (define-amount ?dough-portion 1.7 percent) ;; 60 servings, 5 dozen ;;
         (bind two-inch ?pattern ,(make-instance 'two-inch))
         (to-portion-and-arrange ?portions-dough ?kitchen-state-with-portions-dough ?kitchen-state-with-dough ?dough ?dough-portion ?pattern ?greased-cookie-sheets)
         (shape ?small-balls ?kitchen-state-with-small-balls ?kitchen-state-with-portions-dough  ?portions-dough ball-shape)
         (to-dip ?dipped-balls ?kitchen-state-with-dipped-balls ?kitchen-state-with-small-balls ?small-balls ?proportioned-decoration-sugar)

         ;; "Bake for 10 to 15 minutes in the preheated oven, until the tops are cracked. Bake longer for crispy cookies, less time for chewy cookies."
         (to-transfer ?oven-with-pan ?pan-in-oven ?kitchen-state-with-pan-in-oven ?kitchen-state-with-dipped-balls ?dipped-balls ?preheated-oven)
         (define-amount ?time-to-bake 10 minute)
         (to-bake ?pan-with-ginger-snaps ?kitchen-state-with-ginger-snaps-in-oven ?kitchen-state-with-pan-in-oven ?pan-in-oven ?time-to-bake)
         (to-fetch ?fetched-pan-with-ginger-snaps ?kitchen-state-with-ginger-snaps-on-counter ?kitchen-state-with-ginger-snaps-in-oven ?pan-with-ginger-snaps)
                      
         ;; "Cool on wire racks."
         (bind-and-fetch ?wire-racks ?kitchen-state-with-wire-racks ?kitchen-state-with-ginger-snaps-on-counter wire-rack)
         (define-amount ?all 100 percent)
         (to-transfer-contents ?snaps-on-wire-racks ?rest ?kitchen-state-with-snaps-on-wire-racks ?kitchen-state-with-wire-racks ?fetched-pan-with-ginger-snaps ?wire-racks ?all)
         (define-amount ?cool-temp 18 degrees-celsius)
         (to-cool ?cool-snaps-on-wire-racks ?kitchen-state-with-room-temp-snaps ?kitchen-state-with-snaps-on-wire-racks ?snaps-on-wire-racks ?cool-temp)

        
         ))

         

         

         
         
        

(evaluate-irl-program (expand-macros *whole-wheat-ginger-snaps*) nil)




;; ======================
;; Visualise the recipe
;; ====================== 

;; High-level recipe notation: 
(draw-recipe *whole-wheat-ginger-snaps* :expand nil)

;; All primitives expanded:
(draw-recipe *whole-wheat-ginger-snaps* :expand t) 

