(in-package :muhai-cookingbot)

;; The 'trace-irl' monitor will make sure that
;; the IRL evaluation process is shown on the web
;; interface (which can be found at localhost:8000).
;; We need to activate it:
(activate-monitor trace-irl)

;; Defining the initial kitchen state
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
                                                       :contents (list (make-instance 'white-sugar :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 1000)))))
                                        (make-instance 'medium-bowl
                                                       :contents (list (make-instance 'vanilla-extract :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 100)))))
                                        (make-instance 'medium-bowl
                                                       :contents (list (make-instance 'chopped-walnut :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 100)))))
                                        (make-instance 'medium-bowl
                                                       :contents (list (make-instance 'all-purpose-flour :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 1000)))))
                                        (make-instance 'medium-bowl
                                                         :contents (list (make-instance 'cocoa-powder :amount
                                                                                        (make-instance 'amount
                                                                                                       :unit (make-instance 'g)
                                                                                                       :quantity (make-instance 'quantity
                                                                                                                                :value 500)))))
                                        (make-instance 'medium-bowl
                                                       :contents (list (make-instance 'salt :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 500)))))))
         (make-instance 'kitchen-cabinet
                        :contents (list
                                   ;; bowls
                                   (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)

                                   ;; tools
                                   (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                   (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                   (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)

                                   ;; baking equipment
                                   (make-instance 'baking-tray)
                                   (make-instance 'pan)
                                   (make-instance 'baking-paper))))))

;; 'make-html' makes an HTML representation of the kitchen state
;; and 'add-element' transfers that to the web interface
(add-element (make-html *initial-kitchen-state* :expand-initially t))




;; ##################################################################
;; Best brownie recipe
;; https://www.allrecipes.com/recipe/25010/absolutely-best-brownies/
;; ##################################################################

(defparameter *brownie-recipe* 
  '((get-kitchen ?kitchen-state)
    
    ;; "1/2 cup butter, melted"
    (fetch-and-proportion ?proportioned-butter ?kitchen-state-with-butter ?kitchen-state ?new-container-1 butter 113 g)
    (melt ?melted-butter ?kitchen-state-with-melted-butter ?kitchen-state-with-butter ?proportioned-butter)
    
    ;; "1 cup white sugar"
    (fetch-and-proportion ?proportioned-sugar ?kitchen-state-with-sugar ?kitchen-state-with-melted-butter ?new-container-2 white-sugar 201 g)
                          
    ;; "2 eggs"
    (fetch-and-proportion ?proportioned-eggs ?kitchen-state-with-eggs ?kitchen-state-with-sugar ?new-container-3 egg 2 piece)
    
    ;; "1/2 cup all-purpose flour"
   (fetch-and-proportion ?proportioned-flour ?kitchen-state-with-flour ?kitchen-state-with-eggs ?new-container-4 all-purpose-flour 68 g)
    
    ;; "1/3 cup unsweetened cocoa powder"
    (fetch-and-proportion ?proportioned-cocoa ?kitchen-state-with-cocoa ?kitchen-state-with-flour ?new-container-5 cocoa-powder 45 g)

    ;; "1/4 teaspoon salt"
    (fetch-and-proportion ?proportioned-salt ?kitchen-state-with-salt ?kitchen-state-with-cocoa ?new-container-6 salt 1.5 g)

    ;; "1 teaspoon vanilla extract"
    (fetch-and-proportion ?proportioned-vanilla ?kitchen-state-with-vanilla ?kitchen-state-with-salt ?new-container-7 vanilla-extract 4 g)
                          
    ;;"1/2 cup chopped walnuts (optional)"
    (fetch-and-proportion ?proportioned-walnuts ?kitchen-state-with-walnuts ?kitchen-state-with-vanilla ?new-container-8 chopped-walnut 50 g)
            
    ;; "Preheat oven to 350 degrees F (175 degrees C)."
    (preheat-oven ?preheated-oven ?kitchen-state-with-preheated-oven ?kitchen-state-with-walnuts 175 degrees-celsius)
                          
    ;; "Grease and flour an 8x8 or 9x9 inch baking pan"
    (grease ?greased-pan ?kitchen-state-with-greased-pan ?kitchen-state-with-preheated-oven pan ?grease)
    (flour ?floured-pan ?kitchen-state-with-floured-pan ?kitchen-state-with-greased-pan ?greased-pan ?all-purpose-flour)

    ;;  "In a medium bowl, beat together the butter and sugar."
    (transfer-contents ?output-container-x ?rest-x ?output-kitchen-state-x ?kitchen-state-with-floured-pan medium-bowl ?melted-butter ?quantity-x ?unit-x)
    (transfer-contents ?output-container-y ?rest-y ?output-kitchen-state-y ?output-kitchen-state-x ?output-container-x ?proportioned-sugar ?quantity-y ?unit-y)
    (beat ?beaten-mixture-bowl ?kitchen-state-with-beaten-mixture ?output-kitchen-state-y ?output-container-y ?beating-tool)

      ;; "Add eggs, and mix well."
    (crack ?mixture-with-cracked-eggs ?kitchen-state-with-cracked-eggs ?kitchen-state-with-beaten-mixture ?proportioned-eggs ?beaten-mixture-bowl)
   #|   (combine ?egg-sugar-mixture-bowl ?kitchen-state-with-eggs-sugar-mixture ?kitchen-state-with-cracked-eggs ?beaten-mixture-bowl ?cracked-eggs)

    ;; "Combine the flour, cocoa and salt; stir into the sugar mixture."
    (to-bind ?bowl-for-mixture *bowl-45*)
    (to-fetch ?kitchen-state-with-bowl-for-mixture ?kitchen-state-with-eggs-sugar-mixture ?bowl-for-mixture )
    (combine ?flour-cocoa-salt-mixture ?kitchen-state-with-flour-cocoa-and-salt-combined ?kitchen-state-with-bowl-for-mixture ?bowl-for-mixture ?proportioned-flour ?proportioned-cocoa ?proportioned-salt)
    (combine ?flour-sugar-mixture-bowl ?kitchen-state-with-flour-sugar-mixture ?kitchen-state-with-flour-cocoa-and-salt-combined ?egg-sugar-mixture-bowl ?flour-cocoa-salt-mixture)

    ;; "Mix in the vanilla and stir in the walnuts if desired."
    (combine ?dough ?kitchen-state-with-dough ?kitchen-state-with-flour-sugar-mixture ?flour-sugar-mixture-bowl ?proportioned-vanilla ?proportioned-walnuts)

    ;;  "Spread evenly into the prepared pan."
    (to-transfer ?pan-with-dough ?kitchen-state-with-dough-in-pan ?kitchen-state-with-dough ?dough ?floured-pan )
;; moet dit niet spread zijn

    ;;  "Bake for 25 to 30 minues in the preheated oven, or until edges are firm."
    (to-transfer ?oven-with-pan ?kitchen-state-with-pan-in-oven ?kitchen-state-with-dough-in-pan ?preheated-oven ?pan-with-dough )
    (define-amount ?time-to-bake 25 min)
    (bake ?pan-with-brownie ?kitchen-state-with-brownie-in-oven ?kitchen-state-with-pan-in-oven ?pan-with-dough ?time-to-bake)
    (to-fetch ?kitchen-state-with-brownie-on-counter ?kitchen-state-with-brownie-in-oven ?pan-with-brownie ) 
                           
    ;;  "Cool before cutting into squares"
    (cool ?cooled-brownie ?kitchen-state-with-cooled-brownie ?kitchen-state-with-brownie-on-counter ?pan-with-brownie)
    (divide ?cut-brownie ?kitchen-state-with-cut-brownie ?kitchen-state-with-cooled-brownie ?cooled-brownie square-pattern)) |#
))



;; ======================
;; Append bindings to the recipe
;; ======================

(defparameter *extended-recipe*
  (append-meaning-and-irl-bindings *brownie-recipe* nil))

;; ======================
;; Evaluate the recipe
;; ======================

;(activate-monitor trace-irl)
;(evaluate-irl-program *extended-recipe* nil)


;; ======================
;; Visualise the recipe
;; ======================

;(draw-recipe *brownie-recipe*)

