(ql:quickload :muhai-cookingbot)

(in-package :muhai-cookingbot)

;; The 'trace-irl' monitor will make sure that
;; the IRL evaluation process is shown on the web
;; interface (which can be found at localhost:8000).
;; We need to activate it:
(activate-monitor trace-irl)

;; ##################################################################
;; Chocolate Fudge Cookies
;; https://www.allrecipes.com/recipe/19259/chocolate-fudge-cookies/
;; ##################################################################

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
                                                       :contents (list (make-instance 'devils-food-cake-mix :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 517)))))
                                        (make-instance 'medium-bowl
                                                       :contents (list (make-instance 'vegetable-oil :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 200)))))
                                        (make-instance 'medium-bowl
                                                       :contents (list (make-instance 'semisweet-chocolate-chips :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 250)))))))
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
                                   (make-instance 'spatula) (make-instance 'knife)

                                   ;; baking equipment
                                   (make-instance 'wire-rack)
                                   (make-instance 'baking-tray)
                                   (make-instance 'cookie-sheet)
                                   (make-instance 'pan)
                                   (make-instance 'baking-paper))))))

;; 'make-html' makes an HTML representation of the kitchen state
;; and 'add-element' transfers that to the web interface
;(add-element (make-html *initial-kitchen-state* :expand-initially t))


(defparameter *chocolate-fudge-cookies-recipe* 
  '((get-kitchen ?kitchen-state)

    ;; "500 grams package devil's food cake mix"
    (fetch-and-proportion ?proportioned-devils-food-cake-mix ?kitchen-state-with-devils-food-cake-mix ?kitchen-state ?target-container-1 devils-food-cake-mix 500 g)

    ;; "2 eggs"
    (fetch-and-proportion ?proportioned-eggs ?kitchen-state-with-eggs ?kitchen-state-with-devils-food-cake-mix ?target-container-2 egg 2 piece)

    ;; "125 ml vegetable oil"
    (fetch-and-proportion ?proportioned-vegetable-oil ?kitchen-state-with-vegetable-oil ?kitchen-state-with-eggs ?target-container-3 vegetable-oil 125 ml)

    ;; "160 grams semi-sweet chocolate chips"
    (fetch-and-proportion ?proportioned-semisweet-chocolate-chips ?kitchen-state-with-semisweet-chocolate-chips ?kitchen-state-with-vegetable-oil ?target-container-4 semisweet-chocolate-chips 160 g)
   
    ;; "Preheat oven to 175 degrees C."
    (preheat-oven ?preheated-oven ?kitchen-state-with-preheated-oven ?kitchen-state-with-semisweet-chocolate-chips 175 degrees-celsius)
    
    ;; "Grease cookie sheet."
    (fetch ?cookie-sheet ?kitchen-state-with-cookie-sheet ?kitchen-state-with-preheated-oven cookie-sheet 1) ;; IMPLICIT
    (grease ?greased-sheet ?kitchen-state-with-greased-sheet ?kitchen-state-with-cookie-sheet ?cookie-sheet ?grease)

    ;; "In a medium bowl, stir together the cake mix, eggs and oil until well blended."    
    (fetch ?medium-bowl-1 ?kitchen-state-with-medium-bowl ?kitchen-state-with-greased-sheet medium-bowl 1) ;; IMPLICIT
    (transfer-contents ?output-container-x ?rest-x ?output-kitchen-state-x ?kitchen-state-with-medium-bowl ?medium-bowl-1 ?proportioned-devils-food-cake-mix ?quantity-x ?unit-x)
    (crack ?output-container-y ?output-kitchen-state-y ?output-kitchen-state-x ?proportioned-eggs ?medium-bowl-1) ;; IMPLICIT
    (transfer-contents ?output-container-z ?rest-z ?output-kitchen-state-z ?output-kitchen-state-y ?output-container-y ?proportioned-vegetable-oil ?quantity-z ?unit-z)
    (mix ?stirred-mixture-bowl ?kitchen-state-with-stirred-mixture ?output-kitchen-state-z ?output-container-z ?mixing-tool)

    ;; "Fold in the chocolate chips."
    (transfer-contents ?output-container-with-chips ?rest-chips ?kitchen-state-with-folded-chips ?kitchen-state-with-stirred-mixture ?stirred-mixture-bowl ?proportioned-semisweet-chocolate-chips ?quantity-chips ?unit-chips)
    (mix ?chips-mixture-bowl ?kitchen-state-with-chips-mixture ?output-kitchen-state-z ?output-container-with-chips ?mixing-tool) ;; use the same whisk

    ;; "Roll the dough into walnut sized balls."
    (portion-and-arrange ?portioned-dough ?kitchen-state-with-portions ?kitchen-state-with-chips-mixture ?chips-mixture-bowl 20 g 5-cm-apart ?countertop)
    (shape ?shaped-bakeables ?ks-with-dough-balls ?kitchen-state-with-portions ?portioned-dough ball-shape)

    ;; "Place the cookies 5 cm apart on the cookie sheet."
    (transfer-items ?cookies-on-sheet ?ks-with-dough-on-sheet ?ks-with-dough-balls ?shaped-bakeables ?greased-sheet)

    ;; "Bake for 8 to 10 minutes in the preheated oven."
    (bake ?baked-cookies-on-sheet ?kitchen-state-with-cookies ?ks-with-dough-on-sheet ?cookies-on-sheet ?preheated-oven 8 minute ?bake-quantity ?bake-unit)

    ;; "Allow cookies to cool on baking sheet for 5 minutes before removing to a wire rack to cool completely."
    (cool-for-time ?cooling-cookies ?kitchen-state-with-cooling-cookies ?kitchen-state-with-cookies ?baked-cookies-on-sheet 5 minute)

    (fetch ?wire-rack ?kitchen-state-with-wire-rack ?kitchen-state-with-cookies wire-rack 1) ;; IMPLICIT
    (transfer-items ?cookies-on-wire-rack ?kitchen-state-with-cookies-on-wire-rack ?kitchen-state-with-wire-rack ?cooling-cookies ?wire-rack)
    (bring-to-temperature ?cooled-cookies ?kitchen-state-with-cooled-cookies ?kitchen-state-with-cookies-on-wire-rack ?cookies-on-wire-rack 18 degrees-celsius)))


;; ======================
;; Append bindings to the recipe
;; ======================

(defparameter *extended-recipe*
  (append-meaning-and-irl-bindings *chocolate-fudge-cookies-recipe* nil))

;; ======================
;; Evaluate the recipe
;; ======================

;(activate-monitor trace-irl)

;(clear-output)

;(evaluate-irl-program *extended-recipe* nil)


;; ======================
;; Visualise the recipe
;; ======================

;(draw-recipe *chocolate-fudge-cookies-recipe*)
;(draw-recipe *extended-recipe*)