(ql:quickload :muhai-cookingbot)

(in-package :muhai-cookingbot)

;; The 'trace-irl' monitor will make sure that
;; the IRL evaluation process is shown on the web
;; interface (which can be found at localhost:8000).
;; We need to activate it:
;(activate-monitor trace-irl)

;; ######################################################################
;; Chocolate Cream Cheese Cupcakes
;; https://www.food.com/recipe/chocolate-cream-cheese-cupcakes-87623
;; ######################################################################

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
                                                                                                                              :value 500)))))
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
                                                                                                                              :value 5)))))))
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
                                                       :contents (list (make-instance 'cocoa-powder :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 100)))))
                                        (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'vanilla :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 100)))))
                                        (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'salt :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 100)))))
                                        (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'semisweet-chocolate-chips :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 300)))))
                                        (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'baking-soda :amount
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
                                                       :contents (list (make-instance 'water :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'l)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 1)))))
                                        (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'extra-virgin-olive-oil :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'l)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 1)))))
                                        (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'cider-vinegar :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'l)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 1)))))
                                        (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'cream-cheese :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 500)))))
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
                                   (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'small-bowl) (make-instance 'small-bowl) (make-instance 'small-bowl)

                                   ;; tools
                                   (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                   (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                   (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                   (make-instance 'wooden-spoon) (make-instance 'wooden-spoon) (make-instance 'wooden-spoon)

                                   (make-instance 'paper-baking-cups :items
                                                  (list (make-instance 'paper-baking-cup) (make-instance 'paper-baking-cup) (make-instance 'paper-baking-cup)
                                                        (make-instance 'paper-baking-cup) (make-instance 'paper-baking-cup) (make-instance 'paper-baking-cup)
                                                        (make-instance 'paper-baking-cup) (make-instance 'paper-baking-cup) (make-instance 'paper-baking-cup)
                                                        (make-instance 'paper-baking-cup) (make-instance 'paper-baking-cup) (make-instance 'paper-baking-cup)
                                                        (make-instance 'paper-baking-cup) (make-instance 'paper-baking-cup) (make-instance 'paper-baking-cup)))

                                   ;; baking equipment
                                   (make-instance 'muffin-tins-12)
                                   (make-instance 'baking-tray)
                                   (make-instance 'baking-paper))))))

;; 'make-html' makes an HTML representation of the kitchen state
;; and 'add-element' transfers that to the web interface
;(add-element (make-html *initial-kitchen-state* :expand-initially t))

(defparameter *chocolate-cream-cheese-cupcakes-recipe*
  '((get-kitchen ?kitchen)
    
    ;; "230 grams cream cheese, softened"
    (fetch-and-proportion ?proportioned-cheese ?ks-with-cheese ?kitchen ?target-container-1 cream-cheese 230 g)
    (bring-to-temperature ?softened-cheese ?ks-with-softened-cheese ?ks-with-cheese ?proportioned-cheese ?room-temp-quantity ?room-temp-unit)
    
     ;; "egg, slightly beaten"
    (fetch-and-proportion ?proportioned-egg ?ks-with-egg ?ks-with-softened-cheese ?target-container-2 egg 1 piece)
    (crack ?cracked-egg ?ks-with-cracked-egg ?ks-with-egg ?proportioned-egg ?empty-bowl) ;; IMPLICIT
    (beat ?beaten-egg ?ks-with-beaten-egg ?ks-with-egg ?proportioned-egg ?mixing-tool)
    
    ;; "70 grams sugar"
    (fetch-and-proportion ?proportioned-sugar-70 ?ks-with-sugar-70 ?ks-with-beaten-egg ?target-container-3 white-sugar 70 g)
    
    ;; "1/2 teaspoon salt"
    (fetch-and-proportion ?proportioned-salt-1 ?ks-with-salt-1 ?ks-with-sugar-70 ?target-container-4 salt 0.5 teaspoon)
    
    ;; "180 grams chocolate chips"
    (fetch-and-proportion ?proportioned-chocolate-chips ?ks-with-chocolate-chips ?ks-with-salt-1 ?target-container-5 chocolate-chips 180 g)
    
    ;; "210 grams sugar"
    (fetch-and-proportion ?proportioned-sugar-210 ?ks-with-sugar-210 ?ks-with-chocolate-chips ?target-container-6 white-sugar 210 g)
    
    ;; "180 grams flour"
    (fetch-and-proportion ?proportioned-flour ?ks-with-flour ?ks-with-sugar-210 ?target-container-7 all-purpose-flour 180 g)

    ;; "30 grams cocoa"
    (fetch-and-proportion ?proportioned-cocoa ?ks-with-cocoa ?ks-with-flour ?target-container-8 cocoa-powder 30 g)

    ;; "1 teaspoon baking soda"
    (fetch-and-proportion ?proportioned-baking-soda ?ks-with-baking-soda ?ks-with-cocoa ?target-container-9 baking-soda 1 teaspoon)

    ;; "1/2 teaspoon salt"
    (fetch-and-proportion ?proportioned-salt-2 ?ks-with-salt-2 ?ks-with-baking-soda ?target-container-10 salt 0.5 teaspoon)

    ;; "250 ml water"
    (fetch-and-proportion ?proportioned-water ?ks-with-water ?ks-with-salt-2 ?target-container-11 water 250 ml)

    ;; "120 ml oil"
    (fetch-and-proportion ?proportioned-oil ?ks-with-oil ?ks-with-water ?target-container-12 oil 120 ml)

    ;; "1 tablespoon vinegar"
    (fetch-and-proportion ?proportioned-vinegar ?ks-with-vinegar ?ks-with-oil ?target-container-13 vinegar 1 tablespoon)

    ;; "1 teaspoon vanilla"
    (fetch-and-proportion ?proportioned-vanilla ?ks-with-vanilla ?ks-with-vinegar ?target-container-14 vanilla 1 teaspoon)

    ;; "In a small bowl, combine first four ingredients."
    (fetch ?small-bowl ?ks-with-small-bowl ?ks-with-vanilla small-bowl 1) ;; IMPLICIT

    (transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-small-bowl ?small-bowl ?softened-cheese ?quantity-a ?unit-a)
    (transfer-contents ?output-container-b ?rest-b ?output-ks-b ?output-ks-a ?output-container-a ?beaten-egg ?quantity-b ?unit-b)
    (transfer-contents ?output-container-c ?rest-c ?output-ks-c ?output-ks-b ?output-container-b ?proportioned-sugar-70 ?quantity-c ?unit-c)
    (transfer-contents ?output-container-d ?rest-d ?output-ks-d ?output-ks-c ?output-container-c ?proportioned-salt-1 ?quantity-d ?unit-d)
    (mix ?intermediate-mixture ?ks-with-intermediate-mixture ?output-ks-d ?output-container-d ?mixing-tool) ; reuse the mixing tool

    ;; "Add chocolate chips and set aside."
    (transfer-contents ?output-container-e ?rest-e ?output-ks-e ?ks-with-intermediate-mixture ?intermediate-mixture ?proportioned-chocolate-chips ?quantity-e ?unit-e)
    (mingle ?chips-mixture ?ks-with-chips-mixture ?output-ks-e ?output-container-e ?mingling-tool)

    ;; "Mix remaining ingredients well."
    (transfer-contents ?output-container-f ?rest-f ?output-ks-f ?ks-with-chips-mixture ?empty-large-bowl ?proportioned-sugar-210 ?quantity-f ?unit-f)
    (transfer-contents ?output-container-g ?rest-g ?output-ks-g ?output-ks-f ?output-container-f ?proportioned-flour ?quantity-g ?unit-g)
    (transfer-contents ?output-container-h ?rest-h ?output-ks-h ?output-ks-g ?output-container-g ?proportioned-cocoa ?quantity-h ?unit-h)
    (transfer-contents ?output-container-i ?rest-i ?output-ks-i ?output-ks-h ?output-container-h ?proportioned-baking-soda ?quantity-i ?unit-i)
    (transfer-contents ?output-container-j ?rest-j ?output-ks-j ?output-ks-i ?output-container-i ?proportioned-salt-2 ?quantity-j ?unit-j)
    (transfer-contents ?output-container-k ?rest-k ?output-ks-k ?output-ks-j ?output-container-j ?proportioned-water ?quantity-k ?unit-k)
    (transfer-contents ?output-container-l ?rest-l ?output-ks-l ?output-ks-k ?output-container-k ?proportioned-oil ?quantity-l ?unit-l)
    (transfer-contents ?output-container-m ?rest-m ?output-ks-m ?output-ks-l ?output-container-l ?proportioned-vinegar ?quantity-m ?unit-m)
    (transfer-contents ?output-container-n ?rest-n ?output-ks-n ?output-ks-m ?output-container-m ?proportioned-vanilla ?quantity-n ?unit-n)
    (mix ?dough ?ks-with-dough ?output-ks-n ?output-container-n ?mixing-tool) ; reuse the mixing tool

    ;; "Fill muffin tins, lined with paper cups with this mixture."
    (fetch ?muffin-tins ?ks-with-muffin-tins ?ks-with-dough muffin-tins 1)
    (line ?lined-tins ?ks-with-lined-tins ?ks-with-muffin-tins ?muffin-tins paper-baking-cups)
    (portion-and-arrange ?portioned-dough ?ks-with-dough-portions ?ks-with-lined-tins ?dough ?portioning-quantity ?portioning-unit ?pattern ?lined-tins)
    
    ;; "Bake at 180 for 20 minutes."
    (bake ?baked-cupcakes ?ks-with-baked-cupcakes ?ks-with-dough-portions ?portioned-dough ?oven 20 minute 180 degrees-celsius)))

;; ======================
;; Append bindings to the recipe
;; ======================

(defparameter *extended-recipe*
  (append-meaning-and-irl-bindings *chocolate-cream-cheese-cupcakes-recipe* nil))

;; ======================
;; Evaluate the recipe
;; ======================

;(activate-monitor trace-irl)

;(clear-output)

;(evaluate-irl-program *extended-recipe* nil)

;; ======================
;; Visualise the recipe
;; ======================

;(draw-recipe *chocolate-cream-cheese-cupcakes-recipe*)
;(draw-recipe *extended-recipe*)
