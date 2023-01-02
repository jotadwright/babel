(ql:quickload :muhai-cookingbot)

(in-package :muhai-cookingbot)

;; The 'trace-irl' monitor will make sure that
;; the IRL evaluation process is shown on the web
;; interface (which can be found at localhost:8000).
;; We need to activate it:
;(activate-monitor trace-irl)

;; ##########################################################################################
;; Polish Almond Crescent Cookies recipe
;; https://www.thespruceeats.com/polish-almond-crescent-cookies-rogaliki-recipe-1136973
;; ##########################################################################################

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
                                                                                                                              :value 6)))))))
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
                                   (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)
                                   (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)
                                   (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)
                                   (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)
                                   (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)
                                   (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)

                                   ;; tools
                                   (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                   (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                   (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)

                                   (make-instance 'egg-separator) (make-instance 'egg-separator) (make-instance 'egg-separator)

                                   ;; lids
                                   (make-instance 'large-bowl-lid) (make-instance 'large-bowl-lid) (make-instance 'large-bowl-lid)
                                   (make-instance 'plastic-wrap)
                                   
                                   ;; baking equipment
                                   (make-instance 'baking-tray)
                                   (make-instance 'baking-paper))))))

;; 'make-html' makes an HTML representation of the kitchen state
;; and 'add-element' transfers that to the web interface
;(add-element (make-html *initial-kitchen-state* :expand-initially t))

(defparameter *almond-cookies-recipe*
  '((get-kitchen ?kitchen)
    
    ;; "230 grams unsalted butter, at room temperature"
    (fetch-and-proportion ?proportioned-butter ?ks-with-butter ?kitchen ?target-container-1 butter 230 g)
    (bring-to-temperature ?warm-butter ?ks-with-warm-butter ?ks-with-butter ?proportioned-butter ?room-temp-quantity ?room-temp-unit)
    
     ;; "100 grams sugar"
    (fetch-and-proportion ?proportioned-sugar ?ks-with-sugar ?ks-with-warm-butter ?target-container-2 white-sugar 100 g)
    
    ;; "1 egg yolk, at room temperature"
    (fetch-and-proportion ?proportioned-egg ?ks-with-egg ?ks-with-sugar ?target-container-3 egg 1 piece)
    (crack ?cracked-egg ?ks-with-cracked-egg ?ks-with-egg ?proportioned-egg ?empty-bowl-1)
    (separate-eggs ?egg-yolk ?egg-white ?ks-with-separated-egg ?ks-with-cracked-egg ?cracked-egg ?empty-bowl-2 ?empty-bowl-3 ?egg-separator)
    (bring-to-temperature ?warm-egg ?ks-with-warm-egg ?ks-with-separated-egg ?egg-yolk ?room-temp-quantity-2 ?room-temp-unit-2)
    
    ;; "1 teaspoon pure vanilla extract"
    (fetch-and-proportion ?proportioned-vanilla ?ks-with-vanilla ?ks-with-warm-egg ?target-container-4 vanilla-extract 1 teaspoon)
    
    ;; "25 grams almond flour"
    (fetch-and-proportion ?proportioned-almond-flour ?ks-with-flour ?ks-with-vanilla ?target-container-5 almond-flour 25 g)
    
    ;; "200 grams all-purpose flour"
    (fetch-and-proportion ?proportioned-flour ?ks-with-almond-flour ?ks-with-flour ?target-container-6 all-purpose-flour 200 g)
    
    ;; "30 grams confectioner's sugar, for garnish"
    (fetch-and-proportion ?proportioned-powdered-sugar ?ks-with-powdered-sugar ?ks-with-almond-flour ?target-container-7 powdered-white-sugar 30 g)

    ;; "Gather the ingredients."
    ; (already happened)
    
    ;; "Preheat the oven to 175 C and grab a baking sheet."
    (preheat-oven ?preheated-oven ?ks-with-preheated-oven ?ks-with-powdered-sugar ?oven 175 degrees-celsius)
    (fetch ?baking-tray ?ks-with-baking-tray ?ks-with-preheated-oven baking-tray 1)

    ;; "Cream 230 grams room-temperature butter and 100 grams sugar in a large bowl."
    (fetch ?large-bowl ?ks-with-large-bowl ?ks-with-baking-tray large-bowl 1) ;; IMPLICIT

    (transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-large-bowl ?large-bowl ?warm-butter ?quantity-a ?unit-a)
    (transfer-contents ?output-container-b ?rest-b ?output-ks-b ?output-ks-a ?output-container-a ?proportioned-sugar ?quantity-b ?unit-b)
    (beat ?beaten-mixture ?ks-with-beaten-mixture ?output-ks-b ?output-container-b ?mixing-tool)
    
    ;; "Beat in 1 room-temperature egg and 1 teaspoon vanilla, mixing well."
    (transfer-contents ?output-container-c ?rest-c ?output-ks-c ?ks-with-beaten-mixture ?beaten-mixture ?warm-egg ?quantity-c ?unit-c) 
    (transfer-contents ?output-container-d ?rest-d ?output-ks-d ?output-ks-c ?output-container-c ?proportioned-vanilla ?quantity-d ?unit-d)
    (beat ?intermediate-mixture ?ks-with-intermediate-mixture ?output-ks-d ?output-container-d ?mixing-tool) ; reuse the mixing tool

    ;; "Add 25 grams almond flour and 200 grams all-purpose flour, thoroughly incorporating."
    (transfer-contents ?output-container-e ?rest-e ?output-ks-e ?ks-with-intermediate-mixture ?intermediate-mixture ?proportioned-almond-flour ?quantity-e ?unit-e)
    (transfer-contents ?output-container-f ?rest-f ?output-ks-f ?output-ks-e ?output-container-e ?proportioned-flour ?quantity-f ?unit-f)
    (mix ?dough ?ks-with-dough ?output-ks-f ?output-container-f ?mixing-tool) ; reuse the mixing tool

    ;; "Shape pieces of dough (should be about 25 grams), into crescents and place on the ungreased baking sheets."
    (portion-and-arrange ?portioned-dough ?ks-with-dough-portions ?ks-with-dough ?dough 25 g ?pattern ?countertop)
    (shape ?bakeable-crescents ?ks-with-crescents ?ks-with-dough-portions ?portioned-dough crescent-shape)
    (transfer-items ?tray-with-crescents ?ks-with-crescents-tray ?ks-with-crescents ?bakeable-crescents ?default-pattern ?baking-tray)
    
    ;; "Bake 20 minutes."
    (bake ?baked-crescents ?ks-with-baked-crescents ?ks-with-crescents-tray ?tray-with-crescents ?preheated-oven 20 minute ?bake-temp-qty ?bake-temp-unit)

    ;; "Dip in confectioners' sugar while still hot. When cool, put them in a large bowl and cover it."
    (dip ?dipped-hot-crescents ?ks-with-dipped-hot-crescents ?ks-with-baked-crescents ?baked-crescents ?proportioned-powdered-sugar)
    (bring-to-temperature ?cooled-crescents ?ks-with-cooled-crescents ?ks-with-dipped-hot-crescents ?dipped-hot-crescents ?room-temp-quantity-2 ?room-temp-unit-2)

    (fetch ?large-bowl-2 ?ks-with-large-bowl-2 ?ks-with-cooled-crescents large-bowl 1) ;; IMPLICIT
    (transfer-contents ?output-container-g ?rest-g ?output-ks-g ?ks-with-large-bowl-2 ?large-bowl-2 ?cooled-crescents ?quantity-g ?unit-g)
    (cover ?covered-cookies ?ks-with-covered-cookies ?output-ks-g ?output-container-g ?cover)))

;; ======================
;; Append bindings to the recipe
;; ======================

(defparameter *extended-recipe*
  (append-meaning-and-irl-bindings *almond-cookies-recipe* nil))

;; ======================
;; Evaluate the recipe
;; ======================

;(activate-monitor trace-irl)

;(clear-output)

;(evaluate-irl-program *extended-recipe* nil)

;; ======================
;; Visualise the recipe
;; ======================

;(draw-recipe *almond-cookies-recipe*)
;(draw-recipe *extended-recipe*)
