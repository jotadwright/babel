(ql:quickload :muhai-cookingbot)

(in-package :muhai-cookingbot)

;; The 'trace-irl' monitor will make sure that
;; the IRL evaluation process is shown on the web
;; interface (which can be found at localhost:8000).
;; We need to activate it:
;(activate-monitor trace-irl)

;; ##################################################################
;; Almond Crescent Cookies recipe
;; https://www.food.com/recipe/almond-crescent-cookies-424789
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
                                   (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'small-bowl) (make-instance 'small-bowl) (make-instance 'small-bowl)

                                   ;; tools
                                   (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                   (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                   (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)

                                   (make-instance 'mixer)

                                   ;; baking equipment
                                   (make-instance 'cookie-sheet)
                                   (make-instance 'baking-tray)
                                   (make-instance 'baking-paper))))))

;; 'make-html' makes an HTML representation of the kitchen state
;; and 'add-element' transfers that to the web interface
;(add-element (make-html *initial-kitchen-state* :expand-initially t))

(defparameter *almond-cookies-recipe*
  '((get-kitchen ?kitchen)
    
    ;; "230 grams unsalted butter, room temperature"
    (fetch-and-proportion ?proportioned-butter ?ks-with-butter ?kitchen ?target-container-1 butter 230 g)
    (bring-to-temperature ?warm-butter ?ks-with-warm-butter ?ks-with-butter ?proportioned-butter ?room-temp-quantity ?room-temp-unit)
    
     ;; "200 grams granulated sugar"
    (fetch-and-proportion ?proportioned-sugar ?ks-with-sugar ?ks-with-warm-butter ?target-container-2 white-sugar 200 g)
    
    ;; "1 teaspoon vanilla extract"
    (fetch-and-proportion ?proportioned-vanilla ?ks-with-vanilla ?ks-with-sugar ?target-container-3 vanilla-extract 1 teaspoon)
    
    ;; "2 teaspoons almond extract"
    (fetch-and-proportion ?proportioned-almond ?ks-with-almond ?ks-with-vanilla ?target-container-4 almond-extract 2 teaspoon)
    
    ;; "280 grams all-purpose flour"
    (fetch-and-proportion ?proportioned-flour ?ks-with-flour ?ks-with-almond ?target-container-5 all-purpose-flour 280 g)
    
    ;; "120 grams almond flour"
    (fetch-and-proportion ?proportioned-almond-flour ?ks-with-almond-flour ?ks-with-flour ?target-container-6 almond-flour 120 g)
    
    ;; "120 grams powdered sugar"
    (fetch-and-proportion ?proportioned-powdered-sugar ?ks-with-powdered-sugar ?ks-with-almond-flour ?target-container-7 powdered-white-sugar 120 g)


    ;; "Preheat oven to 175 degrees C."
    (preheat-oven ?preheated-oven ?ks-with-preheated-oven ?ks-with-powdered-sugar ?oven 175 degrees-celsius) 

    ;; "Grease cookie sheet."
    (fetch ?cookie-sheet ?ks-with-cookie-sheet ?ks-with-preheated-oven cookie-sheet 1) ;; IMPLICIT
    (grease ?greased-sheet ?ks-with-greased-sheet ?ks-with-cookie-sheet ?cookie-sheet ?grease)

    ;; "With a mixer, beat the butter with sugar until light and fluffy."
    (fetch ?mixer ?ks-with-mixer ?ks-with-greased-sheet mixer 1) ;; IMPLICIT
    (transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-mixer ?empty-container-a ?warm-butter ?quantity-a ?unit-a)
    (transfer-contents ?output-container-b ?rest-b ?output-ks-b ?output-ks-a ?output-container-a ?proportioned-sugar ?quantity-b ?unit-b)
    (beat ?beaten-mixture ?ks-with-beaten-mixture ?output-ks-b ?output-container-b ?mixer)
    
    ;; "Add vanilla extract and almond extract, beat until incorporated."
    (transfer-contents ?output-container-c ?rest-c ?output-ks-c ?ks-with-beaten-mixture ?beaten-mixture ?proportioned-vanilla ?quantity-c ?unit-c) 
    (transfer-contents ?output-container-d ?rest-d ?output-ks-d ?output-ks-c ?output-container-c ?proportioned-almond ?quantity-d ?unit-d)
    (mix ?intermediate-mixture ?ks-with-intermediate-mixture ?output-ks-d ?output-container-d ?mixer) ; reuse the mixing tool

    ;; "Stir in the flour and almonds. Work flour mixture into a firm dough."
    (transfer-contents ?output-container-e ?rest-e ?output-ks-e ?ks-with-intermediate-mixture ?intermediate-mixture ?proportioned-flour ?quantity-e ?unit-e)
    (transfer-contents ?output-container-f ?rest-f ?output-ks-f ?output-ks-e ?intermediate-mixture ?proportioned-almond-flour ?quantity-f ?unit-f)
    (mix ?dough ?ks-with-dough ?output-ks-f ?output-container-f ?mixing-tool) ; use a regular mixing tool

    ;; "Working with 1 tablespoon of dough at a time, lightly roll and then shape it so the middle is thicker than both ends. Bend dough log into a crescent shape."
    (portion-and-arrange ?portioned-dough ?ks-with-dough-portions ?ks-with-dough ?dough 25 g ?pattern ?countertop)
    (shape ?bakeable-balls ?ks-with-balls ?ks-with-dough-portions ?portioned-dough ball-shape)
    (shape ?bakeable-crescents ?ks-with-crescents ?ks-with-balls ?bakeable-balls crescent-shape)

    ;; "Place on greased cookie sheets and repeat until all dough is used."
    (transfer-items ?tray-with-crescents ?ks-with-crescents-tray ?ks-with-crescents ?bakeable-crescents ?default-pattern ?greased-sheet)

    ;; "Bake 12 minutes."
    (bake ?baked-crescents ?ks-with-baked-crescents ?ks-with-crescents-tray ?tray-with-crescents ?oven 12 minute ?temp-qty ?temp-unit)

    ;; "Sift powdered sugar into a small bowl."
    (fetch ?small-bowl ?ks-with-small-bowl ?ks-with-baked-crescents small-bowl 1) ;; IMPLICIT
    (sift ?sifted-powdered-sugar ?ks-after-sifting ?ks-with-small-bowl ?small-bowl ?proportioned-powdered-sugar ?sifting-tool)

    ;; "While the cookies are still warm , dip the crescents in the powdered sugar."
    (dip ?dipped-cookies ?ks-with-dipped-cookies ?ks-after-sifting ?baked-crescents ?sifted-powdered-sugar)

    ;; "Cool on racks."
    (fetch ?wire-rack ?ks-with-wire-rack ?ks-with-dipped-cookies wire-rack 1) ;; IMPLICIT
    (transfer-items ?cookies-on-wire-rack ?ks-with-cookies-on-wire-rack ?ks-with-wire-rack ?dipped-cookies ?default-pattern ?wire-rack)
    (bring-to-temperature ?cooled-cookies ?ks-with-cooled-cookies ?ks-with-cookies-on-wire-rack ?cookies-on-wire-rack ?room-temp-quantity ?room-temp-unit)))

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
