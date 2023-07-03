(ql:quickload :muhai-cookingbot)

(in-package :muhai-cookingbot)

;; The 'trace-irl' monitor will make sure that
;; the IRL evaluation process is shown on the web
;; interface (which can be found at localhost:8000).
;; We need to activate it:
(activate-monitor trace-irl)

;; ##################################################################
;; Almond Crescent Cookies recipe
;; https://www.simplyrecipes.com/recipes/almond_crescent_cookies/
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

                                   ;; tools
                                   (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                   (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                   (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)

                                   ;; baking equipment
                                   (make-instance 'baking-tray)
                                   (make-instance 'baking-paper))))))

;; 'make-html' makes an HTML representation of the kitchen state
;; and 'add-element' transfers that to the web interface
;(add-element (make-html *initial-kitchen-state* :expand-initially t))

(defparameter *almond-cookies-recipe*
  '(

    (get-kitchen ?kitchen)

    ;; "230 grams butter, room temperature"
    (fetch-and-proportion ?proportioned-butter ?ks-with-butter ?kitchen ?target-container-1 butter 230 g)
    ;; (bring-to-temperature ?warm-butter ?ks-with-warm-butter ?ks-with-butter ?proportioned-butter ?room-temp-quantity ?room-temp-unit)

    ;;  ;; "120 grams sugar"
    ;; (fetch-and-proportion ?proportioned-sugar ?ks-with-sugar ?ks-with-warm-butter ?target-container-2 white-sugar 120 g)

    ;; ;; "1 teaspoon vanilla extract"
    ;; (fetch-and-proportion ?proportioned-vanilla ?ks-with-vanilla ?ks-with-sugar ?target-container-3 vanilla-extract 1 teaspoon)

    ;; ;; "1 teaspoon almond extract"
    ;; (fetch-and-proportion ?proportioned-almond ?ks-with-almond ?ks-with-vanilla ?target-container-4 almond-extract 1 teaspoon)

    ;; ;; "2 1/2 cups flour"
    ;; (fetch-and-proportion ?proportioned-flour ?ks-with-flour ?ks-with-almond ?target-container-5 all-purpose-flour 340 g)

    ;; ;; "1 cup almond flour"
    ;; (fetch-and-proportion ?proportioned-almond-flour ?ks-with-almond-flour ?ks-with-flour ?target-container-6 almond-flour 120 g)

    ;; ;; "1/4 cup powdered sugar"
    ;; (fetch-and-proportion ?proportioned-powdered-sugar ?ks-with-powdered-sugar ?ks-with-almond-flour ?target-container-7 powdered-white-sugar 30 g)

    ;; ;; "Beat the butter and the sugar together until light and fluffy"
    ;; (transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-powdered-sugar ?empty-container-a ?warm-butter ?quantity-a ?unit-a)
    ;; (transfer-contents ?output-container-b ?rest-b ?output-ks-b ?output-ks-a ?output-container-a ?proportioned-sugar ?quantity-b ?unit-b)
    ;; (beat ?beaten-mixture ?ks-with-beaten-mixture ?output-ks-b ?output-container-b ?mixing-tool)

    ;; ;; "Add the vanilla and almond extracts and mix"
    ;; (transfer-contents ?output-container-c ?rest-c ?output-ks-c ?ks-with-beaten-mixture ?beaten-mixture ?proportioned-vanilla ?quantity-c ?unit-c)
    ;; (transfer-contents ?output-container-d ?rest-d ?output-ks-d ?output-ks-c ?output-container-c ?proportioned-almond ?quantity-d ?unit-d)
    ;; (mix ?intermediate-mixture ?ks-with-intermediate-mixture ?output-ks-d ?output-container-d ?mixing-tool) ; reuse the mixing tool

    ;; ;; "Add the flour and almond flour"
    ;; (transfer-contents ?output-container-e ?rest-e ?output-ks-e ?ks-with-intermediate-mixture ?intermediate-mixture ?proportioned-flour ?quantity-e ?unit-e)
    ;; (transfer-contents ?output-container-f ?rest-f ?output-ks-f ?output-ks-e ?intermediate-mixture ?proportioned-almond-flour ?quantity-f ?unit-f)

    ;; ;; "Mix thoroughly"
    ;; (mix ?dough ?ks-with-dough ?output-ks-f ?output-container-f ?mixing-tool) ; reuse the mixing tool

    ;; ;; "Take generous tablespoons of the dough and roll it into a small ball, about two cm in diameter, and then shape it into a crescent shape"
    ;; (portion-and-arrange ?portioned-dough ?ks-with-dough-portions ?ks-with-dough ?dough 25 g ?pattern ?countertop)
    ;; (shape ?bakeable-balls ?ks-with-balls ?ks-with-dough-portions ?portioned-dough ball-shape)
    ;; (shape ?bakeable-crescents ?ks-with-crescents ?ks-with-balls ?bakeable-balls crescent-shape)

    ;; ;; "Place onto a parchment paper lined baking sheet"
    ;; (fetch ?baking-tray ?ks-with-baking-tray ?ks-with-crescents baking-tray 1)
    ;; (fetch ?baking-paper ?ks-with-baking-paper ?ks-with-baking-tray baking-paper 1)
    ;; (line ?lined-baking-tray ?ks-with-lined-tray ?ks-with-baking-paper ?baking-tray ?baking-paper)
    ;; (transfer-items ?tray-with-crescents ?ks-with-crescents-tray ?ks-with-lined-tray ?bakeable-crescents ?default-pattern ?lined-baking-tray)

    ;; ;; "Bake at 175 C for 15 - 20 minutes"
    ;; (bake ?baked-crescents ?ks-with-baked-crescents ?ks-with-crescents-tray ?tray-with-crescents ?oven 15 minute 175 degrees-celsius)

    ;; ;; "Dust with powdered sugar"
    ;; (sprinkle ?almond-crescent-cookies ?ks-with-almond-crescent-cookies ?ks-with-baked-crescents ?baked-crescents ?proportioned-powdered-sugar)


    ))

;; ======================
;; Append bindings to the recipe
;; ======================


;; ======================
;; append bindings to the recipe
;; ======================

(defparameter *extended-recipe*
  (append-meaning-and-irl-bindings *almond-cookies-recipe* nil))


;; ======================
;; Evaluate the recipe
;; ======================

(evaluate-irl-program *extended-recipe* nil :primitive-inventory *vr-primitives*)

;; ======================
;; Visualise the recipe
;; ======================

;; (draw-recipe *almond-cookies-recipe*)
;; (draw-recipe *extended-recipe*)
