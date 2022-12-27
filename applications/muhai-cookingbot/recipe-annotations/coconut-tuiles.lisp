(ql:quickload :muhai-cookingbot)

(in-package :muhai-cookingbot)

;; The 'trace-irl' monitor will make sure that
;; the IRL evaluation process is shown on the web
;; interface (which can be found at localhost:8000).
;; We need to activate it:
;(activate-monitor trace-irl)

;; #####################################################
;; Coconut Tuiles recipe
;; https://www.simplyrecipes.com/recipes/coconut_tuiles
;; #####################################################

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
                                                       :contents (list (make-instance 'egg-white
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
                                                        :contents (list (make-instance 'shredded-coconut
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
                                   (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)
                                   (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)

                                   ;; tools
                                   (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                   (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                   (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                   (make-instance 'mixer) (make-instance 'mixer) (make-instance 'mixer)

                                   ;; lids
                                   (make-instance 'large-bowl-lid) (make-instance 'large-bowl-lid) (make-instance 'large-bowl-lid)
                                   (make-instance 'plastic-wrap)
                                   
                                   ;; baking equipment
                                   (make-instance 'wire-rack)
                                   (make-instance 'baking-tray)
                                   (make-instance 'baking-paper))))))

;; 'make-html' makes an HTML representation of the kitchen state
;; and 'add-element' transfers that to the web interface
;(add-element (make-html *initial-kitchen-state* :expand-initially t))

(defparameter *coconut-tuiles-recipe*
  '((get-kitchen ?kitchen)
    
    ;; "230 grams of unsalted butter, softened"
    (fetch-and-proportion ?proportioned-butter ?ks-with-butter ?kitchen ?target-container-1 butter 230 g)
    ; softening can be done simply be letting it warm up to room temperature
    (bring-to-temperature ?warm-butter ?ks-with-warm-butter ?ks-with-butter ?proportioned-butter ?room-temp-quantity ?room-temp-unit)
    
     ;; "100 grams sugar"
    (fetch-and-proportion ?proportioned-sugar ?ks-with-sugar ?ks-with-warm-butter ?target-container-2 white-sugar 100 g)
    
    ;; "80 grams of egg whites, lightly beaten"
    (fetch-and-proportion ?proportioned-egg-white ?ks-with-egg-white ?ks-with-sugar ?target-container-3 egg-white 80 g)
    (beat ?beaten-egg-white ?ks-with-beaten-egg-white ?ks-with-egg-white ?proportioned-egg-white ?mixing-tool)

    ;; "3 tablespoons all-purpose flour"
    (fetch-and-proportion ?proportioned-flour ?ks-with-flour ?ks-with-beaten-egg-white ?target-container-4 all-purpose-flour 3 tablespoon)
    
    ;; "200 grams shredded coconut, divided"
    (fetch-and-proportion ?proportioned-coconut-34 ?ks-with-coconut-34 ?ks-with-flour ?target-container-5 shredded-coconut 150 g)
    (fetch-and-proportion ?proportioned-coconut-14 ?ks-with-coconut-14 ?ks-with-coconut-34 ?target-container-6 shredded-coconut 50 g)

    ;; "Preheat oven to 200°C."
    (preheat-oven ?preheated-oven ?ks-with-preheated-oven ?ks-with-coconut-14 ?oven 200 degrees-celsius)
    
    ;; "Using an electric mixer, beat the butter."
    (fetch ?mixer ?ks-with-mixer ?ks-with-coconut-14 mixer 1)
    (beat ?beaten-butter ?ks-with-beaten-butter ?ks-with-mixer ?warm-butter ?mixer)

    ;; "Add the sugar and beat until light and fluffy."
    (transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-beaten-butter ?empty-bowl ?beaten-butter ?quantity-a ?unit-a)
    (transfer-contents ?output-container-b ?rest-b ?output-ks-b ?output-ks-a ?output-container-a ?proportioned-sugar ?quantity-b ?unit-b)
    (beat ?beaten-sugar-mixture ?ks-with-beaten-sugar-mixture ?ks-with-mixer ?proportioned-sugar ?mixer)

    ;; "Add the egg whites, beat slowly to smoothen the mixture."
    (beat ?beaten-egg-mixture ?ks-with-beaten-egg-mixture ?ks-with-beaten-sugar-mixture ?proportioned-egg-white ?mixing-tool)

    ;; "Add flour to the mixture and fold it in gently with 150 grams of the coconut."
    (transfer-contents ?output-container-c ?rest-c ?output-ks-c ?ks-with-beaten-egg-mixture ?beaten-egg-mixture ?proportioned-flour ?quantity-c ?unit-c)
    (transfer-contents ?output-container-d ?rest-d ?output-ks-d ?output-ks-c ?output-container-c ?proportioned-coconut-34 ?quantity-d ?unit-d)
    (mix ?dough ?ks-with-dough ?output-ks-d ?output-container-d ?mixing-tool)

    ;; "Spoon rounded teaspoons of the mixture (around 25 grams) 5 cm apart on to a baking sheet that has been greased."
    (fetch ?baking-tray ?ks-with-baking-tray ?ks-with-dough baking-tray 1)
    (grease ?greased-tray ?ks-with-greased-tray ?ks-with-baking-tray ?baking-tray ?grease)
    (portion-and-arrange ?portioned-dough ?ks-with-dough-portions ?ks-with-greased-tray ?dough 25 g 5-cm-apart ?greased-tray)

    ;; "With the remaining 50 grams coconut, sprinkle a little over each round."
    (sprinkle ?sprinkled-dough ?ks-with-sprinkled-dough ?ks-with-dough-portions ?portioned-dough ?proportioned-coconut-14)

    ;; "Bake the rounds at 200°C in the oven for 6 minutes."
    (bake ?baked-rounds ?ks-with-baked-rounds ?ks-with-sprinkled-dough ?sprinkled-dough ?oven 6 minute 200 degrees-celsius)

    ;; "Let the cookies stay on the baking sheets for half a minute, so they are just firm enough to hold their shape."
    (cool-for-time ?cooling-rounds ?ks-with-cooling-rounds ?ks-with-baked-rounds ?baked-rounds 0.5 minute)

    ;; "Then let the cookies cool completely on wire racks."
    (fetch ?wire-rack ?ks-with-wire-rack ?ks-with-cooling-rounds wire-rack 1) ;; IMPLICIT
    (transfer-items ?rounds-on-wire-rack ?ks-with-rounds-on-wire-rack ?ks-with-wire-rack ?cooling-rounds ?default-pattern ?wire-rack)
    (bring-to-temperature ?cooled-rounds ?ks-with-cooled-cookies ?ks-with-rounds-on-wire-rack ?rounds-on-wire-rack ?room-temp-qty-2 ?room-temp-unit-2)))


;; ======================
;; Append bindings to the recipe
;; ======================

(defparameter *extended-recipe*
  (append-meaning-and-irl-bindings *coconut-tuiles-recipe* nil))

;; ======================
;; Evaluate the recipe
;; ======================

;(activate-monitor trace-irl)

;(clear-output)

;(evaluate-irl-program *extended-recipe* nil)

;; ======================
;; Visualise the recipe
;; ======================

;(draw-recipe *coconut-tuiles-recipe*)
;(draw-recipe *extended-recipe*)
