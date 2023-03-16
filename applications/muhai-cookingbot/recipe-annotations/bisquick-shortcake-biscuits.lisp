(ql:quickload :muhai-cookingbot)

(in-package :muhai-cookingbot)

;; The 'trace-irl' monitor will make sure that
;; the IRL evaluation process is shown on the web
;; interface (which can be found at localhost:8000).
;; We need to activate it:
;(activate-monitor trace-irl)

;; ########################################################################
;; Bisquick Shortcake Biscuits recipe
;; https://www.simplyrecipes.com/recipes/bisquick_shortcake_biscuits
;; ########################################################################

;; Defining the initial kitchen state
(defparameter *initial-kitchen-state* 
  (make-instance 
   'kitchen-state
   :contents
   (list (make-instance 'fridge
                        :contents (list
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'milk
                                                                                 :temperature
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'degrees-celsius)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 5))
                                                                                 :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'l)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 0.5)))))

                                   (make-instance 'medium-bowl
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
                                                       :contents (list (make-instance 'bisquick-baking-mix :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 300)))))
                                        (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'coconut-oil :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'l)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 0.5)))))
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

                                   (make-instance 'bread-knife) (make-instance 'bread-knife) (make-instance 'bread-knife)

                                   ;; baking equipment
                                   (make-instance 'wire-rack)
                                   (make-instance 'cookie-sheet)
                                   (make-instance 'baking-tray)
                                   (make-instance 'baking-paper))))))

;; 'make-html' makes an HTML representation of the kitchen state
;; and 'add-element' transfers that to the web interface
;(add-element (make-html *initial-kitchen-state* :expand-initially t))

(defparameter *bisquick-shortcake-biscuits-recipe*
  '((get-kitchen ?kitchen)
    
    ;; "260 grams Bisquick baking mix"
    (fetch-and-proportion ?proportioned-bisquick ?ks-with-bisquick ?kitchen ?target-container-1 bisquick-baking-mix 260 g)

    ;; "3 tablespoons butter"
    (fetch-and-proportion ?proportioned-butter ?ks-with-butter ?ks-with-bisquick ?target-container-2 butter 3 tablespoon)
    
    ;; "3 tablespoons coconut-oil"
    (fetch-and-proportion ?proportioned-coconut-oil ?ks-with-coconut-oil ?ks-with-butter ?target-container-3 coconut-oil 3 tablespoon)
    
    ;; "250 ml milk"
    (fetch-and-proportion ?proportioned-milk ?ks-with-milk ?ks-with-coconut-oil ?target-container-4 milk 250 ml)
    
    ;; "3 tablespoons sugar"
    (fetch-and-proportion ?proportioned-sugar ?ks-with-sugar ?ks-with-milk ?target-container-5 white-sugar 3 tablespoon)
      
    ;; "Heat oven to 220°C."
    (preheat-oven ?preheated-oven ?ks-with-preheated-oven ?ks-with-sugar ?oven 220 degrees-celsius)

    ;; "Whisk together the baking mix and sugar in a medium bowl. Stir in the milk and coconut oil. Don't over-mix!"
    (fetch ?medium-bowl ?ks-with-medium-bowl ?ks-with-preheated-oven medium-bowl 1) ; IMPLICIT

    (transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-medium-bowl ?medium-bowl ?proportioned-bisquick ?quantity-a ?unit-a)
    (transfer-contents ?output-container-b ?rest-b ?output-ks-b ?output-ks-a ?medium-bowl ?proportioned-sugar ?quantity-b ?unit-b)
    (mix ?intermediate-mixture ?ks-with-intermediate-mixture ?output-ks-b ?output-container-b ?mixing-tool)

    (transfer-contents ?output-container-c ?rest-c ?output-ks-c ?ks-with-intermediate-mixture ?intermediate-mixture ?proportioned-milk ?quantity-c ?unit-c)
    (transfer-contents ?output-container-d ?rest-d ?output-ks-d ?output-ks-c ?output-container-c ?proportioned-coconut-oil ?quantity-d ?unit-d)
    (mix ?dough ?ks-with-dough ?output-ks-d ?output-container-d ?mixing-tool) ; reuse the mixing tool

    ;; "Grease a cookie sheet with butter. Divide the dough into roughly 6 equal portions (should be around 100g each) and drop them on the cookie sheet."
    (fetch ?cookie-sheet ?ks-with-cookie-sheet ?ks-with-dough cookie-sheet 1) ;; IMPLICIT
    (grease ?greased-sheet ?ks-with-greased-sheet ?ks-with-cookie-sheet ?cookie-sheet ?proportioned-butter)
    (portion-and-arrange ?portioned-dough ?ks-with-portioned-dough ?ks-with-dough ?dough 100 g ?pattern ?countertop)
    (transfer-items ?portions-on-sheet ?ks-with-portions-on-sheet ?ks-with-portioned-dough ?portioned-dough ?default-pattern ?greased-sheet)

    ;; "Bake at 220°C for 10 minutes. Cool for 5 minutes on a rack."
    (bake ?baked-biscuit ?ks-with-baked-biscuit ?ks-with-portions-on-sheet ?portions-on-sheet ?oven 10 minute 220 degrees-celsius)

    (fetch ?wire-rack ?ks-with-wire-rack ?ks-with-baked-biscuit wire-rack 1) ;; IMPLICIT
    (transfer-items ?biscuit-on-wire-rack ?ks-with-biscuit-on-wire-rack ?ks-with-wire-rack ?baked-biscuit ?default-pattern-2 ?wire-rack)
    (leave-for-time ?cooling-biscuit ?ks-with-cooling-biscuit ?ks-with-biscuit-on-wire-rack ?biscuit-on-wire-rack 5 minute)

    ;; "Slice with a bread knife to serve for shortcake. It goes well with berries."
    (fetch ?bread-knife ?ks-with-bread-knife ?ks-with-cooling-biscuit bread-knife 1) ;; IMPLICIT
    (cut ?bisquick-shortcake-biscuits ?ks-with-bisquick-shortcake-biscuits ?ks-with-bread-knife ?cooling-biscuit slices ?bread-knife)))

;; ======================
;; Append bindings to the recipe
;; ======================

(defparameter *extended-recipe*
  (append-meaning-and-irl-bindings *bisquick-shortcake-biscuits-recipe* nil))

;; ======================
;; Evaluate the recipe
;; ======================

;(activate-monitor trace-irl)

;(clear-output)

;(evaluate-irl-program *extended-recipe* nil)

;; ======================
;; Visualise the recipe
;; ======================

;(draw-recipe *bisquick-shortcake-biscuits-recipe*)
;(draw-recipe *extended-recipe*)
