(ql:quickload :muhai-cookingbot)

(in-package :muhai-cookingbot)

;; The 'trace-irl' monitor will make sure that
;; the IRL evaluation process is shown on the web
;; interface (which can be found at localhost:8000).
;; We need to activate it:
;(activate-monitor trace-irl)

;; ##################################################################
;; Mexican Wedding Cookies
;; https://www.food.com/recipe/mexican-wedding-cookies-8278
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
                                                       :contents (list (make-instance 'vanilla :amount
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
                                                       :contents (list (make-instance 'walnut :amount
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

                                   (make-instance 'sift) (make-instance 'sift) (make-instance 'sift)
                                   (make-instance 'food-processor)

                                   ;; baking equipment
                                   (make-instance 'cookie-sheet)
                                   (make-instance 'baking-tray)
                                   (make-instance 'baking-paper))))))

;; 'make-html' makes an HTML representation of the kitchen state
;; and 'add-element' transfers that to the web interface
;(add-element (make-html *initial-kitchen-state* :expand-initially t))

(defparameter *mexican-wedding-cookies-recipe*
  '((get-kitchen ?kitchen)
    
    ;; "230 grams butter, softened"
    (fetch-and-proportion ?proportioned-butter ?ks-with-butter ?kitchen ?target-container-1 butter 230 g)
    (bring-to-temperature ?warm-butter ?ks-with-warm-butter ?ks-with-butter ?proportioned-butter ?room-temp-quantity ?room-temp-unit)
    
     ;; "120 grams powdered sugar"
    (fetch-and-proportion ?proportioned-powdered-sugar ?ks-with-powdered-sugar ?ks-with-warm-butter ?target-container-2 powdered-white-sugar 120 g)
        
    ;; "240 grams flour, sifted"
    (fetch-and-proportion ?proportioned-flour ?ks-with-flour ?ks-with-powdered-sugar ?target-container-3 all-purpose-flour 240 g)
    (sift ?sifted-flour ?ks-with-sifted-flour ?ks-with-flour ?target-container-4 ?proportioned-flour ?sifting-tool)
    
    ;; "120 g walnuts, ground"
    (fetch-and-proportion ?proportioned-walnut ?ks-with-walnut ?ks-with-sifted-flour ?target-container-5 walnut 120 g)
    (grind ?ground-walnut ?ks-with-ground-walnut ?ks-with-walnut ?proportioned-walnut ?grinding-tool)
    
    ;; "1 teaspoon vanilla"
    (fetch-and-proportion ?proportioned-vanilla ?ks-with-vanilla ?ks-with-ground-walnut ?target-container-7 vanilla 1 teaspoon)

    ;; "60 grams additional powdered sugar for rolling"
    (fetch-and-proportion ?additional-powdered-sugar ?ks-with-additional-sugar ?ks-with-vanilla ?target-container-8 powdered-white-sugar 60 g)

    ;; "Combine all ingredients."
    (transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-additional-sugar ?empty-container ?warm-butter ?quantity-a ?unit-a)
    (transfer-contents ?output-container-b ?rest-b ?output-ks-b ?output-ks-a ?output-container-a ?proportioned-powdered-sugar ?quantity-b ?unit-b)
    (transfer-contents ?output-container-c ?rest-c ?output-ks-c ?output-ks-b ?output-container-b ?sifted-flour ?quantity-c ?unit-c) 
    (transfer-contents ?output-container-d ?rest-d ?output-ks-d ?output-ks-c ?output-container-c ?ground-walnut ?quantity-d ?unit-d)
    (transfer-contents ?output-container-e ?rest-e ?output-ks-e ?output-ks-d ?output-container-d ?proportioned-vanilla ?quantity-e ?unit-e)
    (mix ?dough ?ks-with-dough ?output-ks-e ?output-container-e ?mixing-tool)

    ;; "Form into balls of around 25g."
    (portion-and-arrange ?portioned-dough ?ks-with-dough-portions ?ks-with-dough ?dough 25 g ?pattern ?countertop)
    (shape ?bakeable-balls ?ks-with-balls ?ks-with-dough-portions ?portioned-dough ball-shape)

    ;; "Bake on cookie sheet at 180°C for about 10 minutes."

    ;; IMPLICIT
    (fetch ?cookie-sheet ?ks-with-cookie-sheet ?ks-with-balls cookie-sheet 1)
    (transfer-items ?tray-with-balls ?ks-with-balls-tray ?ks-with-cookie-sheet ?bakeable-balls ?default-pattern ?cookie-sheet)

    (bake ?baked-balls ?ks-with-baked-balls ?ks-with-balls-tray ?tray-with-balls ?oven 10 minute 180 degrees-celsius)

    ;; "Roll in additional powdered sugar while still warm."
    (dip ?mexican-wedding-cookies ?ks-with-mexican-wedding-cookies ?ks-with-baked-balls ?baked-balls ?additional-powdered-sugar)))

;; ======================
;; Append bindings to the recipe
;; ======================

(defparameter *extended-recipe*
  (append-meaning-and-irl-bindings *mexican-wedding-cookies-recipe* nil))

;; ======================
;; Evaluate the recipe
;; ======================

;(activate-monitor trace-irl)

;(clear-output)

;(evaluate-irl-program *extended-recipe* nil)

;; ======================
;; Visualise the recipe
;; ======================

;(draw-recipe *mexican-wedding-cookies-recipe*)
;(draw-recipe *extended-recipe*)
