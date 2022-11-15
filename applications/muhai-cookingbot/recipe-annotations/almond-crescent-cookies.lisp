(ql:quickload :muhai-cookingbot)

(in-package :muhai-cookingbot)

;; The 'trace-irl' monitor will make sure that
;; the IRL evaluation process is shown on the web
;; interface (which can be found at localhost:8000).
;; We need to activate it:
;(activate-monitor trace-irl)

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
                                                       :contents (list (make-instance 'almond-extract :amount
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
                                                       :contents (list (make-instance 'almond-flour :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 1000)))))
                                        (make-instance 'medium-bowl
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

  '((get-kitchen ?kitchen-state-1785)
    
    ;; "230 grams butter, room temperature"
    (fetch-and-proportion ?ingredient-out-891 ?kitchen-state-out-5343 ?kitchen-state-1785 ?target-container-891 butter 230 g)
    (bring-to-temperature ?ingredient-at-room-temperature-892 ?output-kitchen-state-6240 ?kitchen-state-out-5343 ?ingredient-out-891 18 degrees-celsius)
     ;; "120 grams sugar"
    (fetch-and-proportion ?ingredient-out-898 ?kitchen-state-out-5385 ?output-kitchen-state-6240 ?target-container-898 white-sugar 120 g)
    ;; "1 teaspoon vanilla extract"
    (fetch-and-proportion ?ingredient-out-907 ?kitchen-state-out-5442 ?kitchen-state-out-5385 ?target-container-907 vanilla-extract 1 teaspoon)
    ;; "1 teaspoon almond extract"
    (fetch-and-proportion ?ingredient-out-914 ?kitchen-state-out-5483 ?kitchen-state-out-5442 ?target-container-914 almond-extract 1 teaspoon)
    ;; "2 1/2 cups flour"
    (fetch-and-proportion ?ingredient-out-920 ?kitchen-state-out-5518 ?kitchen-state-out-5483 ?target-container-920 all-purpose-flour 340 g)
    ;; "1 cup almond flour"
    (fetch-and-proportion ?ingredient-out-926 ?kitchen-state-out-5554 ?kitchen-state-out-5518 ?target-container-926 almond-flour 120 g)
    ;; "1/4 cup powdered sugar"
    (fetch-and-proportion ?ingredient-out-932 ?kitchen-state-out-5590 ?kitchen-state-out-5554 ?target-container-932 powdered-white-sugar 30 g)
    
    ;; "Beat the butter and the sugar together until light and fluffy"
    (transfer-contents ?output-container-?x-940 ?rest-x-1879 ?output-kitchen-state-x-940 ?kitchen-state-out-5590 ?empty-container-940 ?ingredient-at-room-temperature-892 ?quantity-x-1879 ?unit-x-1879)
    (transfer-contents ?input-container-4672 ?rest-y-1879 ?input-kitchen-state-5606 ?output-kitchen-state-x-940 ?output-container-?x-940 ?ingredient-out-898 ?quantity-y-1879 ?unit-y-1879)
    (beat ?output-container-5607 ?output-kitchen-state-6541 ?input-kitchen-state-5606 ?input-container-4672 ?tool-2803)

    ;; "Add the vanilla and almond extracts and mix"
    (transfer-contents ?output-container-after-adding-x-981 ?rest-x-1962 ?intermediate-ks-981 ?output-kitchen-state-6541 ?output-container-5607 ?ingredient-out-914 ?quantity-x-1962 ?unit-x-1962) ;almond extract
    (transfer-contents ?input-container-4711 ?rest-y-1962 ?input-kitchen-state-5653 ?intermediate-ks-981 ?output-container-after-adding-x-981 ?ingredient-out-907 ?quantity-y-1962 ?unit-y-1962)
    (mix ?output-container-5653 ?output-kitchen-state-6595 ?input-kitchen-state-5653 ?input-container-4711 ?tool-2827)

    ;; "Add the flour and almond flour"
    (transfer-contents ?output-container-after-adding-x-992 ?rest-x-1983 ?intermediate-ks-992 ?output-kitchen-state-6595 ?output-container-5653 ?ingredient-out-926 ?quantity-x-1983 ?unit-x-1983) ;almond flour
    (transfer-contents ?output-container-5948 ?rest-y-1983 ?output-kitchen-state-6939 ?intermediate-ks-992 ?output-container-after-adding-x-992 ?ingredient-out-920 ?quantity-y-1983 ?unit-y-1983)

    ;; "Mix thoroughly"
    (mix ?output-container-5963 ?output-kitchen-state-6956 ?output-kitchen-state-6939 ?output-container-5948 ?tool-2981)

    ;; "Take generous tablespoons of the dough and roll it into a small ball , about an inch in diameter , and then shape it into a crescent shape"
    (portion-and-arrange ?portioned-dough-998 ?kitchen-state-with-portions-on-tray-998 ?output-kitchen-state-6956 ?output-container-5963 25 g ?pattern-998 ?countertop)
    (shape ?shaped-bakeables-1998 ?kitchen-state-out-5992 ?kitchen-state-with-portions-on-tray-998 ?portioned-dough-998 ball-shape)
    (shape ?shaped-bakeables-1999 ?kitchen-state-out-5999 ?kitchen-state-out-5992 ?shaped-bakeables-1998 crescent-shape)
    
    ;; "Place onto a parchment paper lined baking sheet"
    (fetch ?baking-tray ?kitchen-state-with-baking-tray ?kitchen-state-out-5999 baking-tray 1)
    (fetch ?baking-paper ?kitchen-state-with-baking-paper ?kitchen-state-with-baking-tray baking-paper 1)
    (line ?lined-baking-tray-2005 ?kitchen-state-out-6014 ?kitchen-state-with-baking-paper ?baking-tray ?baking-paper)
    (transfer-items ?things-placed-1004 ?kitchen-out-1004 ?kitchen-state-out-6014 ?shaped-bakeables-1999 ?default-pattern ?lined-baking-tray-2005)

    ;; "Bake at 175 C for 15 - 20 minutes"
    (bake ?thing-baked-1016 ?kitchen-state-out-6096 ?kitchen-out-1004 ?things-placed-1004 ?oven 15 minute 175 degrees-celsius)

    ;; "Dust with powdered sugar"
    (sprinkle ?sprinkled-object-1019 ?kitchen-state-out-6112 ?kitchen-state-out-6096 ?thing-baked-1016 ?ingredient-out-932)))

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
