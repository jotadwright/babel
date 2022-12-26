(ql:quickload :muhai-cookingbot)

(in-package :muhai-cookingbot)

;; The 'trace-irl' monitor will make sure that
;; the IRL evaluation process is shown on the web
;; interface (which can be found at localhost:8000).
;; We need to activate it:
;(activate-monitor trace-irl)

;; ##################################################################
;; Almond Crescent Cookies recipe
;; https://www.allrecipes.com/recipe/245336/almond-crescent-cookies/
;; ##################################################################

;; Defining the initial kitchen state
(defparameter *initial-kitchen-state* 
  (make-instance 
   'kitchen-state
   :contents
   (list (make-instance 'fridge
                        :contents (list (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'salted-butter
                                                                                      :temperature
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'degrees-celsius)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 5))
                                                                                      :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 5000)))))))
         (make-instance 'pantry
                        :contents (list (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'salt :amount
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
                                                                                                                              :value 1000)))))
                                        (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'almond-extract :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 1000)))))
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
                                                                                                                              :value 1000)))))))
         (make-instance 'kitchen-cabinet
                        :contents (list
                                   ;; bowls
                                   (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)
                                   (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)
                                   (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)
                                   (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)
                                   (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)
                                   (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)
                                   (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)
                                   (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)
                                   
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)

                                   ;; tools
                                   (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                   (make-instance 'mixer) (make-instance 'mixer) (make-instance 'mixer)
                                   (make-instance 'sift) (make-instance 'sift) (make-instance 'sift)
                                   (make-instance 'sift) (make-instance 'sift) (make-instance 'sift)
                                   (make-instance 'knife) (make-instance 'knife)   (make-instance 'knife)
                                   (make-instance 'wire-rack)
                                   
                                   ;; baking equipment
                                   (make-instance 'baking-tray)
                                   (make-instance 'baking-paper))))))

;; 'make-html' makes an HTML representation of the kitchen state
;; and 'add-element' transfers that to the web interface
;(add-element (make-html *initial-kitchen-state* :expand-initially t))

(defparameter *almond-cookies-recipe*
  '((get-kitchen ?kitchen)
    
    ;; "120 grams salted butter, at room temperature"
    (fetch-and-proportion ?proportioned-butter ?ks-with-butter ?kitchen ?target-container-1 salted-butter 120 g)
    (bring-to-temperature ?warm-butter ?ks-with-warm-butter ?ks-with-butter ?proportioned-butter ?room-temp-qty ?room-temp-unit)
    
     ;; "40 grams confectioners' sugar, plus 30 grams extra for dusting"
    (fetch-and-proportion ?proportioned-sugar ?ks-with-sugar ?ks-with-warm-butter ?target-container-2 powdered-white-sugar 40 g)
    (fetch-and-proportion ?proportioned-dusting-sugar ?ks-with-dusting-sugar ?ks-with-sugar ?target-container-3 powdered-white-sugar 30 g)
    
    ;; "1 teaspoon vanilla extract"
    (fetch-and-proportion ?proportioned-vanilla ?ks-with-vanilla ?ks-with-dusting-sugar ?target-container-4 vanilla-extract 1 teaspoon)
    
    ;; "1 teaspoon almond extract"
    (fetch-and-proportion ?proportioned-almond-extract ?ks-with-almond ?ks-with-vanilla ?target-container-5 almond-extract 1 teaspoon)

    ;; "1/8 teaspoon salt"
    (fetch-and-proportion ?proportioned-salt ?ks-with-salt ?ks-with-almond ?target-container-6 salt 0.125 teaspoon)
        
    ;; "90 grams all-purpose flour, sifted"
    (fetch-and-proportion ?proportioned-flour ?ks-with-flour ?ks-with-salt ?target-container-7 all-purpose-flour 90 g)
    (sift ?sifted-flour ?ks-with-sifted-flour ?ks-with-flour ?large-bowl ?proportioned-flour ?sifting-tool)

    ;; "2 tablespoons all-purpose flour, sifted"
    (fetch-and-proportion ?proportioned-tbsp-flour ?ks-with-tbsp-flour ?ks-with-sifted-flour ?target-container-8 all-purpose-flour 2 tablespoon)
    (sift ?sifted-tbsp-flour ?ks-with-sifted-tbsp-flour ?ks-with-tbsp-flour ?large-bowl-2 ?proportioned-tbsp-flour ?sifting-tool) ;; reuse sifting tool
    
    ;; "70 grams almonds, finely chopped"
    (fetch-and-proportion ?proportioned-almonds ?ks-with-almonds ?ks-with-sifted-tbsp-flour ?target-container-9 almond 70 g)
    (cut ?finely-chopped-almonds ?ks-with-finely-chopped-almonds ?ks-with-almonds ?proportioned-almonds fine-slices ?knife)
    
    ;; "Preheat oven to 165 degrees C."
    (preheat-oven ?preheated-oven ?ks-with-preheated-oven ?ks-with-finely-chopped-almonds ?oven 165 degrees-celsius) 

    ;; "Beat butter and confectioners' sugar in a bowl using an electric mixer until smooth and creamy."
    (transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-preheated-oven ?empty-container-a ?warm-butter ?quantity-a ?unit-a)
    (transfer-contents ?output-container-b ?rest-b ?output-ks-b ?output-ks-a ?output-container-a ?proportioned-sugar ?quantity-b ?unit-b)

    (fetch ?mixer ?ks-with-mixer ?output-ks-b mixer 1) ;; IMPLICIT
    (beat ?beaten-mixture ?ks-with-beaten-mixture ?ks-with-mixer ?output-container-b ?mixer)

    ;; "Add vanilla extract, almond extract, and salt; mix briefly to incorporate."
    (transfer-contents ?output-container-c ?rest-c ?output-ks-c ?ks-with-beaten-mixture ?beaten-mixture ?proportioned-vanilla ?quantity-c ?unit-c)
    (transfer-contents ?output-container-d ?rest-d ?output-ks-d ?output-ks-c ?output-container-c ?proportioned-almond-extract ?quantity-d ?unit-d)
    (transfer-contents ?output-container-e ?rest-e ?output-ks-e ?output-ks-d ?output-container-d ?proportioned-salt ?quantity-e ?unit-e)
    (mix ?intermediate-mixture ?ks-with-intermediate-mixture ?output-ks-e ?output-container-e ?mixing-tool)

    ;; "Gradually stir 90 grams plus 2 tablespoons flour into the creamed butter, add almonds, and mix until dough is just combined."
    (transfer-contents ?output-container-f ?rest-f ?output-ks-f ?ks-with-intermediate-mixture ?intermediate-mixture ?sifted-flour ?quantity-f ?unit-f)
    (transfer-contents ?output-container-g ?rest-g ?output-ks-g ?output-ks-f ?output-container-f ?sifted-tbsp-flour ?quantity-g ?unit-g)
    (transfer-contents ?output-container-h ?rest-h ?output-ks-h ?output-ks-g ?output-container-g ?finely-chopped-almonds ?quantity-h ?unit-h)
    (mix ?dough ?ks-with-dough ?output-ks-h ?output-container-h ?mixing-tool) ; reuse the same mixing tool

    ;; "Shape dough into tiny crescents of 25 grams; place on an ungreased baking sheet about 5 cm apart."
    (portion-and-arrange ?portioned-dough ?ks-with-dough-portions ?ks-with-dough ?dough 25 g ?pattern ?countertop)
    (shape ?bakeable-crescents ?ks-with-crescents ?ks-with-dough-portions ?portioned-dough crescent-shape)
    
    (fetch ?baking-tray ?ks-with-baking-tray ?ks-with-crescents baking-tray 1)
    (transfer-items ?tray-with-crescents ?ks-with-crescents-tray ?ks-with-baking-tray ?bakeable-crescents 5-cm-apart ?baking-tray)

    ;; "Bake cookies in preheated oven for about 15 minutes."
    (bake ?baked-crescents ?ks-with-baked-crescents ?ks-with-crescents-tray ?tray-with-crescents ?preheated-oven 15 minute ?bake-temp-qty ?bake-temp-unit)

    ;; "Cool on the baking sheet for 5 minutes before transferring to a wire rack to cool completely."
    (cool-for-time ?cooling-cookies ?ks-with-cooling-cookies ?ks-with-baked-crescents ?baked-crescents 5 minute)
    
    (fetch ?wire-rack ?ks-with-wire-rack ?ks-with-cooling-cookies wire-rack 1) ;; IMPLICIT
    (transfer-items ?cookies-on-wire-rack ?ks-with-cookies-on-wire-rack ?ks-with-wire-rack ?cooling-cookies ?default-pattern ?wire-rack)
    (bring-to-temperature ?cooled-cookies ?ks-with-cooled-cookies ?ks-with-cookies-on-wire-rack ?cookies-on-wire-rack ?room-temp-qty ?room-temp-unit)
    
    ;; "Sprinkle cookies with sifted confectioners' sugar when cooled."
    (sift ?sifted-sugar ?ks-with-sifted-sugar ?ks-with-cooled-cookies ?large-bowl ?proportioned-dusting-sugar ?sifting-tool) ;; IMPLICIT
    (sprinkle ?almond-crescent-cookies ?ks-with-almond-crescent-cookies ?ks-with-sifted-sugar ?cooled-cookies ?sifted-sugar)))

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
