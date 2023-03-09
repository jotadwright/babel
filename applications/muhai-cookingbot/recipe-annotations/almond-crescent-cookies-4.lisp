(ql:quickload :muhai-cookingbot)

(in-package :muhai-cookingbot)

;; The 'trace-irl' monitor will make sure that
;; the IRL evaluation process is shown on the web
;; interface (which can be found at localhost:8000).
;; We need to activate it:
;(activate-monitor trace-irl)

;; ###################################################################
;; Almond Crescent Cookies recipe
;; https://www.cooks.com/recipe/3144r3hg/almond-crescent-cookies.html
;; ###################################################################

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
                                                                                                                              :value 500)))))
                                        (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'almond :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 500)))))
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

                                   (make-instance 'knife) (make-instance 'knife) (make-instance 'knife)

                                   ;; baking equipment
                                   (make-instance 'cookie-sheet)
                                   (make-instance 'baking-tray)
                                   (make-instance 'baking-paper))))))

;; 'make-html' makes an HTML representation of the kitchen state
;; and 'add-element' transfers that to the web interface
;(add-element (make-html *initial-kitchen-state* :expand-initially t))

(defparameter *almond-cookies-recipe*
  '((get-kitchen ?kitchen)
    
    ;; "230 grams butter"
    (fetch-and-proportion ?proportioned-butter ?ks-with-butter ?kitchen ?target-container-1 butter 230 g)
    
    ;; "2 tsp vanilla"
    (fetch-and-proportion ?proportioned-vanilla ?ks-with-vanilla ?ks-with-butter ?target-container-2 vanilla 2 teaspoon)
    
    ;; "100 g confectioners' sugar"
    (fetch-and-proportion ?proportioned-powdered-sugar ?ks-with-sugar ?ks-with-vanilla ?target-container-3 powdered-white-sugar 100 g)
    
    ;; "1 tbsp water"
    (fetch-and-proportion ?proportioned-water ?ks-with-water ?ks-with-sugar ?target-container-4 water 1 tablespoon)
    
    ;; "240 grams flour"
    (fetch-and-proportion ?proportioned-flour ?ks-with-flour ?ks-with-water ?target-container-5 all-purpose-flour 240 g)
    
    ;; "140 grams chopped almonds"
    (fetch-and-proportion ?proportioned-almonds ?ks-with-almonds ?ks-with-flour ?target-container-6 almond 140 g)
    (cut ?chopped-almonds ?ks-with-chopped-almonds ?ks-with-almonds ?proportioned-almonds chopped ?knife)
    
    ;; "Cream butter, vanilla, sugar"
    (transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-chopped-almonds ?empty-container-a ?proportioned-butter ?quantity-a ?unit-a)
    (transfer-contents ?output-container-b ?rest-b ?output-ks-b ?output-ks-a ?output-container-a ?proportioned-vanilla ?quantity-b ?unit-b)
    (transfer-contents ?output-container-c ?rest-c ?output-ks-c ?output-ks-b ?output-container-b ?proportioned-powdered-sugar ?quantity-c ?unit-c)
    (beat ?beaten-mixture ?ks-with-beaten-mixture ?output-ks-c ?output-container-c ?mixing-tool)

    ;; " and add flour and water."
    (transfer-contents ?output-container-d ?rest-d ?output-ks-d ?ks-with-beaten-mixture ?beaten-mixture ?proportioned-flour ?quantity-d ?unit-d)
    (transfer-contents ?output-container-e ?rest-e ?output-ks-e ?output-ks-d ?output-container-d ?proportioned-water ?quantity-e ?unit-e)

    ;; "Mix thoroughly, stir in almonds."
    (mix ?dough ?ks-with-dough ?output-ks-e ?output-container-e ?mixing-tool) ; reuse the mixing tool
    (transfer-contents ?output-container-f ?rest-f ?output-ks-f ?ks-with-dough ?dough ?chopped-almonds ?quantity-f ?unit-f)
    (mingle ?dough-with-almonds ?ks-with-almonds-dough ?output-ks-f ?output-container-f ?mixing-tool) ; reuse the mixing tool

    ;; "Form crescents of around 30 grams."
    (portion-and-arrange ?portioned-dough ?ks-with-dough-portions ?ks-with-almonds-dough ?dough-with-almonds 30 g ?pattern ?countertop)
    (shape ?bakeable-crescents ?ks-with-crescents ?ks-with-dough-portions ?portioned-dough crescent-shape)

    ;; "Bake in greased cookie sheet, in slow oven, about 150 degrees, for 20 minutes."
    (fetch ?cookie-sheet ?ks-with-cookie-sheet ?ks-with-crescents cookie-sheet 1) ;; IMPLICIT
    (grease ?greased-sheet ?ks-with-greased-sheet ?ks-with-cookie-sheet ?cookie-sheet ?grease) ;; IMPLICIT
    (transfer-items ?tray-with-crescents ?ks-with-crescents-tray ?ks-with-greased-sheet ?bakeable-crescents ?default-pattern ?greased-sheet) ;; IMPLICIT

    (bake ?baked-crescents ?ks-with-baked-crescents ?ks-with-crescents-tray ?tray-with-crescents ?oven 20 minute 150 degrees-celsius)))

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
