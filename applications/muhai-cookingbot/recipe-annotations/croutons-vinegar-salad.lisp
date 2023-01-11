(ql:quickload :muhai-cookingbot)

(in-package :muhai-cookingbot)

;; The 'trace-irl' monitor will make sure that
;; the IRL evaluation process is shown on the web
;; interface (which can be found at localhost:8000).
;; We need to activate it:
;(activate-monitor trace-irl)

;; #####################################################################################
;; Tossed Salad With Homemade Croutons and Oil and Vinegar Dressing Recipe
;; https://www.thespruceeats.com/basic-tossed-salad-with-homemade-croutons-3959138
;; #####################################################################################

;; Defining the initial kitchen state
(defparameter *initial-kitchen-state* 
  (make-instance 
   'kitchen-state
   :contents
   (list (make-instance 'fridge
                        :contents (list
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'lime-juice :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'l)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 0.5)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'cherry-tomato :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'piece)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 30)))))))
         (make-instance 'pantry
                        :contents (list  (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'white-sugar :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 1000)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'garlic-powder :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 500)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'red-onion :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'piece)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 10)))))                                          
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'extra-virgin-olive-oil :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'ml)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 500)))))

                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'red-wine-vinegar :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'l)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 0.5)))))  
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'carrot :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'piece)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 5)))))

                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'radish :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'piece)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 10)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'ground-black-pepper :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 500)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'green-onion :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'piece)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 5)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'coarse-salt :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 50)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'dried-dill-weed :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 500)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'white-bread-slice :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'piece)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 12)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'cucumber :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'piece)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 2)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'black-olive :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'piece)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 20)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'romaine-lettuce :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'piece)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 2)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'mixed-greens :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 500)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'coarse-salt :amount
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
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'small-bowl) (make-instance 'small-bowl) (make-instance 'small-bowl)

                                   ;; bowl-lids
                                   (make-instance 'medium-bowl-lid) (make-instance 'medium-bowl-lid) (make-instance 'medium-bowl-lid)
                                   (make-instance 'large-bowl-lid) (make-instance 'large-bowl-lid) (make-instance 'large-bowl-lid)

                                   ;; jars
                                   (make-instance 'jar) (make-instance 'jar) (make-instance 'jar)

                                   ;; jar-lids
                                   (make-instance 'jar-lid) (make-instance 'jar-lid) (make-instance 'jar-lid)

                                   ;; wrapping
                                   (make-instance 'plastic-wrap)

                                   ;; tools
                                   (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                   (make-instance 'wooden-spoon) (make-instance 'wooden-spoon) (make-instance 'wooden-spoon)
                                   (make-instance 'knife) (make-instance 'knife) (make-instance 'knife)

                                   ;; baking equipment
                                   (make-instance 'cookie-sheet)
                                   (make-instance 'baking-tray)
                                   (make-instance 'baking-paper))))))

;; 'make-html' makes an HTML representation of the kitchen state
;; and 'add-element' transfers that to the web interface
;(add-element (make-html *initial-kitchen-state* :expand-initially t))

(defparameter *croutons-vinegar-salad-recipe* 
  '((get-kitchen ?kitchen)
    
    ;; "4 slices white bread"
    (fetch-and-proportion ?proportioned-bread ?ks-with-bread ?kitchen ?target-container-1 white-bread-slice 1 piece)

    ;; "2 tablespoons extra-virgin olive oil"
    (fetch-and-proportion ?proportioned-olive-oil-tbsp ?ks-with-olive-oil-tbsp ?ks-with-bread ?target-container-2 extra-virgin-olive-oil 2 tablespoon)

    ;; "1/4 teaspoon dried dill"
    (fetch-and-proportion ?proportioned-dill ?ks-with-dried-dill ?ks-with-olive-oil-tbsp ?target-container-3 dried-dill-weed 0.25 teaspoon)

    ;; "1/4 teaspoon garlic powder"
    (fetch-and-proportion ?proportioned-garlic ?ks-with-garlic ?ks-with-dried-dill ?target-container-4 garlic-powder 0.25 teaspoon)

    ;; "225 grams romaine lettuce leaves"
    (fetch-and-proportion ?proportioned-lettuce ?ks-with-lettuce ?ks-with-garlic ?target-container-6 romaine-lettuce 225 g)

    ;; "115 grams mixed greens"
    (fetch-and-proportion ?proportioned-greens ?ks-with-greens ?ks-with-lettuce ?target-container-7 mixed-greens 115 g)

    ;; "115 grams cherry tomatoes, sliced"
    (fetch-and-proportion ?proportioned-cherry-tomato ?ks-with-cherry-tomato ?ks-with-greens ?target-container-8 cherry-tomato 115 g)
    (cut ?sliced-tomato ?ks-with-sliced-tomato ?ks-with-cherry-tomato ?proportioned-cherry-tomato slices ?knife)

    ;; "5 radishes, sliced"
    (fetch-and-proportion ?proportioned-radish ?ks-with-radish ?ks-with-sliced-tomato ?target-container-9 radish 5 piece)
    (cut ?sliced-radish ?ks-with-sliced-radish ?ks-with-radish ?proportioned-radish slices ?knife)

    ;; "1 red onion, thinly sliced"
    (fetch-and-proportion ?proportioned-onion ?ks-with-onion ?ks-with-sliced-radish ?target-container-10 red-onion 1 piece)
    (cut ?sliced-onion ?ks-with-sliced-onion ?ks-with-onion ?proportioned-onion fine-slices ?knife)

    ;; "1 carrot, shredded"
    (fetch-and-proportion ?proportioned-carrot ?ks-with-carrot ?ks-with-sliced-onion ?target-container-11 carrot 1 piece)
    (cut ?shredded-carrot ?ks-with-shredded-carrot ?ks-with-carrot ?proportioned-carrot shredded ?knife)

    ;; "250 ml extra-virgin olive oil"
    (fetch-and-proportion ?proportioned-olive-oil-ml ?ks-with-olive-oil-ml ?ks-with-shredded-carrot ?target-container-14 extra-virgin-olive-oil 250 ml)

    ;; "6 tablespoons red wine vinegar"
    (fetch-and-proportion ?proportioned-vinegar ?ks-with-vinegar ?ks-with-olive-oil-ml ?target-container-15 red-wine-vinegar 6 tablespoon)

    ;; "1 1/2 teaspoons sugar"
    (fetch-and-proportion ?proportioned-sugar ?ks-with-sugar ?ks-with-vinegar ?target-container-16 white-sugar 1.5 teaspoon)

    ;; "Gather the ingredients."
    ; (this already happened)

    ;; "Heat the oven to 190 C."
    (preheat-oven ?heated-oven ?ks-with-heated-oven ?ks-with-sugar ?oven 190 degrees-celsius)

    ;; "Cut the bread into 2-cm cubes and put in a large bowl."
    (cut ?bread-cubes ?ks-with-bread-cubes ?ks-with-heated-oven ?proportioned-bread two-cm-cubes ?knife)
    (fetch ?large-bowl-1 ?ks-with-large-bowl-1 ?ks-with-bread-cubes large-bowl 1) ;; IMPLICIT
    (transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-large-bowl-1 ?large-bowl-1 ?bread-cubes ?quantity-a ?unit-a)

    ;; "Drizzle with 2 tablespoons extra-virgin olive oil and sprinkle with the dill; toss to coat."
    (transfer-contents ?output-container-b ?rest-b ?output-ks-b ?output-ks-a ?output-container-a ?proportioned-olive-oil-tbsp ?quantity-b ?unit-b)
    (transfer-contents ?output-container-c ?rest-c ?output-ks-c ?output-ks-b ?output-container-b ?proportioned-dill ?quantity-c ?unit-c)
    (mingle ?sprinkled-bread ?ks-with-sprinkled-bread ?output-ks-c ?output-container-c ?mingling-tool)

    ;; "Spread the bread cubes out on a rimmed baking sheet and sprinkle lightly with the garlic powder."
    (fetch ?baking-tray ?ks-with-baking-tray ?ks-with-sprinkled-bread baking-tray 1) ;; IMPLICIT
    (transfer-contents ?output-container-d ?rest-d ?output-ks-d ?ks-with-baking-tray ?baking-tray ?sprinkled-bread ?quantity-d ?unit-d)

    (sprinkle ?bread-with-garlic ?ks-with-sprinkled-garlic ?output-ks-d ?output-container-d ?proportioned-garlic)

    ;; "Bake for about 12 minutes."
    (bake ?baked-croutons ?ks-with-baked-croutons ?ks-with-sprinkled-garlic ?bread-with-garlic ?oven 12 minute ?baking-temp-qty ?baking-temp-unit)

    ;; "In a large bowl, combine the romaine lettuce, mixed greens, tomatoes, radishes, onion, and carrot. Toss well."
    (fetch ?large-bowl-2 ?ks-with-large-bowl-2 ?ks-with-baked-croutons large-bowl 1) ;; IMPLICIT

    (transfer-contents ?output-container-e ?rest-e ?output-ks-e ?ks-with-large-bowl-2 ?large-bowl-2 ?proportioned-lettuce ?quantity-e ?unit-e)
    (transfer-contents ?output-container-f ?rest-f ?output-ks-f ?output-ks-e ?output-container-e ?proportioned-greens ?quantity-f ?unit-f)
    (transfer-contents ?output-container-g ?rest-g ?output-ks-g ?output-ks-f ?output-container-f ?sliced-tomato ?quantity-g ?unit-g)
    (transfer-contents ?output-container-h ?rest-h ?output-ks-h ?output-ks-g ?output-container-g ?sliced-radish ?quantity-h ?unit-h)
    (transfer-contents ?output-container-i ?rest-i ?output-ks-i ?output-ks-h ?output-container-h ?sliced-onion ?quantity-i ?unit-i)
    (transfer-contents ?output-container-j ?rest-j ?output-ks-j ?output-ks-i ?output-container-i ?shredded-carrot ?quantity-j ?unit-j)
    (mingle ?veggies-mix ?ks-with-veggies-mix ?output-ks-j ?output-container-j ?mingling-tool)

    ;; "Cover and refrigerate until serving time."
    (cover ?covered-salad ?ks-with-covered-salad ?ks-with-veggies-mix ?veggies-mix ?plastic-wrap)
    (refrigerate ?cooled-salad ?ks-with-cooled-salad ?ks-with-covered-salad ?covered-salad ?fridge ?cooling-quantity ?cooling-unit)

    ;; "In a jar with a screw-on lid, combine the extra-virgin olive oil, red wine vinegar, and sugar."
    (fetch ?jar ?ks-with-fetched-jar ?ks-with-cooled-salad jar 1) ;; IMPLICIT
    (fetch ?jar-lid ?ks-with-fetched-jar-lid ?ks-with-fetched-jar jar-lid 1) ;; IMPLICIT

    (transfer-contents ?output-container-m ?rest-m ?output-ks-m ?ks-with-fetched-jar-lid ?jar ?proportioned-olive-oil-ml ?quantity-m ?unit-m)
    (transfer-contents ?output-container-n ?rest-n ?output-ks-n ?output-ks-m ?output-container-m ?proportioned-vinegar ?quantity-n ?unit-n)
    (transfer-contents ?output-container-o ?rest-o ?output-ks-o ?output-ks-n ?output-container-n ?proportioned-sugar ?quantity-o ?unit-o)

    (cover ?covered-jar ?ks-with-covered-jar ?output-ks-o ?output-container-o ?jar-lid)

    ;; "Shake to blend. Keep refrigerated until serving time."
    (shake ?salad-dressing ?ks-with-dressing ?ks-with-covered-jar ?covered-jar)
    (refrigerate ?cooled-dressing ?ks-with-cooled-dressing ?ks-with-dressing ?salad-dressing ?fridge-2 ?cooling-quantity-2 ?cooling-unit-2)

    ;; "To serve the salad, toss with dressing and top with croutons."
     ;; IMPLICIT: the bowl and the jar are still covered and should be uncovered first
    (uncover ?uncovered-salad ?used-plastic-wrap ?ks-with-uncovered-salad ?ks-with-cooled-dressing ?cooled-salad)
    (uncover ?uncovered-jar ?used-jar-lid ?ks-with-uncovered-jar ?ks-with-uncovered-salad ?covered-jar)
    
    (transfer-contents ?output-container-r ?rest-r ?output-ks-r ?ks-with-uncovered-jar ?uncovered-salad ?uncovered-jar ?quantity-r ?unit-r)
    (mingle ?dressed-salad ?ks-with-dressed-salad ?output-ks-r ?output-container-r ?mingling-tool)
    
    (sprinkle ?croutons-vinegar-salad ?ks-with-croutons-vinegar-salad ?ks-with-dressed-salad ?dressed-salad ?baked-croutons)))

    ;; "Serve and enjoy!"
    ; (does not require an action)

;; ======================
;; Append bindings to the recipe
;; ======================

(defparameter *extended-recipe*
  (append-meaning-and-irl-bindings *croutons-vinegar-salad-recipe* nil))

;; ======================
;; Evaluate the recipe
;; ======================

;(activate-monitor trace-irl)

;(clear-output)

;(evaluate-irl-program *extended-recipe* nil)

;; ======================
;; Visualise the recipe
;; ======================

;(draw-recipe *croutons-vinegar-salad-recipe*)
;(draw-recipe *extended-recipe*)