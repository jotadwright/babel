(ql:quickload :muhai-cookingbot)

(in-package :muhai-cookingbot)

;; The 'trace-irl' monitor will make sure that
;; the IRL evaluation process is shown on the web
;; interface (which can be found at localhost:8000).
;; We need to activate it:
;(activate-monitor trace-irl)

;; ############################################################
;; Greek Salad With Lettuce and Lemon Garlic Dressing
;; https://www.thespruceeats.com/classic-greek-salad-3964419
;; ############################################################

;; Defining the initial kitchen state
(defparameter *initial-kitchen-state* 
  (make-instance 
   'kitchen-state
   :contents
   (list (make-instance 'fridge
                        :contents (list  (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'onion :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'piece)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 10)))))))
         (make-instance 'pantry
                        :contents (list  (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'sweet-potato :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 500)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'red-bell-pepper :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 500)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'feta-cheese :amount
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
                                                                                                                               :value 5)))))                                          
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'extra-virgin-olive-oil :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 50)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'salt :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 500)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'ground-black-pepper :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 500)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'garlic :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'piece)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 1)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'lemon-juice :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 500)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'radish :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'piece)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 10)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'tomato :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'piece)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 12)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'black-olive :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'piece)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 100)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'fresh-cilantro :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 50)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'fresh-oregano :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 50)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'romaine-lettuce :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'piece)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 2)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'cucumber :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'piece)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 2)))))))
         (make-instance 'kitchen-cabinet
                        :contents (list
                                   ;; bowls
                                   (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)
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

(defparameter *classic-greek-salad-recipe* 
  '((get-kitchen ?kitchen)
    
    ;; "1 cucumber"
    (fetch-and-proportion ?proportioned-cucumber ?ks-with-cucumber ?kitchen ?target-container-1 cucumber 1 piece)
    
    ;; "1 tomato"
    (fetch-and-proportion ?proportioned-tomato ?ks-with-tomato ?ks-with-cucumber ?target-container-2 tomato 1 piece)

    ;; "3 radishes"
    (fetch-and-proportion ?proportioned-radish ?ks-with-radish ?ks-with-tomato ?target-container-3 radish 3 piece)
        
    ;; "1 red onion"
    (fetch-and-proportion ?proportioned-onion ?ks-with-onion ?ks-with-radish ?target-container-4 red-onion 1 piece)

    ;; "1 red bell pepper"
    (fetch-and-proportion ?proportioned-bell-pepper ?ks-with-bell-pepper ?ks-with-onion ?target-container-5 red-bell-pepper 1 piece)

    ;; "115 grams feta cheese (cubed)"
    (fetch-and-proportion ?proportioned-cheese ?ks-with-cheese ?ks-with-bell-pepper ?target-container-6 feta-cheese 115 g)
    (cut ?cubed-cheese ?ks-with-cubed-cheese ?ks-with-cheese ?proportioned-cheese cubes ?knife)

    ;; "90 grams black olives (pitted)"
    (fetch-and-proportion ?proportioned-olives ?ks-with-olives ?ks-with-cubed-cheese ?target-container-7 black-olive 90 g)
    (seed ?pitted-olives ?olive-pits ?ks-with-pitted-olives ?ks-with-olives ?proportioned-olives ?knife)

    ;; "4 tablespoons olive oil"
    (fetch-and-proportion ?proportioned-olive-oil ?ks-with-olive-oil ?ks-with-pitted-olives ?target-container-8 olive-oil 4 tablespoon)
            
    ;; "8 tablespoons lemon juice"
    (fetch-and-proportion ?proportioned-lemon-juice ?ks-with-lemon-juice ?ks-with-olive-oil ?target-container-9 lemon-juice 8 tablespoon)
    
    ;; "1 clove garlic (minced)"
    (fetch-and-proportion ?proportioned-garlic ?ks-with-garlic ?ks-with-lemon-juice ?target-container-10 garlic 1 piece)
    (cut ?minced-garlic ?ks-with-minced-garlic ?ks-with-garlic ?proportioned-garlic minced ?knife)

    ;; "1 1/2 teaspoon fresh oregano leaves"
    (fetch-and-proportion ?proportioned-oregano ?ks-with-oregano ?ks-with-minced-garlic ?target-container-11 fresh-oregano 1.5 teaspoon)

    ;; "1 head romaine lettuce (washed, chopped)"
    (fetch-and-proportion ?proportioned-lettuce ?ks-with-lettuce ?ks-with-oregano ?target-container-12 romaine-lettuce 1 piece)
    (wash ?washed-lettuce ?ks-with-washed-lettuce ?ks-with-lettuce ?proportioned-lettuce)
    (cut ?chopped-lettuce ?ks-with-chopped-lettuce ?ks-with-washed-lettuce ?washed-lettuce chopped ?knife)

    ;; "1/2 teaspoon salt"
    (fetch-and-proportion ?proportioned-salt ?ks-with-salt ?ks-with-chopped-lettuce ?target-container-13 salt 1/2 teaspoon)

    ;; "1/2 teaspoon black pepper (ground)"
    (fetch-and-proportion ?proportioned-pepper ?ks-with-pepper ?ks-with-salt ?target-container-14 ground-black-pepper 1/2 teaspoon)

    ;; "Gather the ingredients."
    ; (already happened)

    ;; "Peel the cucumber. Slice it into thin rounds."
    (peel ?peeled-cucumber ?cucumber-peels ?ks-with-peeled-cucumber ?ks-with-pepper ?proportioned-cucumber ?knife)
    (cut ?sliced-cucumber ?ks-with-sliced-cucumber ?ks-with-peeled-cucumber ?peeled-cucumber slices ?knife)

    ;; "Cut out the stem and core of the tomato and chop."
    (seed ?seeded-tomato ?tomato-core ?ks-with-seeded-tomato ?ks-with-sliced-cucumber ?proportioned-tomato ?knife)
    (cut ?chopped-tomato ?ks-with-chopped-tomato ?ks-with-seeded-tomato ?proportioned-tomato chopped ?knife)

    ;; "Thinly slice the radishes and onion."
    (cut ?sliced-radish ?ks-with-sliced-radish ?ks-with-chopped-tomato ?proportioned-radish fine-slices ?knife)
    (cut ?sliced-onion ?ks-with-sliced-onion ?ks-with-sliced-radish ?proportioned-onion fine-slices ?knife) 

    ;; "Slice the bell pepper into strips."
    (cut ?sliced-bell-pepper ?ks-with-sliced-bell-pepper ?ks-with-sliced-onion ?proportioned-onion fine-slices ?knife) 

    ;; "Combine the cucumber and tomato with the sliced radishes and onion in a large bowl."
    (fetch ?large-bowl ?ks-with-large-bowl ?ks-with-sliced-bell-pepper large-bowl 1)
    
    (transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-large-bowl ?large-bowl ?sliced-cucumber ?quantity-a ?unit-a)
    (transfer-contents ?output-container-b ?rest-b ?output-ks-b ?output-ks-a ?output-container-a ?chopped-tomato ?quantity-b ?unit-b)
    (transfer-contents ?output-container-c ?rest-c ?output-ks-c ?output-ks-b ?output-container-b ?sliced-radish ?quantity-c ?unit-c)
    (transfer-contents ?output-container-d ?rest-d ?output-ks-d ?output-ks-c ?output-container-c ?sliced-onion ?quantity-d ?unit-d)
    (mingle ?veggies-mix ?ks-with-veggies-mix ?output-ks-d ?output-container-d ?wooden-spoon)
 
    ;; "Add the feta cheese and olives."
    (transfer-contents ?output-container-e ?rest-e ?output-ks-e ?ks-with-veggies-mix ?veggies-mix ?cubed-cheese ?quantity-e ?unit-e)
    (transfer-contents ?output-container-f ?rest-f ?output-ks-f ?output-ks-e ?output-container-e ?pitted-olives ?quantity-f ?unit-f)
    (mingle ?cheesy-veggies-mix ?ks-with-cheesy-veggies-mix ?output-ks-f ?output-container-f ?wooden-spoon)

    ;; "In a small bowl combine the olive oil, lemon juice, garlic, and oregano. Whisk to blend."
    (fetch ?small-bowl ?ks-with-small-bowl ?ks-with-cheesy-veggies-mix small-bowl 1)

    (transfer-contents ?output-container-g ?rest-g ?output-ks-g ?ks-with-small-bowl ?small-bowl ?proportioned-olive-oil ?quantity-g ?unit-g)
    (transfer-contents ?output-container-h ?rest-h ?output-ks-h ?output-ks-g ?output-container-g ?proportioned-lemon-juice ?quantity-h ?unit-h)
    (transfer-contents ?output-container-i ?rest-i ?output-ks-i ?output-ks-h ?output-container-h ?minced-garlic ?quantity-i ?unit-i)
    (transfer-contents ?output-container-j ?rest-j ?output-ks-j ?output-ks-i ?output-container-i ?proportioned-oregano ?quantity-j ?unit-j)
    (mix ?dressing ?ks-with-dressing ?output-ks-j ?output-container-j ?whisk)

    ;; "Toss the vegetables with lemon and olive oil mixture."
    (transfer-contents ?output-container-k ?rest-k ?output-ks-k ?ks-with-dressing ?cheesy-veggies-mix ?dressing ?quantity-k ?unit-k)
    (mingle ?salad ?ks-with-salad ?output-ks-k ?output-container-k ?wooden-spoon)

    ;; "Sprinkle lightly with salt and ground black papper."
    (sprinkle ?salt-salad ?ks-with-salt-salad ?ks-with-salad ?salad ?proportioned-salt)
    (sprinkle ?greek-salad ?ks-with-greek-salad ?ks-with-salt-salad ?salt-salad ?proportioned-pepper)))

    ;; "Enjoy!"
    ; (not an actual instruction)

;; ======================
;; Append bindings to the recipe
;; ======================

(defparameter *extended-recipe*
  (append-meaning-and-irl-bindings *classic-greek-salad-recipe* nil))

;; ======================
;; Evaluate the recipe
;; ======================

;(activate-monitor trace-irl)

;(clear-output)

;(evaluate-irl-program *extended-recipe* nil)

;; ======================
;; Visualise the recipe
;; ======================

;(draw-recipe *classic-greek-salad-recipe*)
;(draw-recipe *extended-recipe*)