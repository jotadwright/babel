(ql:quickload :muhai-cookingbot)

(in-package :muhai-cookingbot)

;; The 'trace-irl' monitor will make sure that
;; the IRL evaluation process is shown on the web
;; interface (which can be found at localhost:8000).
;; We need to activate it:
;(activate-monitor trace-irl)

;; ############################################################
;; Classic Potato Salad
;; https://www.food.com/recipe/classic-potato-salad-22747
;; ############################################################

;; Defining the initial kitchen state
(defparameter *initial-kitchen-state* 
  (make-instance 
   'kitchen-state
   :contents
   (list (make-instance 'fridge
                        :contents (list
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'hard-boiled-egg :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'piece)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 12)))))
                                   (make-instance 'medium-bowl
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
                                                        :contents (list (make-instance 'white-sugar :amount
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
                                                        :contents (list (make-instance 'olive-oil :amount
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
                                                        :contents (list (make-instance 'onion :amount
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
                                                        :contents (list (make-instance 'mayonnaise :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'l)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 0.5)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'cider-vinegar :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'l)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 0.5)))))
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
                                                        :contents (list (make-instance 'yellow-mustard :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'l)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 0.5)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'garlic-powder :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 500)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'potato :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'piece)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 12)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'water :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'l)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 1)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'celery :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'piece)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 6)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'paprika-powder :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 300)))))
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

                                   ;; cooking pots
                                   (make-instance 'cooking-pot) (make-instance 'cooking-pot) (make-instance 'cooking-pot)
                                   (make-instance 'cooking-pot) (make-instance 'cooking-pot) (make-instance 'cooking-pot)

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

                                   (make-instance 'colander) (make-instance 'colander) (make-instance 'colander)

                                   ;; baking equipment
                                   (make-instance 'cookie-sheet)
                                   (make-instance 'baking-tray)
                                   (make-instance 'baking-paper))))))

;; 'make-html' makes an HTML representation of the kitchen state
;; and 'add-element' transfers that to the web interface
;(add-element (make-html *initial-kitchen-state* :expand-initially t))

(defparameter *classic-potato-salad-recipe* 
  '((get-kitchen ?kitchen)

    ;; "8 potatoes, cooked and diced"
    ; cooking and dicing is repeated in the instructions
    (fetch-and-proportion ?proportioned-potatoes ?ks-with-potatoes ?kitchen ?target-container-1 potato 8 piece)

    ;; "350 grams mayonnaise"
    (fetch-and-proportion ?proportioned-mayo ?ks-with-mayo ?ks-with-potatoes ?target-container-2 mayonnaise 350 g)

    ;; "2 tablespoons cider vinegar"
    (fetch-and-proportion ?proportioned-cider-vinegar ?ks-with-cider-vinegar ?ks-with-mayo ?target-container-3 cider-vinegar 2 tablespoon)

    ;; "2 tablespoons sugar"
    (fetch-and-proportion ?proportioned-sugar ?ks-with-sugar ?ks-with-cider-vinegar ?target-container-4 white-sugar 2 tablespoon)

    ;; "1 tablespoon yellow mustard"
    (fetch-and-proportion ?proportioned-mustard ?ks-with-mustard ?ks-with-sugar ?target-container-5 yellow-mustard 1 tablespoon)

    ;; "1 teaspoon salt, divided"
    (fetch-and-proportion ?proportioned-salt-1 ?ks-with-salt-1 ?ks-with-mustard ?target-container-6 salt 1/2 teaspoon)
    (fetch-and-proportion ?proportioned-salt-2 ?ks-with-salt-2 ?ks-with-salt-1 ?target-container-7 salt 1/2 teaspoon)

    ;; "1 teaspoon garlic powder"
    (fetch-and-proportion ?proportioned-garlic-powder ?ks-with-garlic-powder ?ks-with-salt-2 ?target-container-8 garlic-powder 1 teaspoon)

    ;; "1/2 teaspoon pepper"
    (fetch-and-proportion ?proportioned-pepper ?ks-with-pepper ?ks-with-garlic-powder ?target-container-9 ground-black-pepper 1/2 teaspoon)

    ;; "2 celery ribs, sliced"
    (fetch-and-proportion ?proportioned-celery ?ks-with-celery ?ks-with-pepper ?target-container-10 celery 2 piece)
    (cut ?sliced-celery ?ks-with-sliced-celery ?ks-with-celery ?proportioned-celery slices ?knife)

    ;; "50 grams onion, minced"
    (fetch-and-proportion ?proportioned-onion ?ks-with-onion ?ks-with-sliced-celery ?target-container-11 onion 50 g)
    (cut ?minced-onion ?ks-with-minced-onion ?ks-with-onion ?proportioned-onion minced ?knife)

    ;; "5 hard-boiled eggs"
    (fetch-and-proportion ?proportioned-eggs ?ks-with-eggs ?ks-with-onion ?target-container-12 hard-boiled-egg 50 g)

    ;; "1/2 teaspoon paprika"
    (fetch-and-proportion ?proportioned-paprika ?ks-with-paprika ?ks-with-eggs ?target-container-13 paprika-powder 1/2 teaspoon)

    ;; "Boil peeled potatoes in a pot with 500 ml of salted water. Cool to room temperature."
    (fetch ?cooking-pot ?ks-with-cooking-pot ?ks-with-paprika cooking-pot 1)
    (fetch-and-proportion ?proportioned-water ?ks-with-water ?ks-with-cooking-pot ?target-container-14 water 500 ml)

    (peel ?peeled-potato ?potato-peel ?ks-with-peeled-potato ?ks-with-water ?proportioned-potatoes ?knife)
    
    (transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-peeled-potato ?cooking-pot ?proportioned-water ?quantity-a ?unit-a)
    (transfer-contents ?output-container-b ?rest-b ?output-ks-b ?output-ks-a ?output-container-a ?proportioned-salt-1 ?quantity-b ?unit-b)
    (transfer-contents ?output-container-c ?rest-c ?output-ks-c ?output-ks-b ?output-container-b ?peeled-potato ?quantity-c ?unit-c)

    (boil ?boiled-potatoes ?ks-with-boiled-potatoes ?output-ks-c ?output-container-c ?stove ?boiling-time-qty ?boiling-time-unit)
    (drain ?drained-potato ?rest-liquid ?ks-with-drained-potatoes ?ks-with-boiled-potatoes ?boiled-potatoes ?colander)

    (bring-to-temperature ?cooled-potatoes ?ks-with-cooled-potatoes ?ks-with-drained-potatoes ?drained-potato ?room-temp-quantity ?room-temp-unit)
    
    ;; "Place diced potatoes in large bowl."
    (fetch ?large-bowl ?ks-with-large-bowl ?ks-with-cooled-potatoes large-bowl 1)
    (cut ?diced-potato ?ks-with-diced-potato ?ks-with-large-bowl ?drained-potato diced ?knife)
    (transfer-contents ?diced-potatoes-in-large-bowl ?rest-d ?output-ks-d ?ks-with-diced-potato ?large-bowl ?diced-potato ?quantity-d ?unit-d)

    ;; "Mix mayonnaise, cider vinegar, sugar, mustard, salt, garlic powder, and pepper in another bowl."
    (transfer-contents ?output-container-e ?rest-e ?output-ks-e ?output-ks-d ?empty-bowl ?proportioned-mayo ?quantity-e ?unit-e)
    (transfer-contents ?output-container-f ?rest-f ?output-ks-f ?output-ks-e ?output-container-e ?proportioned-cider-vinegar ?quantity-f ?unit-f)  
    (transfer-contents ?output-container-g ?rest-g ?output-ks-g ?output-ks-f ?output-container-f ?proportioned-sugar ?quantity-g ?unit-g)
    (transfer-contents ?output-container-h ?rest-h ?output-ks-h ?output-ks-g ?output-container-g ?proportioned-mustard ?quantity-h ?unit-h)
    (transfer-contents ?output-container-i ?rest-i ?output-ks-i ?output-ks-h ?output-container-h ?proportioned-salt-2 ?quantity-i ?unit-i)
    (transfer-contents ?output-container-j ?rest-j ?output-ks-j ?output-ks-i ?output-container-i ?proportioned-garlic-powder ?quantity-j ?unit-j)
    (transfer-contents ?output-container-k ?rest-k ?output-ks-k ?output-ks-j ?output-container-j ?proportioned-pepper ?quantity-k ?unit-k)
    (mix ?dressing ?ks-with-dressing ?output-ks-k ?output-container-k ?whisk)
    
    ;; "Add to potatoes."
    (transfer-contents ?output-container-l ?rest-l ?output-ks-l ?ks-with-dressing ?diced-potatoes-in-large-bowl ?dressing ?quantity-l ?unit-l)

    ;; "Add celery and onions and mix well."
    (transfer-contents ?output-container-m ?rest-m ?output-ks-m ?output-ks-l ?output-container-l ?sliced-celery ?quantity-m ?unit-m)
    (transfer-contents ?output-container-n ?rest-n ?output-ks-n ?output-ks-m ?output-container-m ?minced-onion ?quantity-n ?unit-n)
    (mingle ?salad ?ks-with-salad ?output-ks-n ?output-container-n ?wooden-spoon)

    ;; "Stir in eggs."
    (transfer-contents ?output-container-o ?rest-o ?output-ks-o ?ks-with-salad ?salad ?proportioned-eggs ?quantity-o ?unit-o)
    (mingle ?salad-with-eggs ?ks-with-egg-salad ?output-ks-o ?output-container-o ?wooden-spoon)

    ;; "Sprinkle a little paprika on top."
    (sprinkle ?potato-salad ?ks-with-potato-salad ?ks-with-egg-salad ?salad-with-eggs ?proportioned-paprika)))

;; ======================
;; Append bindings to the recipe
;; ======================

(defparameter *extended-recipe*
  (append-meaning-and-irl-bindings *classic-potato-salad-recipe* nil))

;; ======================
;; Evaluate the recipe
;; ======================

;(activate-monitor trace-irl)

;(clear-output)

;(evaluate-irl-program *extended-recipe* nil)

;; ======================
;; Visualise the recipe
;; ======================

;(draw-recipe *classic-potato-salad-recipe*)
;(draw-recipe *extended-recipe*)
