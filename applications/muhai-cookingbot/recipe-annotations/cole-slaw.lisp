(ql:quickload :muhai-cookingbot)

(in-package :muhai-cookingbot)

;; The 'trace-irl' monitor will make sure that
;; the IRL evaluation process is shown on the web
;; interface (which can be found at localhost:8000).
;; We need to activate it:
;(activate-monitor trace-irl)

;; ############################################################
;; Classic Slaw
;; https://www.cooks.com/recipe/r80079ed/cole-slaw.html
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
                                                        :contents (list (make-instance 'green-chili-pepper :amount
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
                                                        :contents (list (make-instance 'carrot :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'piece)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 2)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'green-cabbage :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'piece)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 2)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'paprika-powder :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 300)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'turmeric-powder :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 200)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'grated-horseradish :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 200)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'mustard-seed :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 200)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'celery-seed :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 200))))) 
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

(defparameter *cole-slaw-recipe* 
  '((get-kitchen ?kitchen)

    ;; "1 head green cabbage"
    (fetch-and-proportion ?proportioned-cabbage ?ks-with-cabbage ?kitchen ?target-container-1 green-cabbage 1 piece)

    ;; "2 carrots, thinly shredded"
    (fetch-and-proportion ?proportioned-carrots ?ks-with-carrots ?ks-with-cabbage ?target-container-2 carrot 2 piece)

    ;; "1 onion"
    (fetch-and-proportion ?proportioned-onion ?ks-with-onion ?ks-with-carrots ?target-container-3 onion 1 piece)

    ;; "1 green pepper"
    (fetch-and-proportion ?proportioned-green-pepper ?ks-with-green-pepper ?ks-with-onion ?target-container-4 green-chili-pepper 2 tablespoon)

    ;; "200 g sugar"
    (fetch-and-proportion ?proportioned-sugar ?ks-with-sugar ?ks-with-green-pepper ?target-container-5 white-sugar 200 g)

    ;; "230 g cider vinegar"
    (fetch-and-proportion ?proportioned-vinegar ?ks-with-vinegar ?ks-with-sugar ?target-container-6 cider-vinegar 230 g)

    ;; "1 tsp. salt"
    (fetch-and-proportion ?proportioned-salt ?ks-with-salt ?ks-with-vinegar ?target-container-7 salt 1 teaspoon)

    ;; "1 tablespoon grated horseradish"
    ; horseradish is generally bought already grated
    (fetch-and-proportion ?proportioned-horseradish ?ks-with-horseradish ?ks-with-salt ?target-container-8 grated-horseradish 1 tablespoon)

    ;; "1 tsp. mustard seed"
    (fetch-and-proportion ?proportioned-mustard-seed ?ks-with-mustard-seed ?ks-with-horseradish ?target-container-10 mustard-seed 1 teaspoon)

    ;; "1 tsp. celery seed"
    (fetch-and-proportion ?proportioned-celery-seed ?ks-with-celery-seed ?ks-with-mustard-seed ?target-container-11 celery-seed 1 teaspoon)

    ;; "1 tsp. turmeric"
    (fetch-and-proportion ?proportioned-turmeric ?ks-with-turmeric ?ks-with-celery-seed ?target-container-12 turmeric-powder 1 teaspoon)

    ;; "Shred the cabbage and carrots together, then chop them up a bit."    
    (cut ?shredded-cabbage ?ks-with-shredded-cabbage ?ks-with-turmeric ?proportioned-cabbage shredded ?knife)
    (cut ?shredded-carrots ?ks-with-shredded-carrots ?ks-with-shredded-cabbage ?proportioned-carrots shredded ?knife)
    (transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-shredded-carrots ?empty-container ?shredded-cabbage ?quantity-a ?unit-a)
    (transfer-contents ?output-container-b ?rest-b ?output-ks-b ?output-ks-a ?output-container-a ?shredded-carrots ?quantity-b ?unit-b)
    (mingle ?cabbage-mix ?ks-with-cabbage-mix ?output-ks-b ?output-container-b ?wooden-spoon)
    (cut ?chopped-cabbage-mix ?ks-with-chopped-cabbage-mix ?ks-with-cabbage-mix ?cabbage-mix chopped ?knife)

    ;; "Chop the green pepper and onion and combine it with the cabbage."
    (cut ?chopped-green-pepper ?ks-with-chopped-green-pepper ?ks-with-chopped-cabbage-mix ?proportioned-green-pepper chopped ?knife)
    (cut ?chopped-onion ?ks-with-chopped-onion ?ks-with-chopped-green-pepper ?proportioned-onion chopped ?knife)
    (transfer-contents ?output-container-c ?rest-c ?output-ks-c ?ks-with-chopped-onion ?chopped-cabbage-mix ?chopped-green-pepper ?quantity-c ?unit-c)
    (transfer-contents ?output-container-d ?rest-d ?output-ks-d ?output-ks-c ?output-container-c ?chopped-onion ?quantity-d ?unit-d)
    (mingle ?slaw ?ks-with-slaw ?output-ks-d ?output-container-d ?wooden-spoon)

    ;; "Boil together the vinegar, sugar, salt, and spices."
    (fetch ?cooking-pot ?ks-with-cooking-pot ?ks-with-slaw cooking-pot 1) ;; IMPLICIT

    (transfer-contents ?output-container-e ?rest-e ?output-ks-e ?ks-with-cooking-pot ?cooking-pot ?proportioned-vinegar ?quantity-e ?unit-e)
    (transfer-contents ?output-container-f ?rest-f ?output-ks-f ?output-ks-e ?output-container-e ?proportioned-sugar ?quantity-f ?unit-f)
    (transfer-contents ?output-container-g ?rest-g ?output-ks-g ?output-ks-f ?output-container-f ?proportioned-salt ?quantity-g ?unit-g)
    (transfer-contents ?output-container-h ?rest-h ?output-ks-h ?output-ks-g ?output-container-g ?proportioned-horseradish ?quantity-h ?unit-h)
    (transfer-contents ?output-container-i ?rest-i ?output-ks-i ?output-ks-h ?output-container-h ?proportioned-mustard-seed ?quantity-i ?unit-i)
    (transfer-contents ?output-container-j ?rest-j ?output-ks-j ?output-ks-i ?output-container-i ?proportioned-celery-seed ?quantity-j ?unit-j)
    (transfer-contents ?output-container-k ?rest-k ?output-ks-k ?output-ks-j ?output-container-j ?proportioned-turmeric ?quantity-k ?unit-k)

    (boil ?dressing ?ks-with-dressing ?output-ks-k ?output-container-k ?stove ?heating-mode ?boiling-time-qty ?boiling-time-unit)
  
    ;; "Pour over the slaw and allow it to set in the refrigerator for two hours before serving."
    (transfer-contents ?output-container-l ?rest-l ?output-ks-l ?ks-with-dressing ?slaw ?dressing ?quantity-l ?unit-l)
    (refrigerate ?cole-slaw ?ks-with-cole-slaw ?output-ks-l ?output-container-l ?fridge ?cooling-quantity ?cooling-unit)))

;; ======================
;; Append bindings to the recipe
;; ======================

(defparameter *extended-recipe*
  (append-meaning-and-irl-bindings *cole-slaw-recipe* nil))

;; ======================
;; Evaluate the recipe
;; ======================

;(activate-monitor trace-irl)

;(clear-output)

;(evaluate-irl-program *extended-recipe* nil)

;; ======================
;; Visualise the recipe
;; ======================

;(draw-recipe *cole-slaw-recipe*)
;(draw-recipe *extended-recipe*)
