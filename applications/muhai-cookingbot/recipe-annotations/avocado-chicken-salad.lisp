(ql:quickload :muhai-cookingbot)

(in-package :muhai-cookingbot)

;; The 'trace-irl' monitor will make sure that
;; the IRL evaluation process is shown on the web
;; interface (which can be found at localhost:8000).
;; We need to activate it:
;(activate-monitor trace-irl)

;; #################################################################
;; Avocado Chicken Salad
;; https://www.simplyrecipes.com/recipes/avocado_chicken_salad
;; #################################################################

;; Defining the initial kitchen state
(defparameter *initial-kitchen-state* 
  (make-instance 
   'kitchen-state
   :contents
   (list (make-instance 'fridge
                        :contents (list
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'cooked-chicken :amount
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
                                                        :contents (list (make-instance 'lime-juice :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'ml)
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
                                                        :contents (list (make-instance 'coarse-salt :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 500)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'garlic-powder :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 500)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'fresh-cilantro :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 200)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'celery :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'piece)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 6)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'avocado :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'piece)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 6)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'apple :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'piece)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 6)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'mixed-greens :amount
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
                                   (make-instance 'fork) (make-instance 'fork) (make-instance 'fork)

                                   (make-instance 'colander) (make-instance 'colander) (make-instance 'colander)

                                   ;; baking equipment
                                   (make-instance 'cookie-sheet)
                                   (make-instance 'baking-tray)
                                   (make-instance 'baking-paper))))))

;; 'make-html' makes an HTML representation of the kitchen state
;; and 'add-element' transfers that to the web interface
;(add-element (make-html *initial-kitchen-state* :expand-initially t))

(defparameter *avocado-chicken-salad-recipe* 
  '((get-kitchen ?kitchen)

    ;; "140 grams finely chopped cooked chicken"
    (fetch-and-proportion ?proportioned-chicken ?ks-with-chicken ?kitchen ?target-container-1 cooked-chicken 140 g)
    (cut ?chopped-chicken ?ks-with-chopped-chicken ?ks-with-chicken ?proportioned-chicken chopped ?knife)

    ;; "1 ripe avocado, seeded and peeled"
    (fetch-and-proportion ?proportioned-avocado ?ks-with-avocado ?ks-with-chopped-chicken ?target-container-2 avocado 1 piece)
    (seed ?seeded-avocado ?avocado-seed ?ks-with-seeded-avocado ?ks-with-avocado ?proportioned-avocado ?knife) ;; use the same knife
    (peel ?peeled-avocado ?avocado-peel ?ks-with-peeled-avocado ?ks-with-seeded-avocado ?seeded-avocado ?knife) ;; use the same knife

    ;; "1 apple, peeled, cored, and finely chopped"
    (fetch-and-proportion ?proportioned-apple ?ks-with-apple ?ks-with-peeled-avocado ?target-container-3 apple 1 piece)
    (peel ?peeled-apple ?apple-peel ?ks-with-peeled-apple ?ks-with-apple ?proportioned-apple ?knife) ;; use the same knife
    (seed ?seeded-apple ?apple-seed ?ks-with-seeded-apple ?ks-with-peeled-apple ?peeled-apple ?knife) ;; use the same knife
    (cut ?chopped-apple ?ks-with-chopped-apple ?ks-with-seeded-apple ?seeded-apple chopped ?knife) ;; use the same knife

    ;; "25 grams finely chopped celery"
    (fetch-and-proportion ?proportioned-celery ?ks-with-celery ?ks-with-chopped-apple ?target-container-4 celery 25 g)
    (cut ?chopped-celery ?ks-with-chopped-celery ?ks-with-celery ?proportioned-celery finely-chopped ?knife) ;; use the same knife

    ;; "15 grams finely chopped red onion"   
    (fetch-and-proportion ?proportioned-onion ?ks-with-onion ?ks-with-chopped-celery ?target-container-5 red-onion 15 g)
    (cut ?chopped-onion ?ks-with-chopped-onion ?ks-with-onion ?proportioned-onion finely-chopped ?knife) ;; use the same knife

    ;; "2 tablespoons finely chopped fresh cilantro"
    (fetch-and-proportion ?proportioned-cilantro ?ks-with-cilantro ?ks-with-chopped-onion ?target-container-6 fresh-cilantro 2 tablespoon)
    (cut ?chopped-cilantro ?ks-with-chopped-cilantro ?ks-with-cilantro ?proportioned-cilantro finely-chopped ?knife) ;; use the same knife

    ;; "2 teaspoons lime juice"
    (fetch-and-proportion ?proportioned-lime-juice ?ks-with-lime-juice ?ks-with-chopped-cilantro ?target-container-7 lime-juice 2 teaspoon)

    ;; "1/2 teaspoon kosher salt"
    (fetch-and-proportion ?proportioned-salt ?ks-with-salt ?ks-with-lime-juice ?target-container-9 coarse-salt 1/2 teaspoon)

    ;; "1/4 teaspoon freshly ground black pepper"
    (fetch-and-proportion ?proportioned-pepper ?ks-with-pepper ?ks-with-salt ?target-container-10 ground-black-pepper 1/4 teaspoon)

    ;; "Gently mash the avocado with a fork."
    (fetch ?fork ?ks-with-fetched-fork ?ks-with-pepper fork 1) ;; IMPLICIT
    (mash ?mashed-avocado ?ks-with-mashed-avocado ?ks-with-fetched-fork ?peeled-avocado ?fork)
    
    ;; "Place the chicken, mashed avocado, apple, celery, and red onion in a medium bowl. Stir it around with the fork so that everything gets well mixed."
    (fetch ?medium-bowl ?ks-with-fetched-medium-bowl ?ks-with-mashed-avocado medium-bowl 1) ;; IMPLICIT
    (transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-fetched-medium-bowl ?medium-bowl ?chopped-chicken ?quantity-a ?unit-a)
    (transfer-contents ?output-container-b ?rest-b ?output-ks-b ?output-ks-a ?output-container-a ?mashed-avocado ?quantity-b ?unit-b)   
    (transfer-contents ?output-container-c ?rest-c ?output-ks-c ?output-ks-b ?output-container-b ?chopped-apple ?quantity-c ?unit-c)
    (transfer-contents ?output-container-d ?rest-d ?output-ks-d ?output-ks-c ?output-container-c ?chopped-celery ?quantity-d ?unit-d)
    (transfer-contents ?output-container-e ?rest-e ?output-ks-e ?output-ks-d ?output-container-d ?chopped-onion ?quantity-e ?unit-e)
    (mingle ?salad ?ks-with-salad ?output-ks-e ?output-container-e ?fork)

    ;; "Add the cilantro, lime juice, salt, and pepper. Stir in a teaspoon of olive oil."
    (transfer-contents ?output-container-f ?rest-f ?output-ks-f ?ks-with-salad ?salad ?chopped-cilantro ?quantity-f ?unit-f)
    (transfer-contents ?output-container-g ?rest-g ?output-ks-g ?output-ks-f ?output-container-f ?proportioned-lime-juice ?quantity-g ?unit-g)
    (transfer-contents ?output-container-h ?rest-h ?output-ks-h ?output-ks-g ?output-container-g ?proportioned-salt ?quantity-h ?unit-h)
    (transfer-contents ?output-container-i ?rest-i ?output-ks-i ?output-ks-h ?output-container-h ?proportioned-pepper ?quantity-i ?unit-i)

    (fetch-and-proportion ?proportioned-olive-oil ?ks-with-olive-oil ?output-ks-i ?target-container-12 olive-oil 1 teaspoon) ;; IMPLICIT
    (transfer-contents ?output-container-j ?rest-j ?output-ks-j ?ks-with-olive-oil ?output-container-i ?proportioned-olive-oil ?quantity-j ?unit-j)
    (mingle ?seasoned-salad ?ks-with-seasoned-salad ?output-ks-j ?output-container-j ?fork) ;; reuse the fork
    
    ;; "Place in a large bowl and cover completely with plastic wrap so that no part of the chicken salad is exposed to air. Keep it in the fridge."
    (fetch ?large-bowl ?ks-with-fetched-large-bowl ?ks-with-seasoned-salad large-bowl 1) ;; IMPLICIT
    (transfer-contents ?output-container-k ?rest-k ?output-ks-k ?ks-with-fetched-large-bowl ?large-bowl ?seasoned-salad ?quantity-k ?unit-k)

    (fetch ?plastic-wrap ?ks-with-fetched-plastic-wrap ?output-ks-k plastic-wrap 1) ;; IMPLICIT
    (cover ?covered-salad ?ks-with-covered-salad ?ks-with-fetched-plastic-wrap ?output-container-k ?plastic-wrap)

    (refrigerate ?cooled-salad ?ks-with-cooled-salad ?ks-with-covered-salad ?covered-salad ?fridge ?cooling-quantity ?cooling-unit)))

;; ======================
;; Append bindings to the recipe
;; ======================

(defparameter *extended-recipe*
  (append-meaning-and-irl-bindings *avocado-chicken-salad-recipe* nil))

;; ======================
;; Evaluate the recipe
;; ======================

;(activate-monitor trace-irl)

;(clear-output)

;(evaluate-irl-program *extended-recipe* nil)

;; ======================
;; Visualise the recipe
;; ======================

;(draw-recipe *avocado-chicken-salad-recipe*)
;(draw-recipe *extended-recipe*)
