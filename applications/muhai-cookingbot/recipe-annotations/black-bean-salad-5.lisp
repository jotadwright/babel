(ql:quickload :muhai-cookingbot)

(in-package :muhai-cookingbot)

;; The 'trace-irl' monitor will make sure that
;; the IRL evaluation process is shown on the web
;; interface (which can be found at localhost:8000).
;; We need to activate it:
;(activate-monitor trace-irl)

;; #########################################################
;; Black Bean Salad
;; https://www.simplyrecipes.com/recipes/black_bean_salad
;; #########################################################

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
                                                                                                                         :value 5)))))))
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
                                                        :contents (list (make-instance 'black-bean :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 500)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'red-bell-pepper :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'piece)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 10)))))                                          
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'olive-oil :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 50)))))                                         
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'ground-black-pepper :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 500)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'jalapeno :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'piece)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 5)))))

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
                                                        :contents (list (make-instance 'green-onion :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'piece)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 5)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'fresh-cilantro :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 50)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'avocado :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'piece)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 5)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'frozen-corn :amount
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

(defparameter *black-bean-salad-recipe* 
  '((get-kitchen ?kitchen)
    
    ;; "400 grams black beans, rinsed"
    (fetch-and-proportion ?proportioned-black-beans ?ks-with-black-beans ?kitchen ?target-container-1 black-bean 400 g)
    (wash ?washed-black-beans ?ks-with-washed-black-beans ?ks-with-black-beans ?proportioned-black-beans)

    ;; "250 grams frozen corn, defrosted"
    (fetch-and-proportion ?proportioned-frozen-corn ?ks-with-frozen-corn ?ks-with-washed-black-beans ?target-container-2 frozen-corn 250 g)
    (bring-to-temperature ?defrosted-corn ?ks-with-defrosted-corn ?ks-with-frozen-corn ?proportioned-frozen-corn ?room-temp-quantity ?room-temp-unit)

    ;; "30 grams chopped scallions"
    (fetch-and-proportion ?proportioned-scallions ?ks-with-scallions ?ks-with-defrosted-corn ?target-container-3 green-onion 30 g)
    (cut ?chopped-scallions ?ks-with-chopped-scallions ?ks-with-scallions ?proportioned-scallions chopped ?knife)

    ;; "1 jalapeno pepper, seeded and minced"
    (fetch-and-proportion ?proportioned-jalapeno ?ks-with-jalapeno ?ks-with-chopped-scallions ?target-container-4 jalapeno 1 piece)
    (seed ?seeded-jalapeno ?jalapeno-seeds ?ks-with-seeded-jalapeno ?ks-with-jalapeno ?proportioned-jalapeno ?knife) ;; reuse the same knife
    (cut ?minced-jalapeno ?ks-with-minced-jalapeno ?ks-with-seeded-jalapeno ?seeded-jalapeno minced ?knife)

    ;; "1 red bell pepper, seeded and chopped"
    (fetch-and-proportion ?proportioned-bell-pepper ?ks-with-bell-pepper ?ks-with-minced-jalapeno ?target-container-5 red-bell-pepper 1 piece)
    (seed ?seeded-bell-peper ?bell-pepper-seeds ?ks-with-seeded-bell-pepper ?ks-with-bell-pepper ?proportioned-bell-pepper ?knife) ;; reuse the same knife
    (cut ?chopped-bell-pepper ?ks-with-chopped-bell-pepper ?ks-with-seeded-bell-pepper ?seeded-bell-peper chopped ?knife)

    ;; "2 tablespoons lime juice"
    (fetch-and-proportion ?proportioned-lime-juice ?ks-with-lime-juice ?ks-with-chopped-bell-pepper ?target-container-6 lime-juice 2 tablespoon)

    ;; "1 tablespoon olive oil"
    (fetch-and-proportion ?proportioned-olive-oil ?ks-with-olive-oil ?ks-with-lime-juice ?target-container-7 olive-oil 1 tablespoon)

    ;; "1 avocado, chopped"
    (fetch-and-proportion ?proportioned-avocado ?ks-with-avocado ?ks-with-olive-oil ?target-container-8 avocado 1 piece)
    (cut ?chopped-avocado ?ks-with-chopped-avocado ?ks-with-avocado ?proportioned-avocado chopped ?knife)

    ;; "1 teaspoon sugar"
    (fetch-and-proportion ?proportioned-sugar ?ks-with-sugar ?ks-with-chopped-avocado ?target-container-9 white-sugar 1 teaspoon)

    ;; "1/2 teaspoon salt and 1/2 teaspoon pepper"
    (fetch-and-proportion ?proportioned-salt ?ks-with-salt ?ks-with-sugar ?target-container-10 salt 0.5 teaspoon)
    (fetch-and-proportion ?proportioned-pepper ?ks-with-pepper ?ks-with-salt ?target-container-11 ground-black-pepper 0.5 teaspoon)

    ;; "10 grams chopped fresh cilantro"
    (fetch-and-proportion ?proportioned-cilantro ?ks-with-cilantro ?ks-with-pepper ?target-container-12 fresh-cilantro 10 g)
    (cut ?chopped-cilantro ?ks-with-chopped-cilantro ?ks-with-cilantro ?proportioned-cilantro diced ?knife)

    ;; "In a large bowl, gently mix the black beans, corn, scallions, jalapeños, red bell pepper, lime juice, and olive oil."
    (fetch ?large-bowl ?ks-with-large-bowl ?ks-with-chopped-cilantro large-bowl 1) ;; IMPLICIT

    (transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-large-bowl ?large-bowl ?washed-black-beans ?quantity-a ?unit-a)
    (transfer-contents ?output-container-b ?rest-b ?output-ks-b ?output-ks-a ?output-container-a ?defrosted-corn ?quantity-b ?unit-b)       
    (transfer-contents ?output-container-c ?rest-c ?output-ks-c ?output-ks-b ?output-container-b ?chopped-scallions ?quantity-c ?unit-c)
    (transfer-contents ?output-container-d ?rest-d ?output-ks-d ?output-ks-c ?output-container-c ?minced-jalapeno ?quantity-d ?unit-d)
    (transfer-contents ?output-container-e ?rest-e ?output-ks-e ?output-ks-d ?output-container-d ?chopped-bell-pepper ?quantity-e ?unit-e)
    (transfer-contents ?output-container-f ?rest-f ?output-ks-f ?output-ks-e ?output-container-e ?proportioned-lime-juice ?quantity-f ?unit-f)
    (transfer-contents ?output-container-g ?rest-g ?output-ks-g ?output-ks-f ?output-container-f ?proportioned-olive-oil ?quantity-g ?unit-g)
    (mingle ?bean-mix ?ks-with-bean-mix ?output-ks-g ?output-container-g ?mingling-tool)

    ;; "Then, gently fold in the chopped avocados."
    (transfer-contents ?output-container-h ?rest-h ?output-ks-h ?ks-with-bean-mix ?bean-mix ?chopped-avocado ?quantity-h ?unit-h)
    (mingle ?salad-base ?ks-with-salad-base ?output-ks-h ?output-container-h ?mingling-tool) ;; reuse mingling tool

    ;; "Add salt and pepper. Sprinkle with sugar, enough to balance the acidity from the lime juice."
    (transfer-contents ?output-container-i ?rest-i ?output-ks-i ?ks-with-salad-base ?salad-base ?proportioned-salt ?quantity-i ?unit-i)
    (transfer-contents ?output-container-j ?rest-j ?output-ks-j ?ks-with-bean-mix ?output-container-i ?proportioned-pepper ?quantity-j ?unit-j)
    (mingle ?seasoned-salad ?ks-with-seasoned-salad ?output-ks-j ?output-container-j ?mingling-tool) ;; IMPLICIT
    (sprinkle ?sugar-salad ?ks-with-sugar-salad ?ks-with-seasoned-salad ?seasoned-salad ?proportioned-sugar)

    ;; "Chill."
    (refrigerate ?cooled-salad ?ks-with-cooled-salad ?ks-with-sugar-salad ?sugar-salad ?fridge ?cooling-quantity ?cooling-unit) 

    ;; "Right before serving, add the chopped fresh cilantro."
    (transfer-contents ?output-container-k ?rest-k ?output-ks-k ?ks-with-cooled-salad ?cooled-salad ?chopped-cilantro ?quantity-k ?unit-k)
    (mingle ?black-bean-salad ?ks-with-black-bean-salad ?output-ks-k ?output-container-k ?mingling-tool))) ;; IMPLICIT

;; ======================
;; Append bindings to the recipe
;; ======================

(defparameter *extended-recipe*
  (append-meaning-and-irl-bindings *black-bean-salad-recipe* nil))

;; ======================
;; Evaluate the recipe
;; ======================

;(activate-monitor trace-irl)

;(clear-output)

;(evaluate-irl-program *extended-recipe* nil)

;; ======================
;; Visualise the recipe
;; ======================

;(draw-recipe *black-bean-salad-recipe*)
;(draw-recipe *extended-recipe*)