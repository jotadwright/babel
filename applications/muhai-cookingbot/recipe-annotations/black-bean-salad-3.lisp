(ql:quickload :muhai-cookingbot)

(in-package :muhai-cookingbot)

;; The 'trace-irl' monitor will make sure that
;; the IRL evaluation process is shown on the web
;; interface (which can be found at localhost:8000).
;; We need to activate it:
;(activate-monitor trace-irl)

;; ###########################################################################
;; Black Bean and Corn Salad
;; https://www.cooks.com/recipe/xw4fl847/black-bean-and-corn-salad.html
;; ###########################################################################

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
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 500)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'tomato :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'piece)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 5)))))))
         (make-instance 'pantry
                        :contents (list
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'onion :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'piece)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 10)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'olive-oil :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'l)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 1)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'black-bean :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 500)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'trader-joes-cilantro-salad-dressing :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'l)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 0.5)))))                                          
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
                                                  :contents (list (make-instance 'ground-cumin :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 500)))))

                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'salt :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 500)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'tabasco :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'l)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 0.5)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'corn :amount
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
                                                                                                                         :value 5)))))))
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

(defparameter *black-bean-and-corn-salad-recipe* 
  '((get-kitchen ?kitchen)
    
    ;; "400 grams black beans"
    (fetch-and-proportion ?proportioned-black-beans ?ks-with-black-beans ?kitchen ?target-container-1 black-bean 400 g)

    ;; "400 grams corn"
    (fetch-and-proportion ?proportioned-corn ?ks-with-corn ?ks-with-black-beans ?target-container-2 corn 400 g)

    ;; "1 chopped onion"
    (fetch-and-proportion ?proportioned-onion ?ks-with-onion ?ks-with-corn ?target-container-3 onion 1 piece)
    (cut ?chopped-onion ?ks-with-chopped-onion ?ks-with-onion ?proportioned-onion chopped ?knife)
    
    ;; "2 chopped tomatoes"
    (fetch-and-proportion ?proportioned-tomato ?ks-with-tomatoes ?ks-with-chopped-onion ?target-container-4 tomato 2 piece)
    (cut ?chopped-tomato ?ks-with-chopped-tomato ?ks-with-tomatoes ?proportioned-tomato chopped ?knife) ; reuse knife
    
    ;; "60 grams diced green chili"
    (fetch-and-proportion ?proportioned-chili ?ks-with-chili ?ks-with-chopped-tomato ?target-container-5 green-chili-pepper 60 g)
    (cut ?diced-chili ?ks-with-diced-chili ?ks-with-chili ?proportioned-chili diced ?knife) ; reuse knife
              
    ;; "3 tbs. Trader Joe's cilantro salad dressing"
    (fetch-and-proportion ?proportioned-dressing ?ks-with-dressing ?ks-with-diced-chili ?target-container-6 trader-joes-cilantro-salad-dressing 3 tablespoon)

    ;; "1/2 teaspoon Tabasco"
    (fetch-and-proportion ?proportioned-tabasco ?ks-with-tabasco ?ks-with-dressing ?target-container-7 tabasco 0.5 teaspoon)

    ;; "1/4 teaspoon pepper"
    (fetch-and-proportion ?proportioned-pepper ?ks-with-pepper ?ks-with-tabasco ?target-container-8 ground-black-pepper 300 g)

    ;; "Combine ingredients, mix well."
    (transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-pepper ?empty-bowl ?proportioned-black-beans ?quantity-a ?unit-a)
    (transfer-contents ?output-container-b ?rest-b ?output-ks-b ?output-ks-a ?output-container-a ?proportioned-corn ?quantity-b ?unit-b)
    (transfer-contents ?output-container-c ?rest-c ?output-ks-c ?output-ks-b ?output-container-b ?chopped-onion ?quantity-c ?unit-c)
    (transfer-contents ?output-container-d ?rest-d ?output-ks-d ?output-ks-c ?output-container-c ?chopped-tomato ?quantity-d ?unit-d)
    (transfer-contents ?output-container-e ?rest-e ?output-ks-e ?output-ks-d ?output-container-d ?diced-chili ?quantity-e ?unit-e)
    (transfer-contents ?output-container-f ?rest-f ?output-ks-f ?output-ks-e ?output-container-e ?proportioned-dressing ?quantity-f ?unit-f)
    (transfer-contents ?output-container-g ?rest-g ?output-ks-g ?output-ks-f ?output-container-f ?proportioned-tabasco ?quantity-g ?unit-g)
    (transfer-contents ?output-container-h ?rest-h ?output-ks-h ?output-ks-g ?output-container-g ?proportioned-pepper ?quantity-h ?unit-h)
    (mingle ?salad ?ks-with-salad ?output-ks-h ?output-container-h ?mingling-tool)
    
    ;; "Refrigerate for 1 hour before serving."
    (refrigerate ?cooled-salad ?ks-with-cooled-salad ?ks-with-salad ?salad ?fridge 1 hour)))

;; ======================
;; Append bindings to the recipe
;; ======================

(defparameter *extended-recipe*
  (append-meaning-and-irl-bindings *black-bean-and-corn-salad-recipe* nil))

;; ======================
;; Evaluate the recipe
;; ======================

;(activate-monitor trace-irl)

;(clear-output)

;(evaluate-irl-program *extended-recipe* nil)

;; ======================
;; Visualise the recipe
;; ======================

;(draw-recipe *black-bean-and-corn-salad-recipe*)
;(draw-recipe *extended-recipe*)