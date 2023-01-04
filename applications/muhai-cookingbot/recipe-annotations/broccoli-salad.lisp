(ql:quickload :muhai-cookingbot)

(in-package :muhai-cookingbot)

;; The 'trace-irl' monitor will make sure that
;; the IRL evaluation process is shown on the web
;; interface (which can be found at localhost:8000).
;; We need to activate it:
;(activate-monitor trace-irl)

;; #########################################################
;; Broccoli Salad
;; https://www.food.com/recipe/broccoli-salad-10733
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
                                                  :contents (list (make-instance 'mayonnaise :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 500)))))))
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
                                                        :contents (list (make-instance 'cider-vinegar :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'ml)
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
                                                        :contents (list (make-instance 'extra-virgin-olive-oil :amount
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
                                                        :contents (list (make-instance 'grated-mozzarella :amount
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
                                                        :contents (list (make-instance 'red-onion :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'piece)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 5)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'cooked-bacon :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 500)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'broccoli :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'piece)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 2)))))
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

(defparameter *broccoli-salad-recipe* 
  '((get-kitchen ?kitchen)
    
    ;; "1 head fresh broccoli (do not use frozen!)"
    (fetch-and-proportion ?proportioned-broccoli ?ks-with-broccoli ?kitchen ?target-container-1 broccoli 1 piece)

    ;; "50 grams red onion, chopped"
    (fetch-and-proportion ?proportioned-onion ?ks-with-onion ?ks-with-broccoli ?target-container-2 red-onion 50 g)
    (cut ?chopped-onion ?ks-with-chopped-onion ?ks-with-onion ?proportioned-onion chopped ?knife)

    ;; "450 grams cooked bacon"
    (fetch-and-proportion ?proportioned-bacon ?ks-with-bacon ?ks-with-chopped-onion ?target-container-3 cooked-bacon 450 g)

    ;; "2 1/2 tablespoons cider vinegar"
    (fetch-and-proportion ?proportioned-vinegar ?ks-with-vinegar ?ks-with-bacon ?target-container-4 cider-vinegar 2.5 tablespoon)

    ;; "230 grams mayonnaise"
    (fetch-and-proportion ?proportioned-mayo ?ks-with-mayo ?ks-with-vinegar ?target-container-5 mayonnaise 230 g)

    ;; "70 grams sugar"
    (fetch-and-proportion ?proportioned-sugar ?ks-with-sugar ?ks-with-mayo ?target-container-6 white-sugar 70 g)

    ;; "170 grams grated mozzarella cheese"
    (fetch-and-proportion ?proportioned-cheese ?ks-with-grated-cheese ?ks-with-sugar ?target-container-7 grated-mozzarella 170 g)

    ;; "Cut cooked bacon into pieces."
    (cut ?chopped-bacon ?ks-with-chopped-bacon ?ks-with-grated-cheese ?proportioned-bacon chopped ?knife)

    ;; "Chop up broccoli into bite size pieces."
    (cut ?chopped-broccoli ?ks-with-chopped-broccoli ?ks-with-chopped-bacon ?proportioned-broccoli chopped ?knife)

    ;; "Mix broccoli, onions, bacon and mozzarella in large bowl."
    (fetch ?large-bowl-1 ?ks-with-large-bowl-1 ?ks-with-chopped-broccoli large-bowl 1) ;; IMPLICIT

    (transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-large-bowl-1 ?large-bowl-1 ?chopped-broccoli ?quantity-a ?unit-a)
    (transfer-contents ?output-container-b ?rest-b ?output-ks-b ?output-ks-a ?output-container-a ?chopped-onion ?quantity-b ?unit-b)       
    (transfer-contents ?output-container-c ?rest-c ?output-ks-c ?output-ks-b ?output-container-b ?proportioned-cheese ?quantity-c ?unit-c)
    (mingle ?broccoli-mixture ?ks-with-broccoli-mixture ?output-ks-c ?output-container-c ?mingling-tool)

    ;; "In separate large bowl combine vinegar, sugar and mayo."
    (fetch ?large-bowl-2 ?ks-with-large-bowl-2 ?ks-with-broccoli-mixture large-bowl 1) ;; IMPLICIT

    (transfer-contents ?output-container-d ?rest-d ?output-ks-d ?ks-with-large-bowl-2 ?large-bowl-2 ?proportioned-vinegar ?quantity-d ?unit-d)
    (transfer-contents ?output-container-e ?rest-e ?output-ks-e ?output-ks-d ?output-container-d ?proportioned-sugar ?quantity-e ?unit-e)       
    (transfer-contents ?output-container-f ?rest-f ?output-ks-f ?output-ks-e ?output-container-e ?proportioned-mayo ?quantity-f ?unit-f)
    (mix ?dressing ?ks-with-dressing ?output-ks-f ?output-container-f ?mixing-tool)

    ;; "Pour over broccoli mixture and toss to coat."
    (transfer-contents ?output-container-g ?rest-g ?output-ks-g ?ks-with-dressing ?broccoli-mixture ?dressing ?quantity-g ?unit-g)
    (mingle ?broccoli-salad ?ks-with-broccoli-salad ?output-ks-g ?output-container-g ?mingling-tool) ; reuse mingling tool

    ;; "Best if made a day ahead and stored in the refrigerator."
    (refrigerate ?cooled-salad ?ks-with-cooled-salad ?ks-with-broccoli-salad ?broccoli-salad ?fridge ?cooling-quantity ?cooling-unit)))

;; ======================
;; Append bindings to the recipe
;; ======================

(defparameter *extended-recipe*
  (append-meaning-and-irl-bindings *broccoli-salad-recipe* nil))

;; ======================
;; Evaluate the recipe
;; ======================

;(activate-monitor trace-irl)

;(clear-output)

;(evaluate-irl-program *extended-recipe* nil)

;; ======================
;; Visualise the recipe
;; ======================

;(draw-recipe *broccoli-salad-recipe*)
;(draw-recipe *extended-recipe*)