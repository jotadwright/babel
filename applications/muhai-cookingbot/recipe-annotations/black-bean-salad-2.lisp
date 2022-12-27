(ql:quickload :muhai-cookingbot)

(in-package :muhai-cookingbot)

;; The 'trace-irl' monitor will make sure that
;; the IRL evaluation process is shown on the web
;; interface (which can be found at localhost:8000).
;; We need to activate it:
;(activate-monitor trace-irl)

;; ##################################################################
;; Black Bean Salad
;; https://www.food.com/recipe/black-bean-salad-213207
;; ##################################################################

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
                                                  :contents (list (make-instance 'cherry-tomato :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'piece)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 5)))))))
         (make-instance 'pantry
                        :contents (list  (make-instance 'medium-bowl
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
                                                        :contents (list (make-instance 'garlic :amount
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
                                                        :contents (list (make-instance 'scallion :amount
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
                                                        :contents (list (make-instance 'red-chili-pepper :amount
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

(defparameter *black-bean-recipe* 
  '((get-kitchen ?kitchen)
    
    ;; "3 teaspoons lime juice"
    (fetch-and-proportion ?proportioned-lime-juice ?ks-with-lime-juice ?kitchen ?target-container-1 lime-juice 3 teaspoon)

    ;; "1 garlic clove, minced"
    (fetch-and-proportion ?proportioned-garlic ?ks-with-garlic ?ks-with-lime-juice ?target-container-2 garlic 1 piece)
    (cut ?minced-garlic ?ks-with-minced-garlic ?ks-with-garlic ?proportioned-garlic minced ?knife)

    ;; "3/4 teaspoon ground cumin"
    (fetch-and-proportion ?proportioned-cumin ?ks-with-cumin ?ks-with-minced-garlic ?target-container-3 ground-cumin 0.75 teaspoon)
    
    ;; "1/2 teaspoon salt"
    (fetch-and-proportion ?proportioned-salt ?ks-with-salt ?ks-with-cumin ?target-container-4 salt 0.5 teaspoon)
    
    ;; "1/4 teaspoon pepper"
    (fetch-and-proportion ?proportioned-pepper ?ks-with-pepper ?ks-with-salt ?target-container-5 ground-black-pepper 0.25 teaspoon)
              
    ;; "2 tablespoons olive oil"
    (fetch-and-proportion ?proportioned-olive-oil ?ks-with-olive-oil ?ks-with-pepper ?target-container-6 olive-oil 2 tablespoon)

    ;; "425 grams black beans, rinsed"
    (fetch-and-proportion ?proportioned-black-beans ?ks-with-black-beans ?ks-with-olive-oil ?target-container-7 black-bean 425 g)
    (wash ?washed-black-beans ?ks-with-washed-black-beans ?ks-with-black-beans ?proportioned-black-beans)

    ;; "300 grams cherry tomatoes"
    (fetch-and-proportion ?proportioned-cherry-tomatoes ?ks-with-cherry-tomatoes ?ks-with-washed-black-beans ?target-container-8 cherry-tomato 300 g)

    ;; "4 scallions, sliced"
    (fetch-and-proportion ?proportioned-scallions ?ks-with-scallions ?ks-with-cherry-tomatoes ?target-container-9 scallion 4 piece)
    (cut ?sliced-scallions ?ks-with-sliced-scallions ?ks-with-scallions ?proportioned-scallions slices ?knife)

    ;; "1 red chili pepper, cut into thin strips"
    (fetch-and-proportion ?proportioned-red-pepper ?ks-with-red-pepper ?ks-with-sliced-scallions ?target-container-10 red-chili-pepper 1 piece)
    (cut ?cut-red-pepper ?ks-with-cut-red-pepper ?ks-with-red-pepper ?proportioned-red-pepper fine-slices ?knife) ; reuse the same knife

    ;; "3 tablespoons chopped cilantro"
    (fetch-and-proportion ?proportioned-cilantro ?ks-with-cilantro ?ks-with-cut-red-pepper ?target-container-11 fresh-cilantro 3 tablespoon)
    (cut ?chopped-cilantro ?ks-with-chopped-cilantro ?ks-with-cilantro ?proportioned-cilantro chopped ?knife) ; reuse the same knife

    ;; "In small bowl stir together lime juice, garlic, cumin, salt and pepper."
    (fetch ?small-bowl ?ks-with-small-bowl ?ks-with-chopped-cilantro small-bowl 1) ;; IMPLICIT

    (transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-small-bowl ?small-bowl ?proportioned-lime-juice ?quantity-a ?unit-a)
    (transfer-contents ?output-container-b ?rest-b ?output-ks-b ?output-ks-a ?output-container-a ?minced-garlic ?quantity-b ?unit-b)
    (transfer-contents ?output-container-c ?rest-c ?output-ks-c ?output-ks-b ?output-container-b ?proportioned-cumin ?quantity-c ?unit-c)
    (transfer-contents ?output-container-d ?rest-d ?output-ks-d ?output-ks-c ?output-container-c ?proportioned-salt ?quantity-d ?unit-d)
    (transfer-contents ?output-container-e ?rest-e ?output-ks-e ?output-ks-d ?output-container-d ?proportioned-pepper ?quantity-e ?unit-e)
    (mix ?dressing ?ks-with-dressing ?output-ks-e ?output-container-e ?mixing-tool)

    ;; "Slowly whisk in oil."
    (transfer-contents ?output-container-f ?rest-f ?output-ks-f ?ks-with-dressing ?output-container-e ?proportioned-olive-oil ?quantity-f ?unit-f)
    (mix ?stirred-mixture ?ks-with-stirred-mixture ?output-ks-f ?output-container-f ?mixing-tool) ; reuse mixing tool

    ;; "In large bowl combine beans, tomatoes, scallions, red pepper and cilantro."
    (fetch ?large-bowl ?ks-with-large-bowl ?ks-with-stirred-mixture large-bowl 1) ;; IMPLICIT

    (transfer-contents ?output-container-g ?rest-g ?output-ks-g ?ks-with-large-bowl ?large-bowl ?washed-black-beans ?quantity-g ?unit-g)
    (transfer-contents ?output-container-h ?rest-h ?output-ks-h ?output-ks-g ?output-container-g ?proportioned-cherry-tomatoes ?quantity-h ?unit-h)
    (transfer-contents ?output-container-i ?rest-i ?output-ks-i ?output-ks-h ?output-container-h ?sliced-scallions ?quantity-i ?unit-i)
    (transfer-contents ?output-container-j ?rest-j ?output-ks-j ?output-ks-i ?output-container-i ?cut-red-pepper ?quantity-j ?unit-j)
    (transfer-contents ?output-container-k ?rest-k ?output-ks-k ?output-ks-j ?output-container-j ?chopped-cilantro ?quantity-k ?unit-k)
    (mingle ?combined-mixture ?ks-with-combined-mixture ?output-ks-k ?output-container-k ?mingling-tool)

    ;; "Toss with dressing to coat."
    (transfer-contents ?output-container-l ?rest-l ?output-ks-l ?ks-with-combined-mixture ?combined-mixture ?dressing ?quantity-l ?unit-l)
    (mingle ?salad ?ks-with-salad ?output-ks-l ?output-container-l ?mingling-tool)

    ;; "Let stand 15 minutes before serving."
    (leave-for-time ?served-salad ?ks-with-served-salad ?ks-with-salad ?salad 15 minute)))


;; ======================
;; Append bindings to the recipe
;; ======================

(defparameter *extended-recipe*
  (append-meaning-and-irl-bindings *black-bean-recipe* nil))

;; ======================
;; Evaluate the recipe
;; ======================

;(activate-monitor trace-irl)

;(clear-output)

;(evaluate-irl-program *extended-recipe* nil)

;; ======================
;; Visualise the recipe
;; ======================

;(draw-recipe *black-bean-recipe*)
;(draw-recipe *extended-recipe*)