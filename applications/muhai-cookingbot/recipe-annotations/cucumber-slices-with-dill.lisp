(ql:quickload :muhai-cookingbot)

(in-package :muhai-cookingbot)

;; The 'trace-irl' monitor will make sure that
;; the IRL evaluation process is shown on the web
;; interface (which can be found at localhost:8000).
;; We need to activate it:
;(activate-monitor trace-irl)

;; ##################################################################
;; Cucumber slices with dill recipe
;; https://www.allrecipes.com/recipe/18304/cucumber-slices-with-dill/
;; ##################################################################

;; Defining the initial kitchen state
(defparameter *initial-kitchen-state* 
  (make-instance 
   'kitchen-state
   :id 'kitchen-state
   :contents
   (list (make-instance 'fridge
                        :contents (list (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'cherry-tomato :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 500)))))
                                        (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'cucumber :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'piece)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 10)))))))
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
                                                  :contents (list (make-instance 'dried-dill-weed :amount
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
                                                  :contents (list (make-instance 'white-vinegar :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 500)))))
                                          
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'water :amount
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
                                                                                                                         :value 500)))))))
         (make-instance 'kitchen-cabinet
                        :contents (list

                                   ;; bowls
                                   (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)

                                   ;; bowl-lids
                                   (make-instance 'medium-bowl-lid) (make-instance 'medium-bowl-lid) (make-instance 'medium-bowl-lid)
                                   (make-instance 'large-bowl-lid) (make-instance 'large-bowl-lid) (make-instance 'large-bowl-lid)

                                   ;; tools
                                   (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                   (make-instance 'wooden-spoon) (make-instance 'wooden-spoon) (make-instance 'wooden-spoon)
                                   (make-instance 'knife) (make-instance 'knife) (make-instance 'knife))))))

;; 'make-html' makes an HTML representation of the kitchen state
;; and 'add-element' transfers that to the web interface
;(add-element (make-html *initial-kitchen-state* :expand-initially t))

(defparameter *cucumber-slices-with-dill-recipe*
  '((get-kitchen ?kitchen)

    ;; "4 large cucumbers, sliced"
    (fetch-and-proportion ?proportioned-cucumbers ?kitchen-state-with-cucumbers ?kitchen ?target-container-1 cucumber 4 piece)
    (cut ?sliced-cucumbers ?kitchen-state-with-sliced-cucumbers ?kitchen-state-with-cucumbers ?proportioned-cucumbers slices ?knife)

    ;; "1 onion, thinly sliced"
    (fetch-and-proportion ?proportioned-onions ?kitchen-state-with-onions ?kitchen-state-with-sliced-cucumbers ?target-container-2 onion 1 piece)
    (cut ?sliced-onions ?kitchen-state-with-sliced-onions ?kitchen-state-with-onions ?proportioned-onions slices ?knife) ;; use the same whisk

    ;; "1 tablespoon dried dill weed"
    (fetch-and-proportion ?proportioned-dill-weed ?kitchen-state-with-dill-weed ?kitchen-state-with-sliced-onions ?target-container-3 dried-dill-weed 1 tablespoon)

    ;; "200 grams white sugar"
    (fetch-and-proportion ?proportioned-white-sugar ?kitchen-state-with-white-sugar ?kitchen-state-with-dill-weed ?target-container-4 white-sugar 200 g)

    ;; "120 ml white vinegar"
    (fetch-and-proportion ?proportioned-white-vinegar ?kitchen-state-with-white-vinegar ?kitchen-state-with-white-sugar ?target-container-5 white-vinegar 120 ml)

    ;; "120 ml water"
    (fetch-and-proportion ?proportioned-water ?kitchen-state-with-water ?kitchen-state-with-white-vinegar ?target-container-6 water 120 ml)

    ;; "1 teaspoon salt"
    (fetch-and-proportion ?proportioned-salt ?kitchen-state-with-salt ?kitchen-state-with-water ?target-container-7 salt 1 teaspoon)

    ;; "In a large serving bowl, combine cucumbers, onions and dill."
    (fetch ?large-bowl ?kitchen-state-with-fetched-large-bowl ?kitchen-state-with-salt large-bowl 1) ;; IMPLICIT
    
    (transfer-contents ?output-a ?rest-a ?kitchen-out-a ?kitchen-state-with-fetched-large-bowl ?large-bowl ?proportioned-cucumbers ?quantity-a ?unit-a)
    (transfer-contents ?output-b ?rest-b ?kitchen-out-b ?kitchen-out-a ?output-a ?proportioned-onions ?quantity-b ?unit-b)
    (transfer-contents ?output-c ?rest-c ?kitchen-out-c ?kitchen-out-b ?output-b ?proportioned-dill-weed ?quantity-c ?unit-c)

    (mingle ?cucumber-mixture ?kitchen-state-with-cucumber-mixture ?kitchen-out-c ?output-c ?mingling-tool)

    ;; "In a medium size bowl combine sugar, vinegar, water and salt; stir until the sugar dissolves."
    (fetch ?medium-bowl ?kitchen-state-with-fetched-medium-bowl ?kitchen-state-with-cucumber-mixture medium-bowl 1) ;; IMPLICIT
    
    (transfer-contents ?output-d ?rest-d ?kitchen-out-d ?kitchen-state-with-fetched-medium-bowl ?medium-bowl ?proportioned-white-sugar ?quantity-d ?unit-d)
    (transfer-contents ?output-e ?rest-e ?kitchen-out-e ?kitchen-out-d ?output-d ?proportioned-white-vinegar ?quantity-e ?unit-e)

    (transfer-contents ?output-f ?rest-f ?kitchen-out-f ?kitchen-out-e ?output-e ?proportioned-water ?quantity-f ?unit-f)

    (transfer-contents ?output-g ?rest-g ?kitchen-out-g ?kitchen-out-f ?output-f ?proportioned-salt ?quantity-g ?unit-g)

    (mix ?liquid-mixture ?kitchen-state-with-liquid-mixture ?kitchen-out-g ?output-g ?mixing-tool) ; TODO RD: or possibly reuse the mingling-tool?

    ; TODO RD: should something special because it's liquid or is transfer-contents enough? (is pour needed?)
    ;; "Pour the liquid mixture over the cucumber mixture."
    (transfer-contents ?output-h ?rest-h ?kitchen-out-h ?kitchen-state-with-liquid-mixture ?cucumber-mixture ?liquid-mixture ?quantity-pour ?unit-pour)

    ;; "Cover and refrigerate at least 2 hours before serving (the longer this dish marinates the tastier it is!)."
    (cover ?covered-mixture ?kitchen-state-with-covered-mixture ?kitchen-out-h ?output-h ?bowl-lid)

    ; TODO RD: refrigerate could be a special case of cool-for-time?
    (refrigerate ?cooled-mixture ?kitchen-state-with-cooled-mixture ?kitchen-state-with-covered-mixture ?covered-mixture ?fridge 2 hour)

    ; TODO RD: include this step or not?
    ;; IMPLICIT: serving involves uncovering the salad bowl
    (uncover ?served-salad ?cover ?kitchen-state-with-served-salad ?kitchen-state-with-cooled-mixture ?cooled-mixture)))

;; ======================
;; Append bindings to the recipe
;; ======================

(defparameter *extended-recipe*
  (append-meaning-and-irl-bindings *cucumber-slices-with-dill-recipe* nil))

;; ======================
;; Evaluate the recipe
;; ======================

;(activate-monitor trace-irl)

;(clear-output)

;(evaluate-irl-program *extended-recipe* nil)

;; ======================
;; Visualise the recipe
;; ======================

;(draw-recipe *cucumber-slices-with-dill-recipe*)
;(draw-recipe *extended-recipe*)
