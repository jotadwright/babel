(ql:quickload :muhai-cookingbot)

(in-package :muhai-cookingbot)

;; The 'trace-irl' monitor will make sure that
;; the IRL evaluation process is shown on the web
;; interface (which can be found at localhost:8000).
;; We need to activate it:
;(activate-monitor trace-irl)

;; #########################################################################
;; https://www.allrecipes.com/recipe/234759/easy-cherry-tomato-corn-salad/
;; #########################################################################

;; Defining the initial kitchen state
(defparameter *initial-kitchen-state* 
  (make-instance 
   'kitchen-state
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
         (make-instance 'freezer
                        :contents (list
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'frozen-corn :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 500)))))))
         (make-instance 'pantry
                        :contents (list
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'fresh-basil :amount
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
                                                  :contents (list (make-instance 'extra-virgin-olive-oil :amount
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
                                                  :contents (list (make-instance 'shallot :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'piece)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 5)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'jalapeno :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'piece)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 5)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'lime-juice :amount
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
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)

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
                                   (make-instance 'knife) (make-instance 'knife) (make-instance 'knife))))))

;; 'make-html' makes an HTML representation of the kitchen state
;; and 'add-element' transfers that to the web interface
;(add-element (make-html *initial-kitchen-state* :expand-initially t))

(defparameter *easy-cherry-tomato-corn-salad-recipe*
  '((get-kitchen ?kitchen)

    ; TODO RD: should fresh basil be plucked?
    ;; "5 grams minced fresh basil"
    (fetch-and-proportion ?proportioned-basil ?kitchen-state-with-basil ?kitchen ?target-container-1 fresh-basil 5 g)
    (cut ?minced-basil ?kitchen-state-with-cut-basil ?kitchen-state-with-basil ?proportioned-basil minced ?knife)
    
    ;; "3 tablespoons olive oil"
    (fetch-and-proportion ?olive-oil ?kitchen-state-with-olive-oil ?kitchen-state-with-cut-basil ?target-container-2 olive-oil 3 tablespoon)
    
    ;; "2 teaspoons lime juice"
    (fetch-and-proportion ?lime-juice ?kitchen-state-with-lime-juice ?kitchen-state-with-olive-oil ?target-container-3 lime-juice 2 teaspoon)
    
    ;; "1 teaspoon white sugar"
    (fetch-and-proportion ?white-sugar ?kitchen-state-with-white-sugar ?kitchen-state-with-lime-juice ?target-container-4 white-sugar 1 teaspoon)
    
    ;; "1/2 teaspoon salt"
    (fetch-and-proportion ?salt ?kitchen-state-with-salt ?kitchen-state-with-white-sugar ?target-container-5 salt 0.5 teaspoon)
    
    ;; "1/4 teaspoon ground black pepper"
    (fetch-and-proportion ?pepper ?kitchen-state-with-pepper ?kitchen-state-with-salt ?target-container-6 ground-black-pepper 0.25 teaspoon)
    
    ;; "350 grams frozen corn, thawed"
    (fetch-and-proportion ?frozen-corn ?kitchen-state-with-frozen-corn ?kitchen-state-with-pepper ?target-container-7 frozen-corn 350 g)
    (bring-to-temperature ?thawed-corn ?kitchen-state-with-thawed-corn ?kitchen-state-with-frozen-corn ?frozen-corn 18 degrees-celsius)
        
    ;; "300 grams cherry tomatoes, halved"
    (fetch-and-proportion ?cherry-tomatoes ?kitchen-state-with-cherry-tomatoes ?kitchen-state-with-thawed-corn ?target-container-8 cherry-tomato 300 g)
    (cut ?cut-tomatoes ?kitchen-state-with-cut-tomatoes ?kitchen-state-with-cherry-tomatoes ?cherry-tomatoes halved ?knife) ;; use the same knife

    ;; "160 grams peeled, seeded, and chopped cucumber"
    (fetch-and-proportion ?cucumber ?kitchen-state-with-cucumber ?kitchen-state-with-cut-tomatoes ?target-container-9 cucumber 160 g)
    (peel ?peeled-cucumber ?cucumber-peels ?kitchen-state-with-peeled-cucumber ?kitchen-state-with-cucumber ?cucumber ?knife) ;; use the same knife
    (seed ?seeded-cucumber ?cucumber-seeds ?kitchen-state-with-seeded-cucumber ?kitchen-state-with-peeled-cucumber ?peeled-cucumber ?knife) ;; use the same knife
    (cut ?chopped-cucumber ?kitchen-state-with-chopped-cucumber ?kitchen-state-with-seeded-cucumber ?seeded-cucumber slices ?knife) ;; use the same knife
    
    ;; "1 jalapeno pepper, seeded and chopped"
    (fetch-and-proportion ?jalapeno ?kitchen-state-with-jalapeno  ?kitchen-state-with-chopped-cucumber ?target-container-10 jalapeno 1 piece)
    (seed ?seeded-jalapeno ?jalapeno-seeds ?kitchen-state-with-seeded-jalapeno ?kitchen-state-with-jalapeno ?jalapeno ?knife) ;; use the same knife
    (cut ?chopped-jalapeno ?kitchen-state-with-chopped-jalapeno ?kitchen-state-with-seeded-jalapeno ?seeded-jalapeno slices ?knife) ;; use the same knife

    ;; "2 shallots, minced"
    (fetch-and-proportion ?shallot ?kitchen-state-with-shallot ?kitchen-state-with-chopped-jalapeno ?target-container-11 shallot 2 piece)
    (cut ?cut-shallot ?kitchen-state-with-cut-shallot ?kitchen-state-with-shallot ?shallot minced ?knife)  ;; use the same knife

    ;; "Combine basil, olive oil, lime juice, sugar, salt, and pepper in a jar and screw on the lid;"
    (fetch ?jar ?kitchen-state-with-fetched-jar ?kitchen-state-with-cut-shallot jar 1) ;; IMPLICIT
    ; TODO RD: should jars be closed initially?
    ;(uncover ?uncovered-jar ?jar-lid ?kitchen-state-with-uncovered-jar ?kitchen-state-with-fetched-jar ?jar)

    (transfer-contents ?output-container-a ?rest-a ?output-kitchen-state-a ?kitchen-state-with-fetched-jar ?jar ?minced-basil ?quantity-a ?unit-a)
    (transfer-contents ?output-container-b ?rest-b ?output-kitchen-state-b ?output-kitchen-state-a ?output-container-a ?olive-oil ?quantity-b ?unit-b)
    (transfer-contents ?output-container-c ?rest-c ?output-kitchen-state-c ?output-kitchen-state-b ?output-container-b ?lime-juice ?quantity-c ?unit-c)
    (transfer-contents ?output-container-d ?rest-d ?output-kitchen-state-d ?output-kitchen-state-c ?output-container-c ?white-sugar ?quantity-d ?unit-d)
    (transfer-contents ?output-container-e ?rest-e ?output-kitchen-state-e ?output-kitchen-state-d ?output-container-d ?salt ?quantity-e ?unit-e)
    (transfer-contents ?output-container-f ?rest-f ?output-kitchen-state-f ?output-kitchen-state-e ?output-container-e ?pepper ?quantity-f ?unit-f)

    (cover ?covered-jar ?kitchen-state-with-covered-jar ?output-kitchen-state-f ?output-container-f ?jar-lid)
    
    ;; "Shake until dressing is completely blended."
    (shake ?salad-dressing ?kitchen-state-with-dressing ?kitchen-state-with-covered-jar ?covered-jar)

    ;; "Stir corn, tomatoes, cucumber, jalapeno, and shallots together in a large bowl."
    (fetch ?large-bowl ?kitchen-state-with-fetched-large-bowl ?kitchen-state-with-dressing large-bowl 1) ;; IMPLICIT

    (transfer-contents ?output-container-g ?rest-g ?output-kitchen-state-g ?kitchen-state-with-fetched-large-bowl ?large-bowl ?thawed-corn ?quantity-g ?unit-g)
    (transfer-contents ?output-container-h ?rest-h ?output-kitchen-state-h ?output-kitchen-state-g ?output-container-g ?chopped-cucumber ?quantity-h ?unit-h)
    (transfer-contents ?output-container-i ?rest-i ?output-kitchen-state-i ?output-kitchen-state-h ?output-container-h ?chopped-jalapeno ?quantity-i ?unit-i)
    (transfer-contents ?output-container-j ?rest-j ?output-kitchen-state-j ?output-kitchen-state-i ?output-container-i ?cut-shallot ?quantity-j ?unit-j)

    (mingle ?salad-base ?kitchen-state-with-salad-base ?output-kitchen-state-j ?output-container-j ?wooden-spoon)

    ; TODO RD: is drizzle sprinkle or transfer-contents?
    ;; "Drizzle dressing over corn mixture and toss to coat."
    ;; IMPLICIT: the jar is still closed and should be opened first
    (uncover ?uncovered-jar ?used-jar-lid ?kitchen-state-with-uncovered-jar ?kitchen-state-with-salad-base ?covered-jar)

    (sprinkle ?drizzled-salad-base ?kitchen-state-with-drizzled-salad-base ?kitchen-state-with-uncovered-jar ?salad-base ?salad-dressing)
    
    (mingle ?salad ?kitchen-state-with-salad ?kitchen-state-with-drizzled-salad-base ?drizzled-salad-base ?wooden-spoon) ;; use the same wooden spoon

    ; TODO RD: refrigerate could be a special case of leave-for-time?
    (refrigerate ?cooled-salad ?kitchen-state-with-cooled-salad ?kitchen-state-with-salad ?salad ?fridge ?cooling-quantity ?cooling-unit)))


;; ======================
;; Append bindings to the recipe
;; ======================

(defparameter *extended-recipe*
  (append-meaning-and-irl-bindings *easy-cherry-tomato-corn-salad-recipe* nil))

;; ======================
;; Evaluate the recipe
;; ======================

;(activate-monitor trace-irl)

;(clear-output)

;(evaluate-irl-program *extended-recipe* nil)

;; ======================
;; Visualise the recipe
;; ======================

;(draw-recipe *easy-cherry-tomato-corn-salad-recipe*)
;(draw-recipe *extended-recipe*)
