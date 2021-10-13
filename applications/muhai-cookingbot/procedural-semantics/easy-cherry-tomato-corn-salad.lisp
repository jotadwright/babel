(ql:quickload :aipp-cookingbot)
(in-package :aipp-cookingbot)

;; The 'trace-irl' monitor will make sure that
;; the IRL evaluation process is shown on the web
;; interface (which can be found at localhost:8000).
;; We need to activate it:
(activate-monitor trace-irl)

;; Defining the initial kitchen state
(defparameter *initial-kitchen-state* 
  (make-instance 
   'kitchen-state
   :id 'kitchen-state
   :contents
   `(,(make-instance 'fridge
                          :contents (list (make-instance 'bowl
                                                         :used T
                                                         :contents (list (make-instance 'cherry-tomato :amount
                                                                                        (make-instance 'amount
                                                                                                       :unit (make-instance 'g)
                                                                                                       :quantity (make-instance 'quantity
                                                                                                                                :value 500)))))

                                          (make-instance 'bowl
                                                         :used T
                                                         :contents (list (make-instance 'cucumber :amount
                                                                                        (make-instance 'amount
                                                                                                       :unit (make-instance 'g)
                                                                                                       :quantity (make-instance 'quantity
                                                                                                                                :value 500)))))
                                          (make-instance 'bowl
                                                         :used T
                                                         :contents (list (make-instance 'onion :amount
                                                                                        (make-instance 'amount
                                                                                                       :unit (make-instance 'piece)
                                                                                                       :quantity (make-instance 'quantity
                                                                                                                                :value 10)))))))
    ,(make-instance 'freezer
                          :contents (list
                                          (make-instance 'bowl
                                                         :used T
                                                         :contents (list (make-instance 'frozen-corn :amount
                                                                                        (make-instance 'amount
                                                                                                       :unit (make-instance 'g)
                                                                                                       :quantity (make-instance 'quantity
                                                                                                                                :value 500)))))))
     
   ,(make-instance 'kitchen-cabinet
                          :contents (list
                                     
                                     (make-instance 'knife) (make-instance 'knife) (make-instance 'knife) (make-instance 'knife) (make-instance 'knife)(make-instance 'knife) (make-instance 'knife) (make-instance 'knife) (make-instance 'knife) (make-instance 'knife)(make-instance 'knife) (make-instance 'knife) (make-instance 'knife) (make-instance 'knife) (make-instance 'knife)(make-instance 'knife) (make-instance 'knife) (make-instance 'knife) (make-instance 'knife) (make-instance 'knife)(make-instance 'knife) (make-instance 'knife) (make-instance 'knife) (make-instance 'knife) (make-instance 'knife)
                                     (make-instance 'bowl) (make-instance 'bowl) (make-instance 'bowl) (make-instance 'bowl) (make-instance 'bowl) (make-instance 'bowl) (make-instance 'bowl) (make-instance 'bowl) (make-instance 'bowl) (make-instance 'bowl) (make-instance 'bowl) (make-instance 'bowl)
                                     
                                     (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                     (make-instance 'medium-bowl-lid) (make-instance 'medium-bowl-lid)
                                      (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                     (make-instance 'medium-bowl-lid) (make-instance 'medium-bowl-lid)
                                      (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                     (make-instance 'medium-bowl-lid) (make-instance 'medium-bowl-lid)
                                      (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                     (make-instance 'medium-bowl-lid) (make-instance 'medium-bowl-lid)
                                      (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                     (make-instance 'medium-bowl-lid) (make-instance 'medium-bowl-lid)
                                      (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                     (make-instance 'medium-bowl-lid) (make-instance 'medium-bowl-lid)
                                      (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                     (make-instance 'medium-bowl-lid) (make-instance 'medium-bowl-lid)
                                      (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                     (make-instance 'medium-bowl-lid) (make-instance 'medium-bowl-lid)
                                      (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                     (make-instance 'medium-bowl-lid) (make-instance 'medium-bowl-lid)
                                      (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                     (make-instance 'medium-bowl-lid) (make-instance 'medium-bowl-lid)
                                      (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                     (make-instance 'medium-bowl-lid) (make-instance 'medium-bowl-lid)
                                      (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                     (make-instance 'medium-bowl-lid) (make-instance 'medium-bowl-lid)(make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                     (make-instance 'medium-bowl-lid) (make-instance 'medium-bowl-lid)(make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)(make-instance 'medium-bowl-lid) (make-instance 'medium-bowl-lid)
                                      (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)(make-instance 'medium-bowl-lid) (make-instance 'medium-bowl-lid)(make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)(make-instance 'medium-bowl-lid) (make-instance 'medium-bowl-lid)(make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)(make-instance 'medium-bowl-lid) (make-instance 'medium-bowl-lid)(make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)(make-instance 'medium-bowl-lid) (make-instance 'medium-bowl-lid)(make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)(make-instance 'medium-bowl-lid) (make-instance 'medium-bowl-lid)(make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)(make-instance 'medium-bowl-lid) (make-instance 'medium-bowl-lid)(make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)(make-instance 'medium-bowl-lid) (make-instance 'medium-bowl-lid)(make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                     (make-instance 'medium-bowl-lid) (make-instance 'medium-bowl-lid)
                                      (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                     (make-instance 'medium-bowl-lid) (make-instance 'medium-bowl-lid)
                                      (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                     (make-instance 'medium-bowl-lid) (make-instance 'medium-bowl-lid)
                                     
                                     (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)
                                     (make-instance 'large-bowl-lid) (make-instance 'large-bowl-lid)(make-instance 'jar) (make-instance 'jar-lid)
                                     (make-instance 'jar) (make-instance 'jar-lid)(make-instance 'jar) (make-instance 'jar-lid)(make-instance 'jar) (make-instance 'jar-lid)
        
                                     (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                      (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                      (make-instance 'wooden-spoon)(make-instance 'wooden-spoon)(make-instance 'wooden-spoon)(make-instance 'wooden-spoon)(make-instance 'wooden-spoon)(make-instance 'wooden-spoon)(make-instance 'wooden-spoon)(make-instance 'wooden-spoon)(make-instance 'wooden-spoon)(make-instance 'wooden-spoon)(make-instance 'wooden-spoon)(make-instance 'wooden-spoon)))
   
   ,(make-instance 'pantry
                          :contents (list
                                          (make-instance 'bowl
                                                         :used T
                                                         :contents (list (make-instance 'fresh-basil :amount
                                                                                        (make-instance 'amount
                                                                                                       :unit (make-instance 'g)
                                                                                                       :quantity (make-instance 'quantity
                                                                                                                                :value 500)))))
                                          (make-instance 'bowl
                                                         :used T
                                                         :contents (list (make-instance 'white-sugar :amount
                                                                                        (make-instance 'amount
                                                                                                       :unit (make-instance 'g)
                                                                                                       :quantity (make-instance 'quantity
                                                                                                                                :value 500)))))
                                          (make-instance 'bowl
                                                         :used T
                                                         :contents (list (make-instance 'olive-oil :amount
                                                                                        (make-instance 'amount
                                                                                                       :unit (make-instance 'g)
                                                                                                       :quantity (make-instance 'quantity
                                                                                                                                :value 500)))))
                                          
                                         (make-instance 'bowl
                                                        :used T
                                                         :contents (list (make-instance 'ground-black-pepper :amount
                                                                                        (make-instance 'amount
                                                                                                       :unit (make-instance 'g)
                                                                                                       :quantity (make-instance 'quantity
                                                                                                                                :value 500)))))
                                        (make-instance 'bowl
                                                       :used T
                                                         :contents (list (make-instance 'shallot :amount
                                                                                        (make-instance 'amount
                                                                                                       :unit (make-instance 'piece)
                                                                                                       :quantity (make-instance 'quantity
                                                                                                                                :value 5)))))
                                         (make-instance 'bowl
                                                        :used T
                                                         :contents (list (make-instance 'jalapeno :amount
                                                                                        (make-instance 'amount
                                                                                                       :unit (make-instance 'piece)
                                                                                                       :quantity (make-instance 'quantity
                                                                                                                                :value 5)))))
                                          (make-instance 'bowl
                                                         :used T
                                                         :contents (list (make-instance 'lime-juice :amount
                                                                                        (make-instance 'amount
                                                                                                       :unit (make-instance 'g)
                                                                                                       :quantity (make-instance 'quantity
                                                                                                                                :value 500)))))
                                         (make-instance 'bowl
                                                        :used T
                                                         :contents (list (make-instance 'salt :amount
                                                                                        (make-instance 'amount
                                                                                                       :unit (make-instance 'g)
                                                                                                       :quantity (make-instance 'quantity
                                                                                                                                :value 500))))))))))

;; 'make-html' makes an HTML representation of the kitchen state
;; and 'add-element' transfers that to the web interface
(add-element (make-html *initial-kitchen-state* :expand-initially t))


;; 'evaluate-irl-program' is used to evaluate an IRL program...
;; The first argument is the IRL program to evaluate, while the
;; second argument is the ontology. This is not yet used here.

;; Test seed and cut
(defparameter *o*
  (evaluate-irl-program `((to-get-kitchen ?kitchen)
                          (bind cherry-tomatoes ?cherry-tomatoes ,(make-instance 'cherry-tomatoes))
                          (bind knife ?knife ,(make-instance 'knife))
                          (to-fetch ?fetched-cherry-tomatoes ?kitchen-state-1 ?kitchen ?cherry-tomatoes)
                          (to-fetch ?vegetable-knife ?kitchen-state-2 ?kitchen-state-1 ?knife)
                          (to-seed ?seeded-cherry-tomatoes ?kitchen-state-3 ?kitchen-state-2 ?fetched-cherry-tomatoes ?vegetable-knife)                          
                          (to-cut ?cut-cherry-tomatoes ?kitchen-state-4 ?kitchen-state-3 ?seeded-cherry-tomatoes ,(make-instance 'sliced) ?vegetable-knife))
                        nil))

;; Test cover
(defparameter *o*
  (evaluate-irl-program `((to-get-kitchen ?kitchen)
                          (bind bowl ?bowl ,(make-instance 'bowl))
                          (bind bowl-lid ?bowl-lid ,(make-instance 'bowl-lid))
                          
                          (to-fetch ?fetched-bowl ?kitchen-state-1 ?kitchen ?bowl)
                          (to-fetch ?fetched-bowl-lid ?kitchen-state-2 ?kitchen-state-1 ?bowl-lid)
                          
                          (to-cover ?covered-bowl ?kitchen-state-3 ?kitchen-state-2 ?fetched-bowl ?fetched-bowl-lid)
                          (to-uncover ?uncovered-bowl ?kitchen-state-4 ?kitchen-state-3 ?covered-bowl))
                        nil))

;; #########################################################################
;; https://www.allrecipes.com/recipe/234759/easy-cherry-tomato-corn-salad/
;; #########################################################################

 (setf *easy-cherry-tomato-corn-salad*
  '(;; Initial Kitchen State
    (to-get-kitchen ?kitchen-state)
    ;; "1/4 cup minced fresh basil"
    (fetch-and-proportion ?proportioned-basil ?kitchen-state-with-basil ?kitchen-state fresh-basil 5 g)
    (cut ?minced-basil ?kitchen-state-with-cut-basil ?kitchen-state-with-basil ?proportioned-basil minced)
    ;; "3 tablespoons olive oil"
    (fetch-and-proportion ?olive-oil ?kitchen-state-with-olive-oil ?kitchen-state-with-cut-basil olive-oil 40 g)
    ;; "2 teaspoons lime juice"
    (fetch-and-proportion ?lime-juice ?kitchen-state-with-lime-juice ?kitchen-state-with-olive-oil lime-juice 9 g)
    ;; "1 teaspoon white sugar"
    (fetch-and-proportion ?white-sugar ?kitchen-state-with-white-sugar ?kitchen-state-with-lime-juice white-sugar 4 g)
    ;; "1/2 teaspoon salt"
    (fetch-and-proportion ?salt ?kitchen-state-with-salt ?kitchen-state-with-white-sugar salt 3 g)
    ;; "1/4 teaspoon ground black pepper"
    (fetch-and-proportion ?pepper ?kitchen-state-with-pepper ?kitchen-state-with-salt ground-black-pepper 1.5 g)
    ;; "2 cups frozen corn, thawed"
    (fetch-and-proportion ?frozen-corn ?kitchen-state-with-frozen-corn ?kitchen-state-with-pepper frozen-corn 350 g)
    (bring-up-to-room-temperature ?thawed-corn ?kitchen-state-with-thawed-corn ?kitchen-state-with-frozen-corn ?frozen-corn)
    ;; is a macro of bring-up-to-temperature
    ;; (thaw ?thawed-corn ?kitchen-state-with-thawed-corn ?kitchen-state-with-frozen-corn ?frozen-corn)
    ;; "2 cups cherry tomatoes, halved"
    (fetch-and-proportion ?cherry-tomatoes ?kitchen-state-with-cherry-tomatoes ?kitchen-state-with-thawed-corn cherry-tomato 300 g)
    (cut ?cut-tomatoes ?kitchen-state-with-cut-tomatoes ?kitchen-state-with-cherry-tomatoes ?cherry-tomatoes halved)
    ;; "1 cup peeled, seeded, and chopped cucumber"
    (fetch-and-proportion ?cucumber ?kitchen-state-with-cucumber ?kitchen-state-with-cut-tomatoes cucumber 160 g)
    (bind-and-fetch ?knife ?kitchen-state-with-knife ?kitchen-state-with-cucumber knife)
    (to-peel ?peeled-cucumber ?kitchen-state-with-peeled-cucumber ?kitchen-state-with-knife ?cucumber ?knife)
    (to-seed ?seeded-cucumber ?kitchen-state-with-seeded-cucumber ?kitchen-state-with-peeled-cucumber ?peeled-cucumber ?knife)
    (cut ?chopped-cucumber ?kitchen-state-with-chopped-cucumber ?kitchen-state-with-seeded-cucumber ?seeded-cucumber slices)
    ;; "1 jalapeno pepper, seeded and chopped"
    (fetch-and-proportion ?jalapeno ?kitchen-state-with-jalapeno  ?kitchen-state-with-chopped-cucumber jalapeno 1 piece)
    (to-seed ?seeded-jalapeno ?kitchen-state-with-seeded-jalapeno ?kitchen-state-with-jalapeno ?jalapeno ?knife)
    (cut ?chopped-jalapeno ?kitchen-state-with-chopped-jalapeno ?kitchen-state-with-seeded-jalapeno ?seeded-jalapeno slices)
    ;; "2 shallots, minced"
    (fetch-and-proportion ?shallot ?kitchen-state-with-shallot ?kitchen-state-with-chopped-jalapeno shallot 2 piece)
    (cut ?cut-shallot ?kitchen-state-with-cut-shallot ?kitchen-state-with-shallot ?shallot minced)
    ;; "Combine basil, olive oil, lime juice, sugar, salt, and pepper in a jar and screw on the lid"
    (bind-and-fetch ?open-jar ?kitchen-state-with-jar ?kitchen-state-with-cut-shallot jar)
    (combine-homogeneous ?mixture-jar ?kitchen-state-with-mixture-jar ?kitchen-state-with-jar ?open-jar ?minced-basil ?olive-oil ?lime-juice ?white-sugar ?salt ?pepper)
    (bind-and-fetch ?jar-lid ?kitchen-state-with-jar-lid ?kitchen-state-with-mixture-jar jar-lid)
    (to-cover ?closed-jar ?kitchen-state-with-closed-jar ?kitchen-state-with-jar-lid ?mixture-jar ?jar-lid)
    ;; "Shake until dressing is completely blended."
    (to-shake ?dressing ?kitchen-state-with-dressing ?kitchen-state-with-closed-jar ?closed-jar)
    ;; "Stir corn, tomatoes, cucumber, jalapeno, and shallots together in a bowl"
    (bind-and-fetch ?medium-bowl ?kitchen-state-with-medium-bowl ?kitchen-state-with-dressing medium-bowl)
    (combine-heterogeneous ?bowl-with-mixture ?kitchen-state-with-mixture-bowl ?kitchen-state-with-medium-bowl ?medium-bowl ?thawed-corn ?cut-tomatoes ?chopped-cucumber ?chopped-jalapeno ?cut-shallot)
    ;; "Drizzle dressing over corn mixture and toss to coat."
    (to-uncover ?open-dressing ?kitchen-state-with-open-dressing ?kitchen-state-with-mixture-bowl ?dressing)
    ;(to-sprinkle ?salad ?kitchen-state-with-salad ?kitchen-state-with-open-dressing ?bowl-with-mixture ?open-dressing)
    ;(bind-and-fetch ?wooden-spoon ?kitchen-state-with-wooden-spoon ?kitchen-state-with-salad wooden-spoon)
    ;(to-mingle ?tossed-salad ?kitchen-state-with-tossed-salad ?kitchen-state-with-wooden-spoon ?salad ?wooden-spoon)
    ;; " Refrigerate until serving."
    ;(to-get-fridge ?fridge ?kitchen-state-with-tossed-salad)
    ;(to-transfer ?fridge-with-salad ?cold-salad ?kitchen-with-cold-salad ?kitchen-state-with-tossed-salad ?tossed-salad ?fridge)
    ))
(evaluate-irl-program (expand-macros *easy-cherry-tomato-corn-salad*) nil)
;; Inspecting small parts of the recipe:
;;--------------------------------------


;; printing to the output buffer
(pprint `((bind ?kitchen-state *kitchen-state-1*)
          ,@(fetch-and-proportion ?proportioned-butter ?kitchen-state-with-butter *butter-1* ?kitchen-state 113 g)
          (melt ?melted-butter ?kitchen-state-with-melted-butter ?proportioned-butter ?kitchen-state-with-butter)))

;; drawing a network
(draw-recipe '((bind ?kitchen-state *kitchen-state-1*)
               (fetch-and-proportion ?proportioned-butter ?kitchen-state-with-butter *butter-1* ?kitchen-state 113 g)
               (melt ?melted-butter ?kitchen-state-with-melted-butter ?proportioned-butter ?kitchen-state-with-butter))
             :expand nil) ;;t or nil


;; ======================
;; Visualise the recipe
;; ======================

;; High-level recipe notation:
(draw-recipe *cherry-tomato-corn-salad* :expand nil)

;; All primitives expanded:
(draw-recipe *cherry-tomato-corn-salad* :expand t)
