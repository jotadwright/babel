(ql:quickload :aipp-cookingbot)
(in-package :aipp-cookingbot)

;; ##################################################################
;; Cucumber slices with dill recipe
;; https://www.allrecipes.com/recipe/18304/cucumber-slices-with-dill/
;; ##################################################################

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

   ,(make-instance 'pantry
                          :contents (list
                                     
                                     (make-instance 'knife)
                                     (make-instance 'knife)
                                     (make-instance 'knife)
                                     (make-instance 'knife)
                                     (make-instance 'knife)
                                     (make-instance 'medium-bowl)
                                     (make-instance 'medium-bowl)
                                     (make-instance 'medium-bowl)
                                     (make-instance 'medium-bowl)
                                     (make-instance 'medium-bowl)
                                     (make-instance 'medium-bowl)
                                     (make-instance 'medium-bowl)
                                     (make-instance 'medium-bowl)
                                     (make-instance 'medium-bowl)
                                     (make-instance 'medium-bowl)
                                     (make-instance 'medium-bowl)
                                     (make-instance 'medium-bowl)
                                     (make-instance 'medium-bowl)
                                     (make-instance 'medium-bowl)
                                     (make-instance 'medium-bowl)
                                     (make-instance 'medium-bowl)
                                     (make-instance 'medium-bowl-lid)
                                     (make-instance 'medium-bowl-lid)
                                     (make-instance 'large-bowl)
                                     (make-instance 'large-bowl)
                                     (make-instance 'large-bowl)
                                     (make-instance 'large-bowl)
                                     (make-instance 'large-bowl-lid)
                                     (make-instance 'large-bowl-lid)
                                     (make-instance 'whisk)
                                     (make-instance 'whisk)
                                     (make-instance 'whisk)
                                     (make-instance 'wooden-spoon)
                                     (make-instance 'wooden-spoon)
                                     (make-instance 'wooden-spoon)))


   ,(make-instance 'kitchen-cabinet
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
                                                                                                                                :value 500))))))))))

;; 'make-html' makes an HTML representation of the kitchen state
;; and 'add-element' transfers that to the web interface
(add-element (make-html *initial-kitchen-state* :expand-initially t))

(setf *cucumber-slices-with-dill* 
  `((to-get-kitchen ?kitchen)
    
    (fetch-and-proportion ?proportioned-cucumbers ?kitchen-state-with-cucumbers ?kitchen cucumber 4 piece)
    
    (cut ?sliced-cucumbers ?kitchen-state-with-sliced-cucumbers ?kitchen-state-with-cucumbers ?proportioned-cucumbers slices)
    
    (fetch-and-proportion ?proportioned-onions ?kitchen-state-with-onions ?kitchen-state-with-sliced-cucumbers onion 1 piece)
    
    (cut ?sliced-onions ?kitchen-state-with-sliced-onions ?kitchen-state-with-onions ?proportioned-onions slices)
    
    (fetch-and-proportion ?proportioned-dill-weed ?kitchen-state-with-dill-weed ?kitchen-state-with-sliced-onions dried-dill-weed 15 g)
    
    (fetch-and-proportion ?proportioned-white-sugar ?kitchen-state-with-white-sugar ?kitchen-state-with-dill-weed white-sugar 227 g)
    
    (fetch-and-proportion ?proportioned-white-vinegar ?kitchen-state-with-white-vinegar ?kitchen-state-with-white-sugar white-vinegar 113.5 g)
    
    (fetch-and-proportion ?proportioned-water ?kitchen-state-with-water ?kitchen-state-with-white-vinegar water 113.5 g)
    
    (fetch-and-proportion ?proportioned-salt ?kitchen-state-with-salt ?kitchen-state-with-water salt 5 g)
    
    (bind-and-fetch ?large-bowl ?kitchen-state-with-large-bowl ?kitchen-state-with-salt large-bowl)

    (combine-heterogeneous ?cucumber-bowl ?kitchen-state-with-cucumber-bowl ?kitchen-state-with-large-bowl ?large-bowl ?sliced-cucumbers ?sliced-onions ?proportioned-dill-weed)
    
    (bind-and-fetch ?medium-bowl ?kitchen-state-with-medium-bowl ?kitchen-state-with-cucumber-bowl medium-bowl)
    
    (combine-homogeneous ?vinegar-bowl ?kitchen-state-with-vinegar-bowl ?kitchen-state-with-medium-bowl ?medium-bowl ?proportioned-white-vinegar ?proportioned-white-sugar ?proportioned-water ?proportioned-salt)
    
    (transfer-all-contents ?salad-bowl ?kitchen-state-with-mixture ?kitchen-state-with-vinegar-bowl ?cucumber-bowl ?vinegar-bowl)
    
    (bind-and-fetch ?bowl-lid ?kitchen-state-with-bowl-lid ?kitchen-state-with-mixture large-bowl-lid)
    
    (to-cover ?covered-salad-bowl ?kitchen-state-with-covered-salad-bowl ?kitchen-state-with-bowl-lid ?salad-bowl ?bowl-lid)

    (to-get-fridge ?fridge ?kitchen-state-with-covered-salad-bowl)
    
    (to-transfer ?fridge-with-covered-salad-bowl ?salad-bowl-in-fridge ?kitchen-state-with-salad-bowl-in-fridge ?kitchen-state-with-covered-salad-bowl ?covered-salad-bowl ?fridge)

    ;; Wait 2 hours
    
    (to-fetch ?refrigerated-salad-bowl ?kitchen-state-with-refrigerated-salad-bowl ?kitchen-state-with-salad-bowl-in-fridge ?salad-bowl-in-fridge)
    ))

(evaluate-irl-program (expand-macros *cucumber-slices-with-dill*) nil)


;; ======================
;; Visualise the recipe
;; ======================

;; High-level recipe notation:
(draw-recipe *cucumber-slices-with-dill-recipe* :expand nil)

;; All primitives expanded:
(draw-recipe *cucumber-slices-with-dill-recipe* :expand t)

