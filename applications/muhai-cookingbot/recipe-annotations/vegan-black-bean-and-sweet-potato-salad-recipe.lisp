(ql:quickload :aipp-cookingbot)
(in-package :aipp-cookingbot)

;; The 'trace-irl' monitor will make sure that
;; the IRL evaluation process is shown on the web
;; interface (which can be found at localhost:8000).
;; We need to activate it:
(activate-monitor trace-irl)

;; ##################################################################
;; Vegan Black Bean and Sweet Potato Salad
;; https://www.allrecipes.com/recipe/238162/vegan-black-bean-and-sweet-potato-salad/
;; ##################################################################

;; Defining the initial kitchen state
(defparameter *initial-kitchen-state* 
  (make-instance 
   'kitchen-state
   :id 'kitchen-state
   :contents
   `(,(make-instance 'fridge
                     :contents (list  (make-instance 'bowl
                                                     :used T
                                                     :contents (list (make-instance 'onion :amount
                                                                                    (make-instance 'amount
                                                                                                   :unit (make-instance 'piece)
                                                                                                   :quantity (make-instance 'quantity
                                                                                                                            :value 10)))))))
     ,(make-instance 'pantry
                     :contents (list  (make-instance 'bowl
                                                     :used T
                                                     :contents (list (make-instance 'sweet-potato :amount
                                                                                    (make-instance 'amount
                                                                                                   :unit (make-instance 'g)
                                                                                                   :quantity (make-instance 'quantity
                                                                                                                            :value 500)))))
                                      (make-instance 'bowl
                                                     :used T
                                                     :contents (list (make-instance 'black-bean :amount
                                                                                    (make-instance 'amount
                                                                                                   :unit (make-instance 'g)
                                                                                                   :quantity (make-instance 'quantity
                                                                                                                            :value 500)))))
                                      (make-instance 'bowl
                                                     :used T
                                                     :contents (list (make-instance 'red-onion :amount
                                                                                    (make-instance 'amount
                                                                                                   :unit (make-instance 'piece)
                                                                                                   :quantity (make-instance 'quantity
                                                                                                                            :value 5)))))                                          
                                      (make-instance 'bowl
                                                     :used T
                                                     :contents (list (make-instance 'olive-oil :amount
                                                                                    (make-instance 'amount
                                                                                                   :unit (make-instance 'g)
                                                                                                   :quantity (make-instance 'quantity
                                                                                                                            :value 50)))))                                         
                                      (make-instance 'bowl
                                                     :used T
                                                     :contents (list (make-instance 'ground-black-pepper :amount
                                                                                    (make-instance 'amount
                                                                                                   :unit (make-instance 'g)
                                                                                                   :quantity (make-instance 'quantity
                                                                                                                            :value 500)))))
                                      (make-instance 'bowl
                                                     :used T
                                                     :contents (list (make-instance 'ground-cumin :amount
                                                                                    (make-instance 'amount
                                                                                                   :unit (make-instance 'g)
                                                                                                   :quantity (make-instance 'quantity
                                                                                                                            :value 500)))))
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
                                                                                                                            :value 500)))))
                                      (make-instance 'bowl
                                                     :used T
                                                     :contents (list (make-instance 'fresh-cilantro :amount
                                                                                    (make-instance 'amount
                                                                                                   :unit (make-instance 'g)
                                                                                                   :quantity (make-instance 'quantity
                                                                                                                            :value 50)))))
                                      (make-instance 'bowl
                                                     :used T
                                                     :contents (list (make-instance 'red-pepper-flakes :amount
                                                                                    (make-instance 'amount
                                                                                                   :unit (make-instance 'g)
                                                                                                   :quantity (make-instance 'quantity
                                                                                                                            :value 50)))))))
     ,(make-instance 'kitchen-cabinet
                     :contents (list 
                                (make-instance 'knife) (make-instance 'knife) (make-instance 'knife) (make-instance 'knife) (make-instance 'knife)
                                (make-instance 'bowl) (make-instance 'bowl) (make-instance 'bowl) (make-instance 'bowl) (make-instance 'bowl) (make-instance 'bowl)
                                (make-instance 'bowl) (make-instance 'bowl) (make-instance 'bowl) (make-instance 'bowl) (make-instance 'bowl) (make-instance 'bowl)
                                     
                                (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                
                                (make-instance 'medium-bowl-lid) (make-instance 'medium-bowl-lid)
                                (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)
                                (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)
                                (make-instance 'baking-tray)(make-instance 'baking-tray)(make-instance 'baking-tray)(make-instance 'baking-tray)
                                
                                (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)(make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                (make-instance 'wooden-spoon)(make-instance 'wooden-spoon)(make-instance 'wooden-spoon)(make-instance 'wooden-spoon))))))

(setf *vegan-black-bean-and-sweet-potato-salad-recipe* 
  '(;; Initial Kitchen State
    (to-get-kitchen ?kitchen-state)
    
    ;; "1 pound sweet potatoes, peeled and cut into 3/4-inch cubes"
    (fetch-and-proportion ?proportioned-sweet-potatoes ?kitchen-state-with-sweet-potatoes ?kitchen-state sweet-potato 450 g)

    (peel ?peeled-sweet-potatoes ?kitchen-state-with-peeled-sweet-potatoes ?kitchen-state-with-sweet-potatoes ?proportioned-sweet-potatoes)

    (cut ?sweet-potato-cubes ?kitchen-state-with-sweet-potato-cubes ?kitchen-state-with-peeled-sweet-potatoes ?peeled-sweet-potatoes three-quarter-inch-cubes)
    
    
    ;; "3 tablespoons olive oil, divided (means it will be used divided accross different parts of the recipe"
    (fetch-and-proportion ?one-tablespoon-olive-oil ?kitchen-state-with-one-tablespoon-olive-oil ?kitchen-state-with-sweet-potato-cubes olive-oil 13 g)
    (fetch-and-proportion ?two-tablespoons-olive-oil ?kitchen-state-with-two-tablespoons-olive-oil ?kitchen-state-with-one-tablespoon-olive-oil olive-oil 26 g)

    ;; "1/2 teaspoon ground cumin, or more to taste"
    (fetch-and-proportion ?proportioned-ground-cumin ?kitchen-state-with-ground-cumin ?kitchen-state-with-two-tablespoons-olive-oil ground-cumin 1 g)
    
    ;; "1/4 teaspoon red pepper flakes (Optional)"
    (fetch-and-proportion ?proportioned-red-pepper-flakes ?kitchen-state-with-red-pepper-flakes ?kitchen-state-with-ground-cumin red-pepper-flakes 0.4 g)
    
    ;; "coarse salt and ground black pepper to taste"
    (fetch-and-proportion ?proportioned-salt ?kitchen-state-with-salt ?kitchen-state-with-red-pepper-flakes salt 0.5 g)
    (fetch-and-proportion ?proportioned-pepper ?kitchen-state-with-pepper ?kitchen-state-with-salt ground-black-pepper 0.5 g)                    

    ;; "2 tablespoons freshly squeezed lime juice"
    (fetch-and-proportion ?lime-juice ?kitchen-state-with-lime-juice ?kitchen-state-with-pepper lime-juice 30 g)

    ;; "14.5 ounce black beans"
    (fetch-and-proportion ?proportioned-black-beans ?kitchen-state-with-black-beans ?kitchen-state-with-lime-juice black-bean 400 g)
                          
    ;; "1/2 red onion, finely chopped" -- cut
    (fetch-and-proportion ?proportioned-red-onion ?kitchen-state-with-red-onion ?kitchen-state-with-black-beans red-onion 0.5 piece)
    (cut ?finely-chopped-red-onion ?kitchen-state-with-finely-chopped-onion ?kitchen-state-with-red-onion ?proportioned-red-onion finely-chopped)
            
    ;; "1/2 cup chopped fresh cilantro" -- cut
    (fetch-and-proportion ?proportioned-fresh-cilantro ?kitchen-state-with-fresh-cilantro ?kitchen-state-with-finely-chopped-onion fresh-cilantro 8 g)
    (cut ?chopped-fresh-cilantro ?kitchen-state-with-fresh-chopped-cilantro ?kitchen-state-with-fresh-cilantro ?proportioned-fresh-cilantro chopped)
    
    ;; "Preheat oven to 450 degrees F (230 degrees C)"
    (preheat-oven ?preheated-oven ?kitchen-state-with-preheated-oven ?kitchen-state-with-fresh-chopped-cilantro 230 degrees-celsius)

    ;; "Spread sweet potatoes onto a rimmed baking sheet"
    (bind-and-fetch ?sheet ?kitchen-state-with-sheet ?kitchen-state-with-preheated-oven baking-tray)


    (transfer-all-contents ?sweet-potatoes-on-sheet ?kitchen-state-with-sweet-potatoes-on-sheet ?kitchen-state-with-sheet ?sheet ?sweet-potato-cubes)

    ;; "Drizzle 1 tablespoon olive oil over sweet potatoes; season with cumin, red pepper flakes, salt, and pepper and toss sweet potatoes until evenly coated"
    (combine-heterogeneous  ?seasoned-sweet-potatoes-on-sheet ?kitchen-state-with-seasoned-sweet-potatoes-on-sheet ?kitchen-state-with-sweet-potatoes-on-sheet ?sweet-potatoes-on-sheet
                            ?one-tablespoon-olive-oil
                            ?proportioned-ground-cumin
                            ?proportioned-red-pepper-flakes
                            ?proportioned-salt
                            ?proportioned-pepper)

    
    ;; "Roast on the lower rack of the preheated oven, until sweet potatoes are tender, 25 to 35 minutes"
    (to-transfer ?oven-with-sheet ?sheet-in-oven ?kitchen-state-with-sheet-in-oven ?kitchen-state-with-seasoned-sweet-potatoes-on-sheet ?seasoned-sweet-potatoes-on-sheet ?preheated-oven)
    
    (define-amount ?time-to-bake 30 minute)
    
    (to-bake ?done-sweet-potatoes ?kitchen-state-with-done-sweet-potatoes-in-oven ?kitchen-state-with-sheet-in-oven ?sheet-in-oven ?time-to-bake)
    
    (to-fetch ?fetched-done-sweet-potatoes ?kitchen-state-with-baked-potatoes-on-counter ?kitchen-state-with-done-sweet-potatoes-in-oven ?done-sweet-potatoes)

    
    ;; "Whisk remaining 2 tablespoons olive oil and lime juice together in a large bowl; season with salt and pepper"
    (bind-and-fetch ?large-bowl ?kitchen-state-with-large-bowl ?kitchen-state-with-baked-potatoes-on-counter large-bowl)
 
    (combine-homogeneous ?oil-juice-mix ?kitchen-state-with-oil-juice-mix ?kitchen-state-with-large-bowl ?large-bowl
                         ?two-tablespoons-olive-oil
                         ?lime-juice) 

    
    ;; "Add sweet potatoes, black beans, onion, and cilantro; gently toss to coat"
    (bind-and-fetch ?large-bowl2 ?kitchen-state-with-large-bowl2 ?kitchen-state-with-oil-juice-mix large-bowl)
    
    (combine-heterogeneous ?completed-salad ?kitchen-state-with-everything-in-large-bowl ?kitchen-state-with-large-bowl2 ?large-bowl2
                           ?proportioned-black-beans
                           ?finely-chopped-red-onion
                           ?chopped-fresh-cilantro
                           ?fetched-done-sweet-potatoes
                           ?oil-juice-mix)))


(evaluate-irl-program (expand-macros *vegan-black-bean-and-sweet-potato-salad-recipe*) nil)
;; ======================
;; Visualise the recipe
;; ======================

;; High-level recipe notation:
(draw-recipe *vegan-black-bean-and-sweet-potato-salad-recipe* :expand nil)

;; All primitives expanded:
(draw-recipe *vegan-black-bean-and-sweet-potato-salad-recipe* :expand t)

