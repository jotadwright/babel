;(ql:quickload :muhai-cookingbot)
(in-package :muhai-cookingbot)

(activate-monitor trace-irl)

;; ##################################################################
;; Easy Banana Bread
;; https://www.cooks.com/recipe/il0tt9uq/easy-banana-bread.html
;; ##################################################################

(defparameter *initial-kitchen-state* 
  (make-instance 
   'kitchen-state
   :contents
   (list (make-instance 'fridge
                        :contents (list (make-instance 'medium-bowl
                                                       :contents (list (make-instance 'butter
                                                                                      :temperature
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'degrees-celsius)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 5))
                                                                                      :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 500)))))
                                        (make-instance 'medium-bowl
                                                       :contents (list (make-instance 'egg
                                                                                      :temperature
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'degrees-celsius)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 5))
                                                                                      :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'piece)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 12)))))))
         (make-instance 'pantry
                        :contents (list (make-instance 'medium-bowl
                                                       :contents (list (make-instance 'white-sugar :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 1000)))))
                                        (make-instance 'medium-bowl
                                                       :contents (list (make-instance 'banana :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'piece)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 6)))))
                                        (make-instance 'medium-bowl
                                                       :contents (list (make-instance 'vanilla-extract :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 100)))))
                                        (make-instance 'medium-bowl
                                                       :contents (list (make-instance 'self-rising-flour :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 1000)))))
                                        (make-instance 'medium-bowl
                                                       :contents (list (make-instance 'all-purpose-flour :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 1000)))))
                                        
                                        (make-instance 'medium-bowl
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

                                   ;; tools
                                   (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                   (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                   (make-instance 'fork) (make-instance 'whisk) (make-instance 'whisk)
                                   (make-instance 'spatula) (make-instance 'knife)

                                   ;; baking equipment
                                   (make-instance 'baking-tray)
                                   (make-instance 'pan)
                                   (make-instance 'baking-paper))))))

(add-element (make-html *initial-kitchen-state* :expand-initially t))

(defparameter *easy-banana-bread-recipe*
  '((get-kitchen ?kitchen-state)

    ;; Ingredients

    ;; 1/4 cup butter
    (fetch-and-proportion ?proportioned-butter ?kitchen-state-with-butter ?kitchen-state ?target-container-1 butter 60 g)
    (melt ?melted-butter ?kitchen-state-with-melted-butter ?kitchen-state-with-butter ?proportioned-butter)

    ;; 2 eggs
    (fetch-and-proportion ?proportioned-eggs ?kitchen-state-with-eggs ?kitchen-state-with-melted-butter ?target-container-2 egg 2 piece)

    ;; 1 cup sugar
    (fetch-and-proportion ?proportioned-sugar ?kitchen-state-with-sugar ?kitchen-state-with-eggs ?target-container-3 sugar 200 g) ;;white sugar?

    ;; 3 bananas, mashed
    (fetch-and-proportion ?proportioned-bananas ?kitchen-state-with-bananas ?kitchen-state-with-sugar ?target-container-4 banana 3 piece)
    (mash ?mashed-bananas ?kitchen-state-with-mashed-bananas ?kitchen-state-with-bananas ?proportioned-bananas ?fork)

    ;; 1 tsp. vanilla
    (fetch-and-proportion ?proportioned-vanilla ?kitchen-state-with-vanilla ?kitchen-state-with-mashed-bananas ?target-container-5 vanilla-extract 4 g)

    ;; 1 1/2 cups self-rising flour
    (fetch-and-proportion ?proportioned-self-rising-flour ?kitchen-state-with-self-rising-flour ?kitchen-state-with-vanilla ?target-container-6 self-rising-flour 200 g)

    ;; Directions

    ;; IMPLICIT: "Crack eggs."
   ;; (crack ?cracked-eggs ?kitchen-state-with-cracked-eggs ?kitchen-state-with-self-rising-flour ?proportioned-eggs ?target-container-7)
    
    ;; "Cream together butter, eggs and sugar until smooth."
    (transfer-contents ?output-container-x ?rest-x ?output-kitchen-state-x ?proportioned-self-rising-flour ?target-container-8 ?melted-butter ?quantity-x ?unit-x)
    (transfer-contents ?output-container-y ?rest-y ?output-kitchen-state-y ?output-kitchen-state-x ?output-container-x ?proportioned-eggs ?quantity-y ?unit-y)
    (transfer-contents ?output-container-z ?rest-z ?output-kitchen-state-z ?output-kitchen-state-y ?output-container-y ?proportioned-sugar ?quantity-z ?unit-z)
    (beat ?creamed-mixture ?kitchen-state-with-creamed-mixture ?output-kitchen-state-z ?output-container-z ?beating-tool)
    
    ;; "Add bananas and vanilla; beat well."
    (transfer-contents ?output-container-a ?rest-a ?output-kitchen-state-a ?kitchen-state-with-creamed-mixture ?creamed-mixture ?mashed-bananas ?quantity-a ?unit-a)
    (transfer-contents ?output-container-b ?rest-b ?output-kitchen-state-b ?output-kitchen-state-a ?output-container-a ?proportioned-vanilla ?quantity-b ?unit-b)
    (beat ?beaten-mixture ?kitchen-state-with-beaten-mixture ?output-kitchen-state-b ?output-container-b ?beating-tool) ;;reuse the same whisk

    ;; "Mix in flour."
     (transfer-contents ?output-container-c ?rest-c ?output-kitchen-state-c ?kitchen-state-with-beaten-mixture ?beaten-mixture ?proportioned-self-rising-flour ?quantity-c ?unit-c)
    (mix ?banana-bread-batter ?kitchen-state-with-banana-bread-batter ?output-kitchen-state-c ?output-container-c ?beating-tool) ;;reusing the same whisk
 
    ;; IMPLICIT: "Grease a pan."
 #|  (bind-and-fetch ?pan ?kitchen-state-with-pan 
		    ?kitchen-state-with-banana-bread-batter pan)
    (grease ?greased-pan ?kitchen-state-with-greased-pan 
	    ?kitchen-state-with-pan ?pan butter)

    ;; IMPLICIT: "Transfer batter into pan and spread evenly."
    (bind-and-fetch ?spatula ?kitchen-state-with-spatula 
		    ?kitchen-state-with-greased-pan spatula)
    (to-spread ?pan-with-spread-batter ?kitchen-state-with-spread-batter-in-pan
	       ?kitchen-state-with-spatula ?greased-pan ?banana-bread-batter ?spatula) |#

    ;; "Bake at 325 degrees Fahrenheit for about 1 hour or until golden brown."
    (bake ?baked-banana-bread ?kitchen-state-with-baked-banana-bread ?kitchen-state-with-banana-bread-batter ?banana-bread-batter ?oven 60 minute 160 degrees-celsius)

    ))

;; ======================
;; Append bindings to the recipe
;; ======================

(defparameter *extended-recipe*
  (append-meaning-and-irl-bindings *easy-banana-bread-recipe* nil))

;; ======================
;; Evaluate the recipe
;; ======================

;(activate-monitor trace-irl)
;(evaluate-irl-program *extended-recipe* nil)

;; ======================
;; Visualise the recipe
;; ======================

;(draw-recipe *brownie-recipe*)


