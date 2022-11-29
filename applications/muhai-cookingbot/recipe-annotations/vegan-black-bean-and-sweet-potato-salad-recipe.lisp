(ql:quickload :muhai-cookingbot)

(in-package :muhai-cookingbot)

;; The 'trace-irl' monitor will make sure that
;; the IRL evaluation process is shown on the web
;; interface (which can be found at localhost:8000).
;; We need to activate it:
;(activate-monitor trace-irl)

;; ##################################################################
;; Vegan Black Bean and Sweet Potato Salad
;; https://www.allrecipes.com/recipe/238162/vegan-black-bean-and-sweet-potato-salad/
;; ##################################################################

;; Defining the initial kitchen state
(defparameter *initial-kitchen-state* 
  (make-instance 
   'kitchen-state
   :contents
   (list (make-instance 'fridge
                        :contents (list  (make-instance 'bowl
                                                        :used T
                                                        :contents (list (make-instance 'onion :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'piece)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 10)))))))
         (make-instance 'pantry
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
                                                        :contents (list (make-instance 'coarse-salt :amount
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
                                   (make-instance 'knife) (make-instance 'knife) (make-instance 'knife)

                                   ;; baking equipment
                                   (make-instance 'cookie-sheet)
                                   (make-instance 'baking-tray)
                                   (make-instance 'baking-paper))))))

;; 'make-html' makes an HTML representation of the kitchen state
;; and 'add-element' transfers that to the web interface
;(add-element (make-html *initial-kitchen-state* :expand-initially t))

(defparameter *vegan-black-bean-and-sweet-potato-salad-recipe* 
  '((get-kitchen ?kitchen)
    
    ;; "450 grams sweet potatoes, peeled and cut into 2 cm cubes"
    (fetch-and-proportion ?proportioned-sweet-potatoes ?kitchen-state-with-sweet-potatoes ?kitchen ?target-container-1 sweet-potato 450 g)
    (peel ?peeled-sweet-potatoes ?sweet-potato-peels ?kitchen-state-with-peeled-sweet-potatoes ?kitchen-state-with-sweet-potatoes ?proportioned-sweet-potatoes ?knife)
    (cut ?sweet-potato-cubes ?kitchen-state-with-sweet-potato-cubes ?kitchen-state-with-peeled-sweet-potatoes ?peeled-sweet-potatoes two-cm-cubes ?knife) ;; use the same knife    
    
    ;; "3 tablespoons olive oil, divided"
    ; TODO RD: "divided" means that it will be used divided accross different parts of the recipe
    ;; IMPLICIT: "divided" means that there should be two separate portions that can be used later
    (fetch-and-proportion ?one-tablespoon-olive-oil ?kitchen-state-with-one-tablespoon-olive-oil ?kitchen-state-with-sweet-potato-cubes ?target-container-1-tablespoon olive-oil 1 tablespoon)
    (fetch-and-proportion ?two-tablespoons-olive-oil ?kitchen-state-with-two-tablespoons-olive-oil ?kitchen-state-with-one-tablespoon-olive-oil ?target-container-2-tablespoons olive-oil 2 tablespoon)

    ;; "1/2 teaspoon ground cumin"
    (fetch-and-proportion ?proportioned-ground-cumin ?kitchen-state-with-ground-cumin ?kitchen-state-with-two-tablespoons-olive-oil ?target-container-3 ground-cumin 0.5 tablespoon)
    
    ;; "1/4 teaspoon red pepper flakes"
    (fetch-and-proportion ?proportioned-red-pepper-flakes ?kitchen-state-with-red-pepper-flakes ?kitchen-state-with-ground-cumin ?target-container-4 red-pepper-flakes 0.25 teaspoon)
    
    ;; "1 gram coarse salt and ground black pepper"
    (fetch-and-proportion ?proportioned-salt ?kitchen-state-with-salt ?kitchen-state-with-red-pepper-flakes ?target-container-5 coarse-salt 1 g)
    (fetch-and-proportion ?proportioned-pepper ?kitchen-state-with-pepper ?kitchen-state-with-salt ?target-container-6 ground-black-pepper 1 g)                  

    ;; "2 tablespoons freshly squeezed lime juice"
    (fetch-and-proportion ?lime-juice ?kitchen-state-with-lime-juice ?kitchen-state-with-pepper ?target-container-7 lime-juice 2 tablespoon)

    ;; "400 grams black beans"
    (fetch-and-proportion ?proportioned-black-beans ?kitchen-state-with-black-beans ?kitchen-state-with-lime-juice ?target-container-8 black-bean 400 g)

    ; TODO RD: cut in half?
    ;; "1/2 red onion, finely chopped" -- cut
    (fetch-and-proportion ?proportioned-red-onion ?kitchen-state-with-red-onion ?kitchen-state-with-black-beans ?target-container-9 red-onion 0.5 piece)
    (cut ?finely-chopped-red-onion ?kitchen-state-with-finely-chopped-onion ?kitchen-state-with-red-onion ?proportioned-red-onion fine-slices ?knife) ;; use the same knife
            
    ;; "8 grams chopped fresh cilantro" -- cut
    (fetch-and-proportion ?proportioned-fresh-cilantro ?kitchen-state-with-fresh-cilantro ?kitchen-state-with-finely-chopped-onion ?target-container-10  fresh-cilantro 8 g)
    (cut ?chopped-fresh-cilantro ?kitchen-state-with-fresh-chopped-cilantro ?kitchen-state-with-fresh-cilantro ?proportioned-fresh-cilantro slices ?knife) ;; use the same knife
    
    ;; "Preheat oven to 230 degrees C."
    (preheat-oven ?preheated-oven ?kitchen-state-with-preheated-oven ?kitchen-state-with-fresh-chopped-cilantro ?oven 230 degrees-celsius)

    ; TODO RD: spread primitive? Not really I think.
    ;; "Spread sweet potatoes onto a rimmed baking sheet."
    (fetch ?baking-tray ?kitchen-state-with-baking-tray ?kitchen-state-with-preheated-oven baking-tray 1) ;; IMPLICIT

    (transfer-contents ?output-container-a ?rest-a ?output-kitchen-state-a ?kitchen-state-with-baking-tray ?baking-tray ?sweet-potato-cubes ?quantity-a ?unit-a)

    ; TODO RD: is drizzle sprinkle or transfer-contents?
    ;; "Drizzle 1 tablespoon olive oil over sweet potatoes;"
    (sprinkle ?drizzled-potatoes ?kitchen-state-with-drizzled-potatoes ?output-kitchen-state-a ?output-container-a ?one-tablespoon-olive-oil)

    ; TODO RD: season primitive?
    ;; "season with cumin, red pepper flakes, salt, and pepper."
    (transfer-contents ?output-container-b ?rest-b ?output-kitchen-state-b ?kitchen-state-with-drizzled-potatoes ?drizzled-potatoes ?proportioned-ground-cumin ?quantity-b ?unit-b)
    (transfer-contents ?output-container-c ?rest-c ?output-kitchen-state-c ?output-kitchen-state-b ?output-container-b ?proportioned-red-pepper-flakes ?quantity-c ?unit-c)
    (transfer-contents ?output-container-d ?rest-d ?output-kitchen-state-d ?output-kitchen-state-c ?output-container-c ?proportioned-salt ?quantity-d ?unit-d) 
    (transfer-contents ?output-container-e ?rest-e ?output-kitchen-state-e ?output-kitchen-state-d ?output-container-d ?proportioned-pepper ?quantity-e ?unit-e)

    ;; "Toss sweet potatoes until evenly coated."
    (mingle ?tossed-potatoes ?kitchen-state-with-tossed-potatoes ?output-kitchen-state-e ?output-container-e ?wooden-spoon)
    
    ;; "Roast in the preheated oven, until sweet potatoes are tender, 25 minutes."
    (bake ?baked-potatoes ?kitchen-state-with-baked-potatoes ?kitchen-state-with-tossed-potatoes ?tossed-potatoes ?preheated-oven 25 minute ?bake-quantity ?bake-unit)
    
    ;; "Whisk remaining 2 tablespoons olive oil and lime juice together in a large bowl."
    (fetch ?large-bowl ?kitchen-state-with-fetched-large-bowl ?kitchen-state-with-baked-potatoes large-bowl 1) ;; IMPLICIT
    (fetch ?whisk ?kitchen-state-with-fetched-whisk ?kitchen-state-with-fetched-large-bowl whisk 1) ;; IMPLICIT
    (transfer-contents ?output-container-f ?rest-f ?output-kitchen-state-f ?kitchen-state-with-fetched-whisk ?large-bowl ?two-tablespoons-olive-oil ?quantity-f ?unit-f)
    (transfer-contents ?output-container-g ?rest-g ?output-kitchen-state-g ?output-kitchen-state-f ?output-container-f ?lime-juice ?quantity-g ?unit-g)
    (mix ?whisked-mixture ?kitchen-state-with-whisked-mixture ?output-kitchen-state-g ?output-container-g ?whisk)

    ;; "Add sweet potatoes, black beans, onion, and cilantro;"
    (transfer-contents ?output-container-h ?rest-h ?output-kitchen-state-h ?output-kitchen-state-g ?whisked-mixture ?baked-potatoes ?quantity-h ?unit-h)
    (transfer-contents ?output-container-i ?rest-i ?output-kitchen-state-i ?output-kitchen-state-h ?output-container-h ?proportioned-black-beans ?quantity-i ?unit-i)

    (transfer-contents ?output-container-j ?rest-j ?output-kitchen-state-j ?output-kitchen-state-i ?output-container-i ?finely-chopped-red-onion ?quantity-j ?unit-j)
    (transfer-contents ?output-container-k ?rest-k ?output-kitchen-state-k ?output-kitchen-state-j ?output-container-j ?proportioned-fresh-cilantro ?quantity-k ?unit-k)

    ;; "gently toss to coat."
    (mingle ?salad ?kitchen-state-with-salad ?output-kitchen-state-k ?output-container-k ?wooden-spoon))) ;; use the same wooden spoon


;; ======================
;; Append bindings to the recipe
;; ======================

(defparameter *extended-recipe*
  (append-meaning-and-irl-bindings *vegan-black-bean-and-sweet-potato-salad-recipe* nil))

;; ======================
;; Evaluate the recipe
;; ======================

;(activate-monitor trace-irl)

;(clear-output)

;(evaluate-irl-program *extended-recipe* nil)

;; ======================
;; Visualise the recipe
;; ======================

;(draw-recipe *vegan-black-bean-and-sweet-potato-salad-recipe*)
;(draw-recipe *extended-recipe*)