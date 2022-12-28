(ql:quickload :muhai-cookingbot)

(in-package :muhai-cookingbot)

;; The 'trace-irl' monitor will make sure that
;; the IRL evaluation process is shown on the web
;; interface (which can be found at localhost:8000).
;; We need to activate it:
;(activate-monitor trace-irl)

;; #############################################################################
;; Black Bean and Mango Salad
;; https://www.thespruceeats.com/black-bean-and-mango-salad-recipe-3378314
;; #############################################################################

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
                                                        :contents (list (make-instance 'red-bell-pepper :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'piece)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 10)))))                                          
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
                                                        :contents (list (make-instance 'jalapeno :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'piece)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 5)))))

                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'salt :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 500)))))
                                         (make-instance 'medium-bowl
                                                        :used T
                                                        :contents (list (make-instance 'green-onion :amount
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
                                                        :contents (list (make-instance 'mango :amount
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
    
    ;; "400 grams black beans, rinsed"
    (fetch-and-proportion ?proportioned-black-beans ?ks-with-black-beans ?kitchen ?target-container-1 black-bean 400 g)
    (wash ?washed-black-beans ?ks-with-washed-black-beans ?ks-with-black-beans ?proportioned-black-beans)

    ;; "150 grams diced red bell pepper"
    (fetch-and-proportion ?proportioned-bell-pepper ?ks-with-bell-pepper ?ks-with-washed-black-beans ?target-container-2 red-bell-pepper 150 g)
   
    ;; IMPLICIT
    (seed ?seeded-bell-pepper ?bell-pepper-seeds ?ks-with-seeded-bell-pepper ?ks-with-bell-pepper ?proportioned-bell-pepper ?knife)
   
    (cut ?diced-bell-pepper ?ks-with-diced-bell-pepper ?ks-with-seeded-bell-pepper ?seeded-bell-pepper diced ?knife) ;; reuse the same knife

    ;; "6 green onions, thinly sliced"
    (fetch-and-proportion ?proportioned-onion ?ks-with-onion ?ks-with-diced-bell-pepper ?target-container-3 green-onion 6 piece)
    (cut ?sliced-onion ?ks-with-sliced-onion ?ks-with-onion ?proportioned-onion fine-slices ?knife) ;; reuse the same knife
    
    ;; "1 jalapeno pepper, seeded and minced"
    (fetch-and-proportion ?proportioned-jalapeno ?ks-with-jalapeno ?ks-with-sliced-onion ?target-container-4 jalapeno 1 piece)
    (seed ?seeded-jalapeno ?jalapeno-seeds ?ks-with-seeded-jalapeno ?ks-with-jalapeno ?proportioned-jalapeno ?knife) ;; reuse the same knife
    (cut ?minced-jalapeno ?ks-with-minced-jalapeno ?ks-with-seeded-jalapeno ?seeded-jalapeno minced ?knife) ;; reuse the same knife

    ;; "20 grams coarsely chopped fresh cilantro leaves"
    (fetch-and-proportion ?proportioned-cilantro ?ks-with-cilantro ?ks-with-minced-jalapeno ?target-container-5 fresh-cilantro 20 g)
    (cut ?chopped-cilantro ?ks-with-chopped-cilantro ?ks-with-cilantro ?proportioned-cilantro minced ?knife) ;; reuse the same knife
    
    ;; "250 ml lime juice"
    (fetch-and-proportion ?proportioned-lime-juice ?ks-with-lime-juice ?ks-with-cilantro ?target-container-6 lime-juice 250 ml)
              
    ;; "1 tablespoon olive oil"
    (fetch-and-proportion ?proportioned-olive-oil ?ks-with-olive-oil ?ks-with-lime-juice ?target-container-7 olive-oil 1 tablespoon)

    ;; "330 grams diced mango"
    (fetch-and-proportion ?proportioned-mango ?ks-with-mango ?ks-with-olive-oil ?target-container-8 mango 330 g)
    (cut ?diced-mango ?ks-with-diced-mango ?ks-with-mango ?proportioned-mango diced ?knife) ;; reuse the same knife

    ;; "1/2 teaspoon salt"
    (fetch-and-proportion ?proportioned-salt ?ks-with-salt ?ks-with-diced-mango ?target-container-9 salt 0.5 teaspoon)

    ;; "Gather the ingredients."
    ; (already happened)

    ;; "In a large bowl, combine the rinsed black beans, the diced bell pepper, green onions, minced jalapeno pepper, and fresh cilantro."
    (fetch ?large-bowl ?ks-with-large-bowl ?ks-with-salt large-bowl 1) ;; IMPLICIT

    (transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-large-bowl ?large-bowl ?washed-black-beans ?quantity-a ?unit-a)
    (transfer-contents ?output-container-b ?rest-b ?output-ks-b ?output-ks-a ?output-container-a ?diced-bell-pepper ?quantity-b ?unit-b)       
    (transfer-contents ?output-container-c ?rest-c ?output-ks-c ?output-ks-b ?output-container-b ?sliced-onion ?quantity-c ?unit-c)
    (transfer-contents ?output-container-d ?rest-d ?output-ks-d ?output-ks-c ?output-container-c ?minced-jalapeno ?quantity-d ?unit-d)
    (transfer-contents ?output-container-e ?rest-e ?output-ks-e ?output-ks-d ?output-container-d ?chopped-cilantro ?quantity-e ?unit-e)
    (mingle ?bean-mix ?ks-with-bean-mix ?output-ks-e ?output-container-e ?mingling-tool)

    ;; "In a separate small bowl, whisk together the olive oil and lime juice."
    (fetch ?small-bowl ?ks-with-small-bowl ?ks-with-bean-mix small-bowl 1) ;; IMPLICIT

    (transfer-contents ?output-container-f ?rest-f ?output-ks-f ?ks-with-small-bowl ?small-bowl ?proportioned-olive-oil ?quantity-f ?unit-f)
    (transfer-contents ?output-container-g ?rest-g ?output-ks-g ?output-ks-f ?output-container-f ?proportioned-lime-juice ?quantity-g ?unit-g)
    (mix ?liquid-mixture ?ks-with-liquid-mixture ?output-ks-g ?output-container-g ?mixing-tool)

    ;; "Pour the olive oil and lime juice mixture over the bean mix, and gently toss together until well mixed."
    (transfer-contents ?output-container-h ?rest-h ?output-ks-h ?ks-with-liquid-mixture ?bean-mix ?liquid-mixture ?quantity-h ?unit-h)
    (mingle ?black-bean-salad ?ks-with-black-bean-salad ?output-ks-h ?output-container-h ?mingling-tool) ;; reuse mingling tool

    ;; "Once the ingredients are well combined, carefully and gently fold in the diced mango and season lightly with salt."
    (transfer-contents ?output-container-i ?rest-i ?output-ks-i ?ks-with-black-bean-salad ?black-bean-salad ?diced-mango ?quantity-i ?unit-i)
    (mingle ?black-bean-mango-salad ?ks-with-black-bean-mango-salad ?output-ks-i ?output-container-i ?mingling-tool)  ;; reuse mingling tool  
    (sprinkle ?seasoned-salad ?ks-with-seasoned-salad ?ks-with-black-bean-mango-salad ?black-bean-mango-salad ?proportioned-salt)))
  

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