(ql:quickload :aipp-cookingbot)
(in-package :aipp-cookingbot)

;; The 'trace-irl' monitor will make sure that
;; the IRL evaluation process is shown on the web
;; interface (which can be found at localhost:8000).
;; We need to activate it:
(activate-monitor trace-irl)

;; ##################################################################
;; Almond Crescent Cookies recipe
;; https://www.simplyrecipes.com/recipes/almond_crescent_cookies/
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
                                                                                                                              :value 500)))))))
         (make-instance 'pantry
                        :contents (list (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'white-sugar :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 1000)))))
                                        (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'vanilla-extract :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 100)))))
                                        (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'almond-extract :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 100)))))
                                        (make-instance 'medium-bowl
                                                       :contents (list (make-instance 'all-purpose-flour :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 1000)))))
                                        (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'almond-flour :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 1000)))))
                                        (make-instance 'medium-bowl
                                                       :used T
                                                       :contents (list (make-instance 'powdered-white-sugar :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 500)))))))
         (make-instance 'kitchen-cabinet
                        :contents (list
                                   ;; bowls
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)

                                   ;; tools
                                   (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                   (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                   (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)

                                   ;; baking equipment
                                   (make-instance 'baking-tray)
                                   (make-instance 'baking-paper))))))

(defparameter *almond-cookies-recipe*
   `((to-get-kitchen ?kitchen)

     ;; "1 cup butter, room temperature"
     (fetch-and-proportion ?proportioned-butter ?kitchen-state-with-butter ?kitchen butter 226 g)
     (bring-up-to-room-temperature ?room-temp-butter ?kitchen-state-with-room-temp-butter ?kitchen-state-with-butter ?proportioned-butter)

     ;; "2/3 cup sugar"
     (fetch-and-proportion ?proportioned-sugar ?kitchen-state-with-sugar ?kitchen-state-with-room-temp-butter white-sugar 134 g)

     ;; "1 teaspoon vanilla extract"
     (fetch-and-proportion ?proportioned-vanilla ?kitchen-state-with-vanilla ?kitchen-state-with-sugar vanilla-extract 4 g)

     ;; "1 teaspoon almond extract"
     (fetch-and-proportion ?proportioned-almond-extract ?kitchen-state-with-almond-extract ?kitchen-state-with-vanilla almond-extract 4 g)

     ;; "2 1/2 cups flour"
     (fetch-and-proportion ?proportioned-flour ?kitchen-state-with-flour ?kitchen-state-with-almond-extract all-purpose-flour 340 g)

     ;; "1 cup almond flour"
     (fetch-and-proportion ?proportioned-almond-flour ?kitchen-state-with-almond-flour ?kitchen-state-with-flour almond-flour 112 g)

     ;; "1/4 cup powdered sugar"
     (fetch-and-proportion ?proportioned-powdered-sugar ?kitchen-state-with-powd-sugar ?kitchen-state-with-almond-flour powdered-white-sugar 29 g)

     ;; "Beat the butter and the sugar together until light and fluffy"
     (bind-and-fetch ?fetched-bowl ?kitchen-state-with-bowl ?kitchen-state-with-powd-sugar medium-bowl)
     (transfer-all-contents ?sugar-mixture-bowl ?kitchen-state-with-sugar-mixture ?kitchen-state-with-bowl ?fetched-bowl ?proportioned-sugar ?room-temp-butter)

     (beat ?beaten-mixture-bowl ?kitchen-state-with-beaten-mixture ?kitchen-state-with-sugar-mixture ?sugar-mixture-bowl)

     ;; "Add the vanilla and almond extracts and mix"
     (transfer-all-contents ?extracts-sugar-bowl ?kitchen-state-with-extracts-sugar-bowl ?kitchen-state-with-beaten-mixture ?beaten-mixture-bowl ?proportioned-vanilla ?proportioned-almond-extract)
     (mix ?sugar-extract-mixture-bowl ?kitchen-state-with-sugar-extract-mixture ?kitchen-state-with-extracts-sugar-bowl ?extracts-sugar-bowl)
     ;(combine-homogeneous ?sugar-extract-mixture-bowl ?kitchen-state-with-sugar-extract-mixture ?kitchen-state-with-beaten-mixture
     ;         ?beaten-mixture-bowl ?proportioned-vanilla ?proportioned-almond-extract)

     ;; "Add the flour and almond flour"
     (transfer-all-contents ?flour-and-sugar-bowl ?kitchen-state-with-flour-and-sugar-bowl ?kitchen-state-with-sugar-extract-mixture ?sugar-extract-mixture-bowl ?proportioned-flour ?proportioned-almond-flour)
     ;(combine-homogeneous ?dough ?kitchen-state-with-dough ?kitchen-state-with-sugar-extract-mixture
     ;         ?sugar-extract-mixture-bowl ?proportioned-flour ?proportioned-almond-flour)

     ;; "Mix thoroughly"
     (mix ?dough ?kitchen-state-with-dough ?kitchen-state-with-flour-and-sugar-bowl ?flour-and-sugar-bowl)

    ;; line baking tray
    (bind-and-fetch ?fetched-baking-tray ?kitchen-state-with-tray ?kitchen-state-with-dough baking-tray)
    (line-with-baking-paper ?lined-baking-tray ?kitchen-state-with-lined-baking-tray ?kitchen-state-with-tray ?fetched-baking-tray)

    ;; "Take generous tablespoons of the dough and roll it into a small ball, about an inch in diameter, 
    ;;  and then shape into a crescent shape"
    (define-amount ?tablespoon-portion 25 g)
    (portion ?tray-with-portioned-dough ?kitchen-state-with-portions-on-tray ?kitchen-state-with-lined-baking-tray
             ?dough ?tablespoon-portion ?lined-baking-tray)

    (shape ?tray-with-crescent-dough ?kitchen-state-with-crescent-dough-on-tray ?kitchen-state-with-portions-on-tray ?tray-with-portioned-dough crescent)

    ;; "Place onto a parchment paper lined baking sheet"
    ;; this step is actually intertwined with the previous step

    ;; "Bake at 350°F (175°C) for 15-20 minutes or until a light golden brown"
    ;; preheating of the oven is (implicitly) inferred
    (preheat-oven ?preheated-oven ?kitchen-state-with-preheated-oven ?kitchen-state-with-crescent-dough-on-tray 175 degrees-celsius)

    (to-transfer ?oven-with-tray ?tray-in-oven ?kitchen-state-with-tray-in-oven ?kitchen-state-with-preheated-oven ?tray-with-crescent-dough ?preheated-oven)
    (define-amount ?time-to-bake 25 minute)
    (to-bake ?tray-with-cookies ?kitchen-state-with-cookies-in-oven ?kitchen-state-with-tray-in-oven ?tray-in-oven ?time-to-bake)
    (to-fetch ?fetched-tray-with-cookies ?kitchen-state-with-cookies-on-counter ?kitchen-state-with-cookies-in-oven ?tray-with-cookies)   

    ;; "Dust with powdered sugar"
    (to-sprinkle ?sprinkled-cookies ?kitchen-state-with-sprinkled-cookies
                 ?kitchen-state-with-cookies-on-counter ?fetched-tray-with-cookies ?proportioned-powdered-sugar)))

(evaluate-irl-program (expand-macros *almond-cookies-recipe*) nil)


;; ======================
;; Macro definitions
;; ======================


;;  (pprint `( ,@(fetch-and-proportion ?proportioned-butter ?kitchen-state-with-butter ?kitchen butter 226 g)))


;; ======================
;; Visualise the recipe
;; ======================

;; High-level recipe notation:
(draw-recipe *almond-cookies-recipe* :expand nil)

;; All primitives expanded:
(draw-recipe *almond-cookies-recipe* :expand t)