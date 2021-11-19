(ql:quickload :aipp-cookingbot)
(in-package :aipp-cookingbot)

;; The 'trace-irl' monitor will make sure that
;; the IRL evaluation process is shown on the web
;; interface (which can be found at localhost:8000).
;; We need to activate it:
(activate-monitor trace-irl)

;; ##################################################################
;; Chocolate Fudge Cookies
;; https://www.allrecipes.com/recipe/19259/chocolate-fudge-cookies/
;; ##################################################################

;; Defining the initial kitchen state
(defparameter *initial-kitchen-state* 
  (make-instance 
   'kitchen-state
   :id 'kitchen-state
   :fridge (make-instance 'fridge
                          :contents ())

   :pantry (make-instance 'pantry
                          :contents (list ;; ingredients
                                          (make-instance 'medium-bowl
                                                         :contents (list (make-instance 'egg :amount
                                                                                        (make-instance 'amount
                                                                                                       :unit (make-instance 'piece)
                                                                                                       :quantity (make-instance 'quantity
                                                                                                                                :value 12)))))
                                          (make-instance 'medium-bowl
                                                         :contents (list (make-instance 'vegetable-oil :amount
                                                                                        (make-instance 'amount
                                                                                                       :unit (make-instance 'l)
                                                                                                       :quantity (make-instance 'quantity
                                                                                                                                :value 1)))))
                                         (make-instance 'medium-bowl
                                                        :contents (list (make-instance 'devils-food-cake-mix :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 1000)))))
                                         (make-instance 'medium-bowl
                                                        :contents (list (make-instance 'semisweet-chocolate-chips :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                               :value 500)))))))
          :kitchen-cabinet (make-instance 'kitchen-cabinet
                                      :contents (list
                                                 ;; bowls
                                                 (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                                 (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                                 (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                                 (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)

                                                 ;; tools
                                                 (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                                 (make-instance 'brush)
                                                 
                                                 ;; baking equipment
                                                 (make-instance 'baking-tray)
                                                 (make-instance 'wire-racks)))))



;; 'make-html' makes an HTML representation of the kitchen state
;; and 'add-element' transfers that to the web interface

;; (add-element (make-html *initial-kitchen-state* :expand-initially t))

;(inspect *initial-kitchen-state*)

;; 'evaluate-irl-program' is used to evaluate an IRL program...
;; The first argument is the IRL program to evaluate, while the
;; second argument is the ontology. This is not yet used here.

(defparameter *o*
  (evaluate-irl-program `(
                        (to-get-kitchen ?kitchen)
                        ; ingredients
                        (bind devils-food-cake-mix ?devils-food-cake-mix ,(make-instance 'devils-food-cake-mix :is-concept T))
                        (to-fetch ?fetched-devils-food-cake-mix ?kitchen-state-1 ?kitchen ?devils-food-cake-mix)) nil))



(setf *chocolate-fudge-cookies-recipe* 
    `((to-get-kitchen ?kitchen)

    ;; Ingredients

    ;; "1 (18.25 ounce) (517 g) package devil's food cake mix"
    (fetch-and-proportion ?proportioned-devils-food-cake-mix ?kitchen-state-with-devils-food-cake-mix ?kitchen devils-food-cake-mix 517 g)

    ;; "2 eggs"
    (fetch-and-proportion ?proportioned-eggs ?kitchen-state-with-eggs ?kitchen-state-with-devils-food-cake-mix egg 2 piece)


    ;; "1/4 cup (56 g) vegetable oil (for dough) + 1/4 cup (56 g) vegetable oil (to grease the baking tray)"
    (fetch-and-proportion ?proportioned-all-vegetable-oil ?kitchen-state-with-all-vegetable-oil ?kitchen-state-with-eggs vegetable-oil 112 g)
    (bind-and-fetch ?bowl-for-vegetable-oil ?kitchen-state-with-bowl-for-vegetable-oil ?kitchen-state-with-all-vegetable-oil medium-bowl)
    (define-amount ?amount-vegetable-oil-for-dough 56 g)
    (to-transfer-contents ?proportioned-vegetable-oil ?proportioned-vegetable-oil-grease ?kitchen-state-with-vegetable-oil ?kitchen-state-with-bowl-for-vegetable-oil ?proportioned-all-vegetable-oil ?bowl-for-vegetable-oil ?amount-vegetable-oil-for-dough)

    ;; "1 cup semi-sweet chocolate chips"
    (fetch-and-proportion ?proportioned-semisweet-chocolate-chips ?kitchen-state-with-semisweet-chocolate-chips ?kitchen-state-with-vegetable-oil semisweet-chocolate-chips 160 g)
  

    ;; Steps
    
    ;; "Grease cookie baking tray"
    (bind-and-fetch ?baking-tray ?kitchen-state-with-baking-tray ?kitchen-state-with-semisweet-chocolate-chips baking-tray)
    (bind-and-fetch ?brush ?kitchen-state-with-brush ?kitchen-state-with-baking-tray brush)
    (to-brush ?greased-baking-tray ?kitchen-state-with-greased-baking-tray ?kitchen-state-with-brush ?proportioned-vegetable-oil-grease ?baking-tray ?brush)
    ;(grease ?greased-baking-tray ?kitchen-state-with-greased-baking-tray ?kitchen-state-with-baking-tray ?baking-tray vegetable-oil)

    ;; "In a medium bowl, stir together the cake mix, eggs and oil until well blended"
    (bind-and-fetch ?medium-bowl ?kitchen-state-with-medium-bowl ?kitchen-state-with-greased-baking-tray medium-bowl)

    (combine ?cake-eggs-oil-mixture ?kitchen-state-with-cake-eggs-oil-mixture ?kitchen-state-with-medium-bowl ?medium-bowl 
             ?proportioned-devils-food-cake-mix ?proportioned-eggs ?proportioned-vegetable-oil)

    ;; "Fold in the chocolate chips"
    (combine ?chocolate-chips-mixture ?kitchen-state-with-chocolate-chips-mixture ?kitchen-state-with-cake-eggs-oil-mixture ?medium-bowl 
             ?proportioned-semisweet-chocolate-chips ?cake-eggs-oil-mixture)

    ;; "Roll the dough into walnut sized balls. Place the cookies 2 inches apart on the cookie baking tray"
    (define-amount ?cookie-dough-portion 20 g)
    (bind two-inch ?pattern ,(make-instance 'two-inch))
    ;(bind even-spread ?pattern ,(make-instance 'even-spread))

    (to-portion-and-arrange ?tray-with-arranged-cookie-dough ?kitchen-state-with-arranged-tray ?kitchen-state-with-chocolate-chips-mixture ?chocolate-chips-mixture
     ?cookie-dough-portion ?pattern ?greased-baking-tray)

    (shape ?tray-with-shaped-cookies ?kitchen-state-with-shaped-cookies-on-tray ?kitchen-state-with-arranged-tray ?tray-with-arranged-cookie-dough walnut-ball-shape)

     ;; "Preheat oven to 350 degrees F (175 degrees C)"
    (preheat-oven ?preheated-oven ?kitchen-state-with-preheated-oven ?kitchen-state-with-shaped-cookies-on-tray 175 degrees-celsius)

    ;; "Bake for 8 to 10 minutes in the preheated oven"
    (to-transfer ?oven-with-tray ?tray-with-shaped-cookies-inside-oven ?kitchen-state-with-tray-in-oven ?kitchen-state-with-preheated-oven 
     ?tray-with-shaped-cookies ?preheated-oven)
    (define-amount ?time-to-bake 9 minute)
    (to-bake ?tray-with-baked-cookies ?kitchen-state-with-baked-cookies-in-oven ?kitchen-state-with-tray-in-oven ?tray-with-shaped-cookies-inside-oven 
      ?time-to-bake)

    ;; "Allow cookies to cool on baking tray for 5 minutes before removing to a wire rack to cool completely"
    ;;(define-ammount ?time-to-cool 5 min)
    ;;(cool ?semi-cooled-cookies ?kitchen-state-with-semi-cooled-cookies ?kitchen-state-with-baked-cookies-in-oven ?tray-with-baked-cookies)
    
    (define-amount ?semi-cool-temp 60 degrees-celsius)
    (to-cool ?semi-cooled-cookies ?kitchen-state-with-semi-cooled-cookies ?kitchen-state-with-baked-cookies-in-oven ?tray-with-baked-cookies ?semi-cool-temp)

    (to-fetch ?fetched-semi-cooled-cookies ?kitchen-state-with-semi-cooled-cookies-on-counter ?kitchen-state-with-semi-cooled-cookies ?semi-cooled-cookies)

    (bind-and-fetch ?wire-racks ?kitchen-state-with-wire-racks-on-counter ?kitchen-state-with-semi-cooled-cookies-on-counter wire-racks)
    
    (transfer-all-contents ?wire-racks-with-semi-cooled-cookies ?kitchen-state-with-semi-cooled-cookies-in-racks ?kitchen-state-with-wire-racks-on-counter 
      ?wire-racks ?semi-cooled-cookies)

    ;(cool ?cooled-cookies ?kitchen-state-with-cooled-cookies ?kitchen-state-with-semi-cooled-cookies-in-racks ?wire-racks-with-semi-cooled-cookies)
    (define-amount ?cool-temp 18 degrees-celsius)
    (to-cool ?cooled-cookies  ?kitchen-state-with-cooled-cookies ?kitchen-state-with-semi-cooled-cookies-in-racks ?wire-racks-with-semi-cooled-cookies ?cool-temp)))

(evaluate-irl-program (expand-macros *chocolate-fudge-cookies-recipe*) nil)




;; Inspecting small parts of the recipe:
;;--------------------------------------
;; printing to the output buffer
(pprint `((bind ?kitchen-state *kitchen-state-1*)
          ,@(fetch-and-proportion ?proportioned-butter ?kitchen-state-with-butter *butter-1* ?kitchen-state 113 g)
          (melt ?melted-butter ?kitchen-state-with-melted-butter ?proportioned-butter ?kitchen-state-with-butter)))

;; drawing a network
(draw-recipe '((bind ?kitchen-state *kitchen-state-1*)   
               ,@(fetch-and-proportion ?proportioned-devils-food-cake-mix ?kitchen-state-with-devils-food-cake-mix *devils-food-cake-mix-1* ?kitchen-state 517 g))
             :expand nil) ;;t or nil


;; ======================
;; Macro definitions
;; ======================


;; ======================
;; Visualise the recipe
;; ======================

;; High-level recipe notation:
(draw-recipe *chocolate-fudge-cookies-recipe* :expand nil)

;; All primitives expanded:
(draw-recipe *chocolate-fudge-cookies-recipe* :expand t)

