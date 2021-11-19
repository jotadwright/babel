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
   :fridge (make-instance 'fridge
                          :contents (list (make-instance 'butter :amount
                                                         (make-instance 'amount
                                                                        :unit (make-instance 'g)
                                                                        :quantity (make-instance 'quantity
                                                                                                 :value 500)))
                                          (make-instance 'milk :amount
                                                         (make-instance 'amount
                                                                        :unit (make-instance 'l)
                                                                        :quantity (make-instance 'quantity
                                                                                                 :value 1)))))))

;; 'make-html' makes an HTML representation of the kitchen state
;; and 'add-element' transfers that to the web interface
(add-element (make-html *initial-kitchen-state* :expand-initially t))



;; 'evaluate-irl-program' is used to evaluate an IRL program...
;; The first argument is the IRL program to evaluate, while the
;; second argument is the ontology. This is not yet used here.
(defparameter *o*
  (evaluate-irl-program `((to-get-kitchen ?kitchen)
                          (bind butter ?butter ,(make-instance 'butter :is-concept t))
                          (to-fetch ?fetched-butter ?kitchen-state-1 ?kitchen ?butter)) nil))






#|((g #:?BUTTER-1 AIPP-COOKINGBOT::*BUTTER-1*)
 (AIPP-COOKINGBOT::TO-FETCH #:?KITCHEN-STATE-1 AIPP-COOKINGBOT::?KITCHEN-STATE #:?BUTTER-1)
 (AIPP-COOKINGBOT::TO-BIND #:?BOWL-1 AIPP-COOKINGBOT::*BOWL-1*)
 (AIPP-COOKINGBOT::TO-FETCH #:?KITCHEN-STATE-2 #:?KITCHEN-STATE-1 #:?BOWL-1)
 (AIPP-COOKINGBOT::TO-BIND #:?QUANTITY-1 113)
 (AIPP-COOKINGBOT::TO-BIND #:?UNIT-1 AIPP-COOKINGBOT::G)
 (AIPP-COOKINGBOT::TO-DEFINE-QUANTITY #:?AMOUNT-1 #:?QUANTITY-1 #:?UNIT-1)
 (AIPP-COOKINGBOT::TO-TRANSFER-CONTENTS AIPP-COOKINGBOT::?PROPORTIONED-BUTTER #:?REST-1 AIPP-COOKINGBOT::?KITCHEN-STATE-WITH-BUTTER #:?KITCHEN-STATE-2 #:?BUTTER-1 #:?BOWL-1 #:?AMOUNT-1)) |#






;; ##################################################################
;; Best brownie recipe
;; https://www.allrecipes.com/recipe/25010/absolutely-best-brownies/
;; ##################################################################

(defparameter *brownie-recipe* 
  '(;; Initial Kitchen State
    (to-bind ?kitchen-state *kitchen-state-1*)
    
    ;; "1/2 cup butter, melted"
    (fetch-and-proportion ?proportioned-butter ?kitchen-state-with-butter ?kitchen-state *butter-1* 113 g)
    (to-melt ?melted-butter ?kitchen-state-with-melted-butter ?kitchen-state-with-butter ?proportioned-butter)
    
    ;; "1 cup white sugar"
    (fetch-and-proportion ?proportioned-sugar ?kitchen-state-with-sugar ?kitchen-state-with-melted-butter *sugar-1* 201 g)
                          
    ;; "2 eggs"
    (fetch-and-proportion ?proportioned-eggs ?kitchen-state-with-eggs ?kitchen-state-with-sugar *eggs-1* 2 piece)
    
    ;; "1/2 cup all-purpose flour"
    (fetch-and-proportion ?proportioned-flour ?kitchen-state-with-flour ?kitchen-state-with-eggs *flour-1* 68 g)
    
    ;; "1/3 cup unsweetened cocoa powder"
    (fetch-and-proportion ?proportioned-cocoa ?kitchen-state-with-cocoa ?kitchen-state-with-flour *cocoa-1* 45 g)

    ;; "1/4 teaspoon salt"
    (fetch-and-proportion ?proportioned-salt ?kitchen-state-with-salt ?kitchen-state-with-cocoa *salt-1* 1.5 g)

    ;; "1 teaspoon vanilla extract"
    (fetch-and-proportion ?proportioned-vanilla ?kitchen-state-with-vanilla ?kitchen-state-with-salt *vanilla-1* 4 g)
                          
    ;;"1/2 cup chopped walnuts (optional)"
    (fetch-and-proportion ?proportioned-walnuts ?kitchen-state-with-walnuts ?kitchen-state-with-vanilla *walnuts-1* 50 g)
            
    ;; "Preheat oven to 350 degrees F (175 degrees C)."
    (preheat-oven ?preheated-oven ?kitchen-state-with-preheated-oven ?kitchen-state-with-walnuts 175 degrees-celcius)
                          
    ;; "Grease and flour an 8x8 or 9x9 inch baking pan"
    (to-bind ?pan *baking-pan-1*)
    (to-fetch ?kitchen-state-with-pan ?kitchen-state-with-preheated-oven ?pan)

    (grease ?greased-pan ?kitchen-state-with-greased-pan ?kitchen-state-with-pan ?pan *butter-1* )
                          
    (flour ?floured-pan ?kitchen-state-with-floured-pan ?kitchen-state-with-greased-pan ?greased-pan *flour-1* )

    ;;  "In a medium bowl, beat together the butter and sugar."
    (to-bind ?medium-bowl *medium-bowl-3*)
    (to-fetch ?kitchen-state-with-bowl ?kitchen-state-with-floured-pan ?medium-bowl)
    ;(combine ?sugar-mixture-bowl ?kitchen-state-with-sugar-mixture ?kitchen-state-with-bowl ?medium-bowl ?melted-butter ?proportioned-sugar)
    (transfer-all-contents ?sugar-mixture-bowl ?kitchen-state-with-sugar-mixture ?kitchen-state-with-bowl ?medium-bowl ?melted-butter ?proportioned-sugar)
    (beat ?beaten-mixture-bowl ?kitchen-state-with-beaten-mixture ?kitchen-state-with-sugar-mixture ?sugar-mixture-bowl)    

    ;; "Add eggs, and mix well."
    (crack ?cracked-eggs ?kitchen-state-with-cracked-eggs ?kitchen-state-with-beaten-mixture ?proportioned-eggs ?container-for-eggs)
    (combine ?egg-sugar-mixture-bowl ?kitchen-state-with-eggs-sugar-mixture ?kitchen-state-with-cracked-eggs ?beaten-mixture-bowl ?cracked-eggs)

    ;; "Combine the flour, cocoa and salt; stir into the sugar mixture."
    (to-bind ?bowl-for-mixture *bowl-45*)
    (to-fetch ?kitchen-state-with-bowl-for-mixture ?kitchen-state-with-eggs-sugar-mixture ?bowl-for-mixture )
    (combine ?flour-cocoa-salt-mixture ?kitchen-state-with-flour-cocoa-and-salt-combined ?kitchen-state-with-bowl-for-mixture ?bowl-for-mixture ?proportioned-flour ?proportioned-cocoa ?proportioned-salt)
    (combine ?flour-sugar-mixture-bowl ?kitchen-state-with-flour-sugar-mixture ?kitchen-state-with-flour-cocoa-and-salt-combined ?egg-sugar-mixture-bowl ?flour-cocoa-salt-mixture)

    ;; "Mix in the vanilla and stir in the walnuts if desired."
    (combine ?dough ?kitchen-state-with-dough ?kitchen-state-with-flour-sugar-mixture ?flour-sugar-mixture-bowl ?proportioned-vanilla ?proportioned-walnuts)

    ;;  "Spread evenly into the prepared pan."
    (to-transfer ?pan-with-dough ?kitchen-state-with-dough-in-pan ?kitchen-state-with-dough ?dough ?floured-pan )
;; moet dit niet spread zijn

    ;;  "Bake for 25 to 30 minues in the preheated oven, or until edges are firm."
    (to-transfer ?oven-with-pan ?kitchen-state-with-pan-in-oven ?kitchen-state-with-dough-in-pan ?preheated-oven ?pan-with-dough )
    (define-amount ?time-to-bake 25 min)
    (bake ?pan-with-brownie ?kitchen-state-with-brownie-in-oven ?kitchen-state-with-pan-in-oven ?pan-with-dough ?time-to-bake)
    (to-fetch ?kitchen-state-with-brownie-on-counter ?kitchen-state-with-brownie-in-oven ?pan-with-brownie ) 
                           
    ;;  "Cool before cutting into squares"
    (cool ?cooled-brownie ?kitchen-state-with-cooled-brownie ?kitchen-state-with-brownie-on-counter ?pan-with-brownie)
    (divide ?cut-brownie ?kitchen-state-with-cut-brownie ?kitchen-state-with-cooled-brownie ?cooled-brownie square-pattern)))


;; ======================
;; Visualise the recipe
;; ======================

;; High-level recipe notation:
;(draw-recipe *brownie-recipe* :expand nil)

;; All primitives expanded:
;(draw-recipe *brownie-recipe* :expand t)

