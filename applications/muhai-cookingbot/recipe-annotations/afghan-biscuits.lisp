
(in-package :muhai-cookingbot)

;; The 'trace-irl' monitor will make sure that
;; the IRL evaluation process is shown on the web
;; interface (which can be found at localhost:8000).
;; We need to activate it:
(activate-monitor trace-irl)


;; ##################################################################
;; New Zealand Afghan Biscuit/Cookie
;; https://www.thespruceeats.com/afghan-biscuit-recipe-256048
;; ##################################################################

(defparameter *initial-kitchen-state* 
  (make-instance 
   'kitchen-state
    :contents
    (list (make-instance 'fridge
                         :contents (list (make-instance 'medium-bowl
                                                        :contents (list (make-instance 'butter :amount
                                                                                       (make-instance 'amount
                                                                                                      :unit (make-instance 'g)
                                                                                                      :quantity (make-instance 'quantity
                                                                                                                                :value 250)))))))
          (make-instance 'pantry
                          :contents (list (make-instance 'medium-bowl
                                                         :contents (list (make-instance 'caster-sugar :amount
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
                                                         :contents (list (make-instance 'cocoa-powder :amount
                                                                                        (make-instance 'amount
                                                                                                       :unit (make-instance 'g)
                                                                                                       :quantity (make-instance 'quantity
                                                                                                                                :value 250)))))
                                          (make-instance 'medium-bowl
                                                         :contents (list (make-instance 'corn-flakes :amount
                                                                                        (make-instance 'amount
                                                                                                       :unit (make-instance 'g)
                                                                                                       :quantity (make-instance 'quantity
                                                                                                                                :value 500)))))
                                          (make-instance 'medium-bowl
                                                         :contents (list (make-instance 'icing-sugar :amount
                                                                                        (make-instance 'amount
                                                                                                       :unit (make-instance 'g)
                                                                                                       :quantity (make-instance 'quantity
                                                                                                                                :value 500)))))
                                          (make-instance 'medium-bowl
                                                         :contents (list (make-instance 'water :amount
                                                                                        (make-instance 'amount
                                                                                                       :unit (make-instance 'l)
                                                                                                       :quantity (make-instance 'quantity :value 1)))))
                                          (make-instance 'medium-bowl
                                                         :contents (list (make-instance 'almond-flakes :amount
                                                                                        (make-instance 'amount
                                                                                                       :unit (make-instance 'g)
                                                                                                       :quantity (make-instance 'quantity :value 250)))))))
          (make-instance 'kitchen-cabinet
				   :contents (list (make-instance 'baking-tray)
						   (make-instance 'baking-paper)
                                                   (make-instance 'whisk)
                                                   (make-instance 'whisk)
                                                   (make-instance 'whisk)
                                                   (make-instance 'whisk)
                                                   (make-instance 'whisk)
                                                   (make-instance 'sift)
                                                   (make-instance 'wooden-spoon)
                                                   (make-instance 'table-spoon)
                                                   (make-instance 'wire-rack)
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
						   (make-instance 'medium-bowl))))))

;(add-element (make-html *initial-kitchen-state* :expand-initially t))

(defparameter *afghan-cookie-recipe* 
  `(;; Initial Kitchen State
    (get-kitchen ?kitchen-state)

    ;;Ingredients

    ;; For biscuits

    ;; "200 grams butter (at room temperature)"
    (fetch-and-proportion ?proportioned-butter ?kitchen-state-with-butter
                          ?kitchen-state butter 200 g)
    
    ;; "1/2 cup caster sugar"
    (fetch-and-proportion ?proportioned-caster-sugar ?kitchen-state-with-caster-sugar
                          ?kitchen-state-with-butter caster-sugar 100 g)
    
    ;; "1 1/2 cup all-purpose flour"
    (fetch-and-proportion ?proportioned-all-purpose-flour ?kitchen-state-with-all-purpose-flour
                          ?kitchen-state-with-caster-sugar all-purpose-flour 301 g)
    
    ;; "3 tablespoons unsweetened cocoa powder"
    (fetch-and-proportion ?proportioned-cocoa-powder ?kitchen-state-with-cocoa-powder
                          ?kitchen-state-with-all-purpose-flour cocoa-powder 38 g)

    ;; "1 1/2  unsweetened corn flakes"
    (fetch-and-proportion ?proportioned-corn-flakes ?kitchen-state-with-corn-flakes
                          ?kitchen-state-with-cocoa-powder corn-flakes 301 g)

    ;; For icing

    ;; "1 cup icing sugar"
    (fetch-and-proportion ?proportioned-icing-sugar ?kitchen-state-with-icing-sugar
                          ?kitchen-state-with-corn-flakes icing-sugar 201 g)

    ;; "2 tablespoons unsweetened cocao powder"
    (fetch-and-proportion ?proportioned-icing-cocoa-powder ?kitchen-state-with-icing-cocoa-powder
                          ?kitchen-state-with-icing-sugar cocoa-powder 26 g)

    ;; "3 tablespoons water"
    (fetch-and-proportion ?proportioned-water ?kitchen-state-with-water
                          ?kitchen-state-with-icing-cocoa-powder water 0.04436 l)

    ;; "Optional: 1/4 cup flaked almonds"
    (fetch-and-proportion ?proportioned-almonds ?kitchen-state-with-almonds
                          ?kitchen-state-with-water  almond-flakes 50 g)

    ;; Instructions

    ;; "Preheat oven to 350 degrees Fahrenheit (175 centigrade)"
    (preheat-oven ?preheated-oven ?kitchen-state-with-preheated-oven
                  ?kitchen-state-with-almonds 175 degrees-celsius)

   
    ;; "Line a baking sheet with baking paper."
    (bind-and-fetch ?baking-tray ?kitchen-state-with-baking-tray
                    ?kitchen-state-with-preheated-oven baking-tray)
    (bind-and-fetch ?baking-paper ?kitchen-state-with-baking-paper
                    ?kitchen-state-with-baking-tray baking-paper)
    (to-line-with ?lined-baking-tray ?kitchen-state-with-lined-baking-tray
                  ?kitchen-state-with-baking-paper ?baking-tray ?baking-paper)

                
    ;; "Cream the butter and sugar until light and fluffy."
    (bind-and-fetch ?bowl-for-creaming ?kitchen-state-with-bowl-for-creaming
                    ?kitchen-state-with-lined-baking-tray medium-bowl)
    (combine-homogeneous ?butter-sugar-mixture ?kitchen-state-with-butter-sugar-mixture
             ?kitchen-state-with-bowl-for-creaming ?bowl-for-creaming ?proportioned-butter ?proportioned-caster-sugar)  
    (beat ?creamed-butter ?kitchen-state-with-creamed-butter
          ?kitchen-state-with-butter-sugar-mixture ?butter-sugar-mixture)
  
    ;; "Sift together the flour and cocoa powder and mix into butter mixture with a wooden spoon."
    (bind-and-fetch ?bowl-for-sifting ?kitchen-state-with-bowl-for-sifting
                    ?kitchen-state-with-creamed-butter medium-bowl)
    (combine-homogeneous ?flour-cocoa-mixture ?kitchen-state-with-flour-cocoa-mixture
             ?kitchen-state-with-bowl-for-sifting ?bowl-for-sifting ?proportioned-all-purpose-flour ?proportioned-cocoa-powder)
    (sift ?sifted-flour-cocoa ?kitchen-state-with-sifted-flour-cocoa
          ?kitchen-state-with-flour-cocoa-mixture ?flour-cocoa-mixture)
    
    (bind-and-fetch ?bowl-for-mixture ?kitchen-state-with-bowl-for-mixture
                    ?kitchen-state-with-sifted-flour-cocoa medium-bowl)
    (combine-homogeneous-with-tool ?butter-flour-cocoa-mixture ?kitchen-state-with-butter-flour-cocoa-mixture
                       ?kitchen-state-with-bowl-for-mixture ?bowl-for-mixture wooden-spoon ?creamed-butter ?sifted-flour-cocoa)

    ;; "Fold in cornflakes and don't worry if they crumble"
    (combine-homogeneous ?cookie-dough ?kitchen-state-with-cookie-dough
             ?kitchen-state-with-butter-flour-cocoa-mixture ?butter-flour-cocoa-mixture ?proportioned-corn-flakes)

    ;; "Roll or press 1 1/2 teaspoons of the dough into balls and flatten them slightly."
    ;; "Place them about 2 inches apart on the baking sheet."
    (define-amount ?single-portion 7.39 g)
    (bind two-inch ?pattern ,(make-instance 'two-inch))
    (to-portion-and-arrange ?tray-with-portioned-dough ?kitchen-state-with-tray-with-portioned-dough
                            ?kitchen-state-with-cookie-dough ?cookie-dough ?single-portion ?pattern ?lined-baking-tray)
 
    (shape ?tray-with-shaped-dough ?kitchen-state-with-tray-with-shaped-balls
           ?kitchen-state-with-tray-with-portioned-dough ?tray-with-portioned-dough flattened-ball-shape)

    ;; "Bake in the oven for 10 to 15 minutes."
    (to-transfer ?oven-with-tray-with-dough ?tray-with-dough-in-oven ?kitchen-state-with-tray-with-dough-in-oven
                 ?kitchen-state-with-tray-with-shaped-balls ?tray-with-shaped-dough ?preheated-oven)
   
    (define-amount ?time-to-bake 10 minute)
    (to-bake ?tray-with-cookies ?kitchen-state-with-tray-with-cookies-in-oven
             ?kitchen-state-with-tray-with-dough-in-oven ?tray-with-dough-in-oven ?time-to-bake)
 
    ;; "Remove from oven, and cool on a wire rack"
    (to-fetch ?fetched-tray-with-cookies ?kitchen-state-with-fetched-tray-with-cookies
              ?kitchen-state-with-tray-with-cookies-in-oven ?tray-with-cookies)
    (bind-and-fetch ?wire-rack ?kitchen-state-with-wire-rack
                    ?kitchen-state-with-fetched-tray-with-cookies wire-rack)
    (define-amount ?all 100 percent)
    (to-transfer-contents ?cookies-on-wire-rack ?rest ?kitchen-state-with-cookies-on-wire-rack
                          ?kitchen-state-with-wire-rack ?fetched-tray-with-cookies ?wire-rack ?all)
    (cool ?cooled-cookies ?kitchen-state-with-cooled-cookies
          ?kitchen-state-with-cookies-on-wire-rack ?cookies-on-wire-rack)      
    
    ;; "Prepare the icing by combining the icing sugar, unsweetened cocoa powder, and water in a bowl."
    ;; "Mix well until mixture is free of lumps and of a creamy consistency"
    (bind-and-fetch ?bowl-for-icing ?kitchen-state-with-bowl-for-icing
                    ?kitchen-state-with-cooled-cookies medium-bowl)


    (combine-homogeneous ?icing-mixture ?kitchen-state-with-icing-mixture
             ?kitchen-state-with-bowl-for-icing ?bowl-for-icing ?proportioned-icing-sugar ?proportioned-icing-cocoa-powder ?proportioned-water)

    ;; "Spoon a little icing on each cookie, and decorate with flaked almonds"
    (bind-and-fetch ?spoon ?kitchen-state-with-spoon
                    ?kitchen-state-with-icing-mixture table-spoon)
    (to-spread ?iced-cookies ?kitchen-state-with-iced-cookies
               ?kitchen-state-with-spoon ?cooled-cookies ?icing-mixture ?spoon)
    (to-sprinkle ?sprinkled-cookies ?kitchen-state-with-sprinkled-cookies
                 ?kitchen-state-with-iced-cookies ?iced-cookies ?proportioned-almonds)
    ))

(defparameter *o*
  (evaluate-irl-program (expand-macros *afghan-cookie-recipe*) nil))
