(ql:quickload :aipp-cookingbot)
(in-package :aipp-cookingbot)

(activate-monitor trace-irl)

;; ##################################################################
;; Easy Banana Bread
;; https://www.cooks.com/recipe/il0tt9uq/easy-banana-bread.html
;; ##################################################################

(defparameter *initial-kitchen-state* 
  (make-instance 
   'kitchen-state
   :id 'kitchen-state
   :fridge (make-instance 'fridge
                          :contents (list (make-instance 'medium-bowl
                                                         :contents (list (make-instance 'butter :amount
                                                         (make-instance 'amount
                                                                        :unit (make-instance 'g)
                                                                        :quantity (make-instance 'quantity
                                                                                                 :value 250)))))
                                         (make-instance 'medium-bowl
                                                         :contents (list (make-instance 'egg :amount
							 (make-instance 'amount
									:unit (make-instance 'piece)
									:quantity (make-instance 'quantity
												 :value 12)))))))
   :pantry (make-instance 'pantry
			  :contents (list (make-instance 'medium-bowl
                                                         :contents (list (make-instance 'sugar :amount
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
									:unit (make-instance 'l)
									:quantity (make-instance 'quantity
												 :value 1)))))
                                          (make-instance 'medium-bowl
                                                         :contents (list (make-instance 'self-rising-flour :amount
							 (make-instance 'amount
									:unit (make-instance 'g)
									:quantity (make-instance 'quantity
												 :value 1000)))))))
   :kitchen-cabinet (make-instance 'kitchen-cabinet
				   :contents (list (make-instance 'fork)
						   (make-instance 'spatula)		
						   (make-instance 'pan)
                                                   (make-instance 'brush)
                                                   (make-instance 'whisk)
                                                   (make-instance 'whisk)
                                                   (make-instance 'whisk)
                                                   (make-instance 'whisk)
						   (make-instance 'whisk)
						   (make-instance 'medium-bowl)
						   (make-instance 'medium-bowl)
                                                   (make-instance 'medium-bowl)
						   (make-instance 'medium-bowl)
						   (make-instance 'medium-bowl)
                                                   (make-instance 'medium-bowl)
                                                   (make-instance 'medium-bowl)
						   (make-instance 'medium-bowl)
						   (make-instance 'medium-bowl)))))

(add-element (make-html *initial-kitchen-state* :expand-initially t))

(defparameter *easy-banana-bread-recipe*
  '((to-get-kitchen ?kitchen-state)

    ;; Ingredients

    ;; 1/4 cup (1/2 stick butter)
    (fetch-and-proportion ?proportioned-butter ?kitchen-state-with-butter 
			  ?kitchen-state butter 57 g)
    (to-melt ?melted-butter ?kitchen-state-with-melted-butter 
	     ?kitchen-state-with-butter ?proportioned-butter)

    ;; 2 eggs
    (fetch-and-proportion ?proportioned-eggs ?kitchen-state-with-eggs
			  ?kitchen-state-with-melted-butter egg 2 piece)

    ;; 1 cup sugar
    (fetch-and-proportion ?proportioned-sugar ?kitchen-state-with-sugar
			  ?kitchen-state-with-eggs sugar 201 g)

    ;; 3 bananas, mashed
    (fetch-and-proportion ?proportioned-bananas ?kitchen-state-with-bananas 
			  ?kitchen-state-with-sugar banana 3 piece)
    (bind-and-fetch ?fork ?kitchen-state-with-fork-for-mashing 
		    ?kitchen-state-with-bananas fork)
    (to-mash ?mashed-bananas ?kitchen-state-with-mashed-bananas 
	     ?kitchen-state-with-fork-for-mashing ?proportioned-bananas ?fork)

    ;; 1 tsp. vanilla
    (fetch-and-proportion ?proportioned-vanilla ?kitchen-state-with-vanilla
			  ?kitchen-state-with-mashed-bananas vanilla-extract 0.004 l)

    ;; 1 1/2 cups self-rising flour
    (fetch-and-proportion ?proportioned-self-rising-flour ?kitchen-state-with-self-rising-flour 
			  ?kitchen-state-with-vanilla self-rising-flour 204 g)

    ;; Directions

    ;; IMPLICIT: "Crack eggs."
    (bind-and-fetch ?bowl-for-cracking ?kitchen-state-with-bowl-for-cracking 
		    ?kitchen-state-with-self-rising-flour medium-bowl)
    (to-crack ?cracked-eggs ?kitchen-state-with-cracked-eggs 
	      ?kitchen-state-with-bowl-for-cracking ?proportioned-eggs ?bowl-for-cracking)
    
    ;; "Cream together butter, eggs and sugar until smooth."
    (bind-and-fetch ?bowl-for-batter ?kitchen-state-with-bowl-for-batter 
		    ?kitchen-state-with-cracked-eggs medium-bowl)
    (combine-homogeneous ?butter-eggs-sugar-mixture ?kitchen-state-with-butter-eggs-sugar-mixture
	     ?kitchen-state-with-bowl-for-batter ?bowl-for-batter ?melted-butter ?cracked-eggs ?proportioned-sugar)
    (beat ?creamed-mixture ?kitchen-state-with-creamed-mixture 
	  ?kitchen-state-with-butter-eggs-sugar-mixture ?butter-eggs-sugar-mixture)

     ;; "Add bananas and vanilla; beat well."
     (combine-homogeneous ?bananas-vanilla-butter-eggs-sugar-mixture ?kitchen-state-with-bananas-vanilla-butter-eggs-sugar-mixture 
	      ?kitchen-state-with-creamed-mixture ?creamed-mixture ?mashed-bananas ?proportioned-vanilla)
     (beat ?beaten-mixture ?kitchen-state-with-beaten-mixture
	   ?kitchen-state-with-bananas-vanilla-butter-eggs-sugar-mixture ?butter-eggs-sugar-mixture)

     ;; "Mix in flour."
    (combine-homogeneous ?banana-bread-batter ?kitchen-state-with-banana-bread-batter 
	     ?kitchen-state-with-beaten-mixture ?beaten-mixture ?proportioned-self-rising-flour)

    ;; IMPLICIT: "Grease a pan."
    (bind-and-fetch ?pan ?kitchen-state-with-pan 
		    ?kitchen-state-with-banana-bread-batter pan)
    (grease ?greased-pan ?kitchen-state-with-greased-pan 
	    ?kitchen-state-with-pan ?pan butter)

    ;; IMPLICIT: "Transfer batter into pan and spread evenly."
    (bind-and-fetch ?spatula ?kitchen-state-with-spatula 
		    ?kitchen-state-with-greased-pan spatula)
    (to-spread ?pan-with-spread-batter ?kitchen-state-with-spread-batter-in-pan
	       ?kitchen-state-with-spatula ?greased-pan ?banana-bread-batter ?spatula)

    ;; IMPLICIT: "Preheat oven."
    (preheat-oven ?preheated-oven ?kitchen-state-with-preheated-oven 
		  ?kitchen-state-with-spread-batter-in-pan 163 degrees-celsius)

    ;; "Bake at 325 degrees Fahrenheit for about 1 hour or until golden brown."
    (to-transfer ?oven-with-pan ?pan-in-oven ?kitchen-state-with-pan-in-oven 
		 ?kitchen-state-with-preheated-oven ?pan-with-spread-batter ?preheated-oven)
    (define-amount ?time-to-bake 60 minute)
    (to-bake ?pan-with-banana-bread ?kitchen-state-with-banana-bread-in-oven 
	     ?kitchen-state-with-pan-in-oven ?pan-with-spread-batter ?time-to-bake)
    (to-fetch ?fetched-pan-with-banana-bread ?kitchen-state-with-banana-bread-on-counter 
	      ?kitchen-state-with-banana-bread-in-oven ?pan-with-banana-bread)))

(defparameter *easy-banana-bread-recipe*
  (evaluate-irl-program (expand-macros *easy-banana-bread-recipe*) nil))
