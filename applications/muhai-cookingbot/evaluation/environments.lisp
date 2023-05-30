(in-package :muhai-cookingbot)

;;;;;;;;;;;;;;;;;;;;
;; Kitchen States ;;
;;;;;;;;;;;;;;;;;;;;

(defparameter *full-kitchen*
  (make-instance
   'kitchen-state
   :contents
   (list (make-instance 'fridge
                        :contents (list
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'apple :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'piece)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 6)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'avocado
                                                                                 :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'piece)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 6)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'banana :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'piece)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 6)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'black-bean :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 500)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'black-olive :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'piece)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 100)))))
                                   (make-instance 'medium-bowl
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
                                                                                                                         :value 500)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'broccoli
                                                                                 :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 500)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'carrot :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'piece)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 12)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'celery :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'piece)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 6)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'cherry-tomato :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 500)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'cooked-bacon :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 500)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'cooked-chicken :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 500)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'corn :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 500)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'cranberry :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 500)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'cream-cheese
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
                                                  :used T
                                                  :contents (list (make-instance 'crushed-pineapple-in-syrup :amount
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
                                                                                                                         :value 10)))))
                                   (make-instance 'medium-bowl
                                                  :used T
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
                                                                                                                         :value 12)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'egg-white
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
                                                  :used T
                                                  :contents (list (make-instance 'feta-cheese :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 500)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'fresh-basil :amount

                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 500))
                                                                                 )))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'fresh-cilantro :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 200)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'fresh-oregano :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 50)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'grated-horseradish :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 200)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'grated-mozzarella :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 500)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'green-cabbage :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'piece)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 2)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'green-chili-pepper :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'piece)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 5)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'hard-boiled-egg :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'piece)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 12)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'heavy-cream :amount
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
                                                  :contents (list (make-instance 'lemon-juice :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'l)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 0.5)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'lime-juice :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'l)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 0.5)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'mango :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'piece)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 5)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'mayonnaise :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'l)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 0.5)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'milk
                                                                                 :temperature
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'degrees-celsius)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 5))
                                                                                 :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'l)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 0.5)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'mixed-greens :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 500)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'radish :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'piece)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 10)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'red-bell-pepper :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'piece)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 10)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'red-chili-pepper :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'piece)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 5)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'romaine-lettuce :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'piece)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 2)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'salted-butter
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
                                                  :used T
                                                  :contents (list (make-instance 'shredded-coconut
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
                                                  :used T
                                                  :contents (list (make-instance 'tomato :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'piece)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 12)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'water :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'l)
                                                                                                :quantity (make-instance 'quantity :value 1)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'yellow-mustard :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'l)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 0.5)))))))
            
         (make-instance 'freezer
                        :contents (list
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'frozen-corn :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 500)))))))
         (make-instance 'pantry                        
                        :contents (list
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'almond :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 500)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'almond-extract :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 100)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'almond-flakes :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity :value 250)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'almond-flour :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 1000)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'all-purpose-flour :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 1000)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'baking-powder :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 250)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'baking-soda :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 50)))))

                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'bisquick-baking-mix :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 300)))))

                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'brown-sugar :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 1000)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'caster-sugar :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 1000)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'celery-seed :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 200)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'cider-vinegar :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'l)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 0.5)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'coarse-salt :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 500)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'cocoa-powder :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 500)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'coconut-oil :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'l)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 0.5)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'corn-flakes :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 500)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'devils-food-cake-mix :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 600)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'dried-dill-weed :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 500)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'extra-virgin-olive-oil :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'l)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 0.5)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'garlic :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'piece)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 5)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'garlic-powder :amount
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
                                                                                                                         :value 10)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'ground-allspice :amount
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
                                                  :contents (list (make-instance 'ground-cinnamon :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 50)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'ground-cloves :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 50)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'ground-cumin :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 500)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'ground-ginger :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 50)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'ground-nutmeg :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 50)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'marshmallow :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 500)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'molasses :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 900)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'mustard-seed :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 200)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'oats :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 500)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'onion :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'piece)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 10)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'paprika-powder :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 300)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'potato :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'piece)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 12)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'powdered-white-sugar :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 500)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'raisin :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 200)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'red-onion :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'piece)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 5)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'red-pepper-flakes :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 50)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'red-wine-vinegar :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'l)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 0.5)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'salt :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 500)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'self-rising-flour :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 1000)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'semisweet-chocolate-chips :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 500)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'shallot :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'piece)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 5)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'sweet-potato :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 500)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'tabasco :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'l)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 0.25)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'trader-joes-cilantro-salad-dressing :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'l)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 0.5)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'turmeric-powder :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 200)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'vanilla :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 500)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'vanilla-extract :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 100)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'vegetable-oil :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 200)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'walnut :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 500)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'white-bread-slice :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'piece)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 12)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'white-sugar :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 1000)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'white-vinegar :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 500)))))
                                   (make-instance 'medium-bowl
                                                  :used T
                                                  :contents (list (make-instance 'whole-wheat-flour :amount
                                                                                 (make-instance 'amount
                                                                                                :unit (make-instance 'g)
                                                                                                :quantity (make-instance 'quantity
                                                                                                                         :value 1000)))))))
         (make-instance 'kitchen-cabinet
                        :contents (append
                                   (loop repeat 9 collect (make-instance 'small-bowl))
                                   (loop repeat 18 collect (make-instance 'medium-bowl))
                                   (loop repeat 9 collect (make-instance 'large-bowl))
                                   (loop repeat 3 collect (make-instance 'cooking-pot))
                                   (loop repeat 3 collect (make-instance 'frying-pan))
                                   (loop repeat 3 collect (make-instance 'jar))
                                   (loop repeat 9 collect (make-instance 'small-bowl-lid))
                                   (loop repeat 18 collect (make-instance 'medium-bowl-lid))
                                   (loop repeat 9 collect (make-instance 'large-bowl-lid))
                                   (loop repeat 3 collect (make-instance 'jar-lid))
                                   (loop repeat 1 collect (make-instance 'plastic-wrap))
                                   (loop repeat 9 collect (make-instance 'fork))
                                   (loop repeat 9 collect (make-instance 'table-spoon))
                                   (loop repeat 9 collect (make-instance 'tea-spoon))
                                   (loop repeat 9 collect (make-instance 'knife))
                                   (loop repeat 3 collect (make-instance 'bread-knife))
                                   (loop repeat 9 collect (make-instance 'whisk))
                                   (loop repeat 9 collect (make-instance 'wooden-spoon))
                                   (loop repeat 3 collect (make-instance 'spatula))
                                   (loop repeat 3 collect (make-instance 'sift))
                                   (loop repeat 3 collect (make-instance 'egg-separator))
                                   (loop repeat 9 collect (make-instance 'cutting-board))
                                   (loop repeat 3 collect (make-instance 'colander))
                                   (loop repeat 3 collect (make-instance 'mixer))
                                   (loop repeat 1 collect (make-instance 'food-processor))
                                   (loop repeat 3 collect (make-instance 'rolling-pin))
                                   (loop repeat 3 collect (make-instance 'baking-paper))
                                   (list (make-instance 'paper-baking-cups
                                                        :items (list (make-instance 'paper-baking-cup) (make-instance 'paper-baking-cup) (make-instance 'paper-baking-cup)
                                                                     (make-instance 'paper-baking-cup) (make-instance 'paper-baking-cup) (make-instance 'paper-baking-cup)
                                                                     (make-instance 'paper-baking-cup) (make-instance 'paper-baking-cup) (make-instance 'paper-baking-cup)
                                                                     (make-instance 'paper-baking-cup) (make-instance 'paper-baking-cup) (make-instance 'paper-baking-cup)
                                                                     (make-instance 'paper-baking-cup) (make-instance 'paper-baking-cup) (make-instance 'paper-baking-cup)))                     
                                         (make-instance 'wire-rack)
                                         (make-instance 'pan)
                                         (make-instance 'cookie-sheet)
                                         (make-instance 'muffin-tins-12)
                                         (make-instance 'baking-tray)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simulation Environments ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass simulation-environment ()
  ((recipe-id :type symbol :initarg :recipe-id :accessor recipe-id :initform nil)
   (kitchen-state :type kitchen-state :initarg :kitchen-state :accessor kitchen-state)
   (meaning-network :type list :initarg :meaning-network :accessor meaning-network :initform '())
   (final-node :type irl-program-processor-node :accessor final-node)
   (primary-output-var :type symbol :initarg :primary-output-var :accessor primary-output-var :initform nil)
   (output-node :type irl-program-processor-node :accessor output-node :initform '())
   (execution-time :accessor execution-time :initform '()))
  (:documentation "Class wrapping all information for setting up and evaluating an environment."))

(defmethod initialize-instance :after ((simulation-environment simulation-environment) &key)
  "Execute the simulation environment's network once and already store the solution (to prevent multiple re-executions)."
  (when (meaning-network simulation-environment)
    (let ((extended-mn (append-meaning-and-irl-bindings (meaning-network simulation-environment) nil)))
      (init-kitchen-state simulation-environment)
      (multiple-value-bind (bindings nodes) (evaluate-irl-program extended-mn nil)
        ; store the time it took to execute the whole recipe (i.e., to have all bindings available) 
        (setf (execution-time simulation-environment) (compute-execution-time (first bindings)))
         ; we only expect there to be one solution
        (setf (final-node simulation-environment) (first nodes)))))
  (when (and (final-node simulation-environment) (primary-output-var simulation-environment)) 
    (let ((node (final-node simulation-environment))
          (var-to-find (primary-output-var simulation-environment)))
      (loop for output-var = (second (irl::primitive-under-evaluation node))
            when (eql output-var var-to-find)
              do (setf (output-node simulation-environment) node)
            do (setf node (parent node))
            while (and node (not (output-node simulation-environment)))))))

(defmethod init-kitchen-state ((simulation-environment simulation-environment))
  "Set initial kitchen state to be used in simulation to the one of the given environment."
  (setf *initial-kitchen-state* (copy-object (kitchen-state simulation-environment))))


(defun almond-crescent-cookies-gold-standard (&optional (kitchen-state *full-kitchen*))
  (make-instance 'simulation-environment
                 :recipe-id 'almond-crescent-cookies
                 :kitchen-state
                 kitchen-state
                 :meaning-network
                 (meaning-network
                  (first
                   (parse-solutions-file
                    (babel-pathname :directory '("applications" "muhai-cookingbot" "recipe-execution-benchmark" "data"
                                                 "gold-standard-solutions" "meaning-only")
                                    :name "almond-crescent-cookies" :type "solution"))))
                 :primary-output-var
                 '?almond-crescent-cookies))

(defun afghan-biscuits-gold-standard (&optional (kitchen-state *full-kitchen*))
  (make-instance 'simulation-environment
                 :recipe-id 'afghan-biscuits
                 :kitchen-state
                 kitchen-state                                    
                 :meaning-network
                 (meaning-network
                  (first
                   (parse-solutions-file
                    (babel-pathname :directory '("applications" "muhai-cookingbot" "recipe-execution-benchmark" "data"
                                                 "gold-standard-solutions" "meaning-only")
                                    :name "afghan-biscuits" :type "solution"))))
                 :primary-output-var '?sprinkled-cookies))

(defun best-brownies-gold-standard (&optional (kitchen-state *full-kitchen*))
  (make-instance 'simulation-environment
                 :recipe-id 'best-brownies
                 :kitchen-state 
                 kitchen-state
                 :meaning-network
                 (meaning-network
                  (first
                   (parse-solutions-file
                    (babel-pathname :directory '("applications" "muhai-cookingbot" "recipe-execution-benchmark" "data"
                                                 "gold-standard-solutions" "meaning-only")
                                    :name "best-brownies" :type "solution"))))
                 :primary-output-var '?cut-brownie))

(defun chocolate-fudge-cookies-gold-standard (&optional (kitchen-state *full-kitchen*))
  (make-instance 'simulation-environment
                 :recipe-id 'chocolate-fudge-cookies
                 :kitchen-state
                 kitchen-state
                 :meaning-network
                 (meaning-network
                  (first
                   (parse-solutions-file
                    (babel-pathname :directory '("applications" "muhai-cookingbot" "recipe-execution-benchmark" "data"
                                                 "gold-standard-solutions" "meaning-only")
                                    :name "chocolate-fudge-cookies" :type "solution"))))
                 :primary-output-var '?cooled-cookies))

(defun easy-banana-bread-gold-standard (&optional (kitchen-state *full-kitchen*))
  (make-instance 'simulation-environment
                 :recipe-id 'easy-banana-bread
                 :kitchen-state
                 kitchen-state
                 :meaning-network
                 (meaning-network
                  (first
                   (parse-solutions-file
                    (babel-pathname :directory '("applications" "muhai-cookingbot" "recipe-execution-benchmark" "data"
                                                 "gold-standard-solutions" "meaning-only")
                                    :name "easy-banana-bread" :type "solution"))))
                 :primary-output-var '?baked-banana-bread))

(defun easy-oatmeal-cookies-gold-standard (&optional (kitchen-state *full-kitchen*))
  (make-instance 'simulation-environment
                 :recipe-id 'easy-oatmeal-cookies
                 :kitchen-state
                 kitchen-state
                 :meaning-network
                 (meaning-network
                  (first
                   (parse-solutions-file
                    (babel-pathname :directory '("applications" "muhai-cookingbot" "recipe-execution-benchmark" "data"
                                                 "gold-standard-solutions" "meaning-only")
                                    :name "easy-oatmeal-cookies" :type "solution"))))
                 :primary-output-var '?baked-cookies))

(defun whole-wheat-ginger-snaps-gold-standard (&optional (kitchen-state *full-kitchen*))
  (make-instance 'simulation-environment
                 :recipe-id 'whole-wheat-ginger-snaps
                 :kitchen-state
                 kitchen-state
                 :meaning-network
                 (meaning-network
                  (first
                   (parse-solutions-file
                    (babel-pathname :directory '("applications" "muhai-cookingbot" "recipe-execution-benchmark" "data"
                                                 "gold-standard-solutions" "meaning-only")
                                    :name "whole-wheat-ginger-snaps" :type "solution"))))
                 :primary-output-var '?cooled-snaps))

(defun cucumber-slices-with-dill-gold-standard (&optional (kitchen-state *full-kitchen*))
  (make-instance 'simulation-environment
                 :recipe-id 'cucumber-slices-with-dill
                 :kitchen-state
                 kitchen-state
                 :meaning-network
                 (meaning-network
                  (first
                   (parse-solutions-file
                    (babel-pathname :directory '("applications" "muhai-cookingbot" "recipe-execution-benchmark" "data"
                                                 "gold-standard-solutions" "meaning-only")
                                    :name "cucumber-slices-with-dill" :type "solution"))))
                 :primary-output-var '?served-salad))

(defun easy-cherry-tomato-corn-salad-gold-standard (&optional (kitchen-state *full-kitchen*))
  (make-instance 'simulation-environment
                 :recipe-id 'easy-cherry-tomato-corn-salad
                 :kitchen-state
                 kitchen-state
                 :meaning-network
                 (meaning-network
                  (first
                   (parse-solutions-file
                    (babel-pathname :directory '("applications" "muhai-cookingbot" "recipe-execution-benchmark" "data"
                                                 "gold-standard-solutions" "meaning-only")
                                    :name "easy-cherry-tomato-corn-salad" :type "solution"))))
                 :primary-output-var '?cooled-salad))

(defun black-bean-and-sweet-potato-salad-gold-standard (&optional (kitchen-state *full-kitchen*))
  (make-instance 'simulation-environment
                 :recipe-id 'black-bean-and-sweet-potato-salad
                 :kitchen-state
                 kitchen-state
                 :meaning-network
                 (meaning-network
                  (first
                   (parse-solutions-file
                    (babel-pathname :directory '("applications" "muhai-cookingbot" "recipe-execution-benchmark" "data"
                                                 "gold-standard-solutions" "meaning-only")
                                    :name "vegan-black-bean-and-sweet-potato-salad" :type "solution"))))
                 :primary-output-var '?salad))

; Extra Almond Crescent Cookies
(defun almond-crescent-cookies-2-gold-standard (&optional (kitchen-state *full-kitchen*))
  (make-instance 'simulation-environment
                 :recipe-id 'almond-crescent-cookies-2
                 :kitchen-state
                 kitchen-state
                 :meaning-network
                 (meaning-network
                  (first
                   (parse-solutions-file
                    (babel-pathname :directory '("applications" "muhai-cookingbot" "recipe-execution-benchmark" "data"
                                                 "gold-standard-solutions" "meaning-only")
                                    :name "almond-crescent-cookies-2" :type "solution"))))
                 :primary-output-var
                 '?cooled-cookies))



(defun almond-crescent-cookies-3-gold-standard (&optional (kitchen-state *full-kitchen*))
  (make-instance 'simulation-environment
                 :recipe-id 'almond-crescent-cookies-3
                 :kitchen-state
                 kitchen-state
                 :meaning-network
                 (meaning-network
                  (first
                   (parse-solutions-file
                    (babel-pathname :directory '("applications" "muhai-cookingbot" "recipe-execution-benchmark" "data"
                                                 "gold-standard-solutions" "meaning-only")
                                    :name "almond-crescent-cookies-3" :type "solution"))))
                 :primary-output-var
                 '?almond-crescent-cookies))

(defun almond-crescent-cookies-4-gold-standard (&optional (kitchen-state *full-kitchen*))
  (make-instance 'simulation-environment
                 :recipe-id 'almond-crescent-cookies-4
                 :kitchen-state
                 kitchen-state
                 :meaning-network
                 (meaning-network
                  (first
                   (parse-solutions-file
                    (babel-pathname :directory '("applications" "muhai-cookingbot" "recipe-execution-benchmark" "data"
                                                 "gold-standard-solutions" "meaning-only")
                                    :name "almond-crescent-cookies-4" :type "solution"))))
                 :primary-output-var
                 '?baked-crescents))

(defun almond-crescent-cookies-5-gold-standard (&optional (kitchen-state *full-kitchen*))
  (make-instance 'simulation-environment
                 :recipe-id 'almond-crescent-cookies-5
                 :kitchen-state
                 kitchen-state
                 :meaning-network
                 (meaning-network
                  (first
                   (parse-solutions-file
                    (babel-pathname :directory '("applications" "muhai-cookingbot" "recipe-execution-benchmark" "data"
                                                 "gold-standard-solutions" "meaning-only")
                                    :name "almond-crescent-cookies-5" :type "solution"))))
                 :primary-output-var
                 '?covered-cookies))

; extra baking recipes

(defun coconut-tuiles-gold-standard (&optional (kitchen-state *full-kitchen*))
  (make-instance 'simulation-environment
                 :recipe-id 'coconut-tuiles
                 :kitchen-state
                 kitchen-state
                 :meaning-network
                 (meaning-network
                  (first
                   (parse-solutions-file
                    (babel-pathname :directory '("applications" "muhai-cookingbot" "recipe-execution-benchmark" "data"
                                                 "gold-standard-solutions" "meaning-only")
                                    :name "coconut-tuiles" :type "solution"))))
                 :primary-output-var
                 '?cooled-rounds))

(defun mexican-wedding-cookies-gold-standard (&optional (kitchen-state *full-kitchen*))
  (make-instance 'simulation-environment
                 :recipe-id 'mexican-wedding-cookies
                 :kitchen-state
                 kitchen-state
                 :meaning-network
                 (meaning-network
                  (first
                   (parse-solutions-file
                    (babel-pathname :directory '("applications" "muhai-cookingbot" "recipe-execution-benchmark" "data"
                                                 "gold-standard-solutions" "meaning-only")
                                    :name "mexican-wedding-cookies" :type "solution"))))
                 :primary-output-var
                 '?mexican-wedding-cookies))

(defun bisquick-shortcake-biscuits-gold-standard (&optional (kitchen-state *full-kitchen*))
  (make-instance 'simulation-environment
                 :recipe-id 'bisquick-shortcake-biscuits
                 :kitchen-state
                 kitchen-state
                 :meaning-network
                 (meaning-network
                  (first
                   (parse-solutions-file
                    (babel-pathname :directory '("applications" "muhai-cookingbot" "recipe-execution-benchmark" "data"
                                                 "gold-standard-solutions" "meaning-only")
                                    :name "bisquick-shortcake-biscuits" :type "solution"))))
                 :primary-output-var
                 '?bisquick-shortcake-biscuits))

(defun chocolate-cream-cheese-cupcakes-gold-standard (&optional (kitchen-state *full-kitchen*))
  (make-instance 'simulation-environment
                 :recipe-id 'chocolate-cream-cheese-cupcakes
                 :kitchen-state
                 kitchen-state
                 :meaning-network
                 (meaning-network
                  (first
                   (parse-solutions-file
                    (babel-pathname :directory '("applications" "muhai-cookingbot" "recipe-execution-benchmark" "data"
                                                 "gold-standard-solutions" "meaning-only")
                                    :name "chocolate-cream-cheese-cupcakes" :type "solution"))))
                 :primary-output-var
                 '?baked-cupcakes))

;; extra salads
(defun black-bean-salad-2-gold-standard (&optional (kitchen-state *full-kitchen*))
  (make-instance 'simulation-environment
                 :recipe-id 'black-bean-salad-2
                 :kitchen-state
                 kitchen-state
                 :meaning-network
                 (meaning-network
                  (first
                   (parse-solutions-file
                    (babel-pathname :directory '("applications" "muhai-cookingbot" "recipe-execution-benchmark" "data"
                                                 "gold-standard-solutions" "meaning-only")
                                    :name "black-bean-salad-2" :type "solution"))))
                 :primary-output-var
                 '?served-salad))

(defun black-bean-salad-3-gold-standard (&optional (kitchen-state *full-kitchen*))
  (make-instance 'simulation-environment
                 :recipe-id 'black-bean-salad-3
                 :kitchen-state
                 kitchen-state
                 :meaning-network
                 (meaning-network
                  (first
                   (parse-solutions-file
                    (babel-pathname :directory '("applications" "muhai-cookingbot" "recipe-execution-benchmark" "data"
                                                 "gold-standard-solutions" "meaning-only")
                                    :name "black-bean-salad-3" :type "solution"))))
                 :primary-output-var
                 '?cooled-salad))

(defun black-bean-salad-4-gold-standard (&optional (kitchen-state *full-kitchen*))
  (make-instance 'simulation-environment
                 :recipe-id 'black-bean-salad-4
                 :kitchen-state
                 kitchen-state
                 :meaning-network
                 (meaning-network
                  (first
                   (parse-solutions-file
                    (babel-pathname :directory '("applications" "muhai-cookingbot" "recipe-execution-benchmark" "data"
                                                 "gold-standard-solutions" "meaning-only")
                                    :name "black-bean-salad-4" :type "solution"))))
                 :primary-output-var
                 '?seasoned-salad))

(defun black-bean-salad-5-gold-standard (&optional (kitchen-state *full-kitchen*))
  (make-instance 'simulation-environment
                 :recipe-id 'black-bean-salad-5
                 :kitchen-state
                 kitchen-state
                 :meaning-network
                 (meaning-network
                  (first
                   (parse-solutions-file
                    (babel-pathname :directory '("applications" "muhai-cookingbot" "recipe-execution-benchmark" "data"
                                                 "gold-standard-solutions" "meaning-only")
                                    :name "black-bean-salad-5" :type "solution"))))
                 :primary-output-var
                 '?black-bean-salad))

(defun classic-greek-salad-gold-standard (&optional (kitchen-state *full-kitchen*))
  (make-instance 'simulation-environment
                 :recipe-id 'classic-greek-salad
                 :kitchen-state
                 kitchen-state
                 :meaning-network
                 (meaning-network
                  (first
                   (parse-solutions-file
                    (babel-pathname :directory '("applications" "muhai-cookingbot" "recipe-execution-benchmark" "data"
                                                 "gold-standard-solutions" "meaning-only")
                                    :name "classic-greek-salad" :type "solution"))))
                 :primary-output-var
                 '?greek-salad))

(defun classic-potato-salad-gold-standard (&optional (kitchen-state *full-kitchen*))
  (make-instance 'simulation-environment
                 :recipe-id 'classic-potato-salad
                 :kitchen-state
                 kitchen-state
                 :meaning-network
                 (meaning-network
                  (first
                   (parse-solutions-file
                    (babel-pathname :directory '("applications" "muhai-cookingbot" "recipe-execution-benchmark" "data"
                                                 "gold-standard-solutions" "meaning-only")
                                    :name "classic-potato-salad" :type "solution"))))
                 :primary-output-var
                       '?potato-salad))

(defun cole-slaw-gold-standard (&optional (kitchen-state *full-kitchen*))
  (make-instance 'simulation-environment
                 :recipe-id 'cole-slaw
                 :kitchen-state
                 kitchen-state
                 :meaning-network
                 (meaning-network
                  (first
                   (parse-solutions-file
                    (babel-pathname :directory '("applications" "muhai-cookingbot" "recipe-execution-benchmark" "data"
                                                 "gold-standard-solutions" "meaning-only")
                                    :name "cole-slaw" :type "solution"))))
                 :primary-output-var
                 '?cole-slaw))

(defun cranberry-fluff-salad-gold-standard (&optional (kitchen-state *full-kitchen*))
  (make-instance 'simulation-environment
                 :recipe-id 'cranberry-fluff-salad
                 :kitchen-state
                 kitchen-state
                 :meaning-network
                 (meaning-network
                  (first
                   (parse-solutions-file
                    (babel-pathname :directory '("applications" "muhai-cookingbot" "recipe-execution-benchmark" "data"
                                                 "gold-standard-solutions" "meaning-only")
                                    :name "cranberry-fluff-salad" :type "solution"))))
                 :primary-output-var
                 '?cranberry-fluff-salad))

(defun avocado-chicken-salad-gold-standard (&optional (kitchen-state *full-kitchen*))
  (make-instance 'simulation-environment
                 :recipe-id 'avocado-chicken-salad
                 :kitchen-state
                 kitchen-state
                 :meaning-network
                 (meaning-network
                  (first
                   (parse-solutions-file
                    (babel-pathname :directory '("applications" "muhai-cookingbot" "recipe-execution-benchmark" "data"
                                                 "gold-standard-solutions" "meaning-only")
                                    :name "avocado-chicken-salad" :type "solution"))))
                 :primary-output-var
                 '?cooled-salad))

(defun basic-chicken-salad-gold-standard (&optional (kitchen-state *full-kitchen*))
  (make-instance 'simulation-environment
                 :recipe-id 'basic-chicken-salad
                 :kitchen-state
                 kitchen-state
                 :meaning-network
                 (meaning-network
                  (first
                   (parse-solutions-file
                    (babel-pathname :directory '("applications" "muhai-cookingbot" "recipe-execution-benchmark" "data"
                                                 "gold-standard-solutions" "meaning-only")
                                    :name "basic-chicken-salad" :type "solution"))))
                 :primary-output-var
                 '?basic-chicken-salad))

(defun broccoli-salad-gold-standard (&optional (kitchen-state *full-kitchen*))
  (make-instance 'simulation-environment
                 :recipe-id 'broccoli-salad
                 :kitchen-state
                 kitchen-state
                 :meaning-network
                 (meaning-network
                  (first
                   (parse-solutions-file
                    (babel-pathname :directory '("applications" "muhai-cookingbot" "recipe-execution-benchmark" "data"
                                                 "gold-standard-solutions" "meaning-only")
                                    :name "broccoli-salad" :type "solution"))))
                 :primary-output-var
                 '?proportioned-broccoli))

(defun croutons-vinegar-salad-gold-standard (&optional (kitchen-state *full-kitchen*))
  (make-instance 'simulation-environment
                 :recipe-id 'croutons-vinegar-salad
                 :kitchen-state
                 kitchen-state
                 :meaning-network
                 (meaning-network
                  (first
                   (parse-solutions-file
                    (babel-pathname :directory '("applications" "muhai-cookingbot" "recipe-execution-benchmark" "data"
                                                 "gold-standard-solutions" "meaning-only")
                                    :name "croutons-vinegar-salad" :type "solution"))))
                 :primary-output-var
                 '?croutons-vinegar-salad))

(defparameter *all-gold-standard-solutions* nil)
  #|(list (almond-crescent-cookies-gold-standard)
        (afghan-biscuits-gold-standard)
        (best-brownies-gold-standard)
        (chocolate-fudge-cookies-gold-standard)
        (easy-banana-bread-gold-standard)
        (easy-oatmeal-cookies-gold-standard)
        (whole-wheat-ginger-snaps-gold-standard)
        (cucumber-slices-with-dill-gold-standard)
        (easy-cherry-tomato-corn-salad-gold-standard)
        (black-bean-and-sweet-potato-salad-gold-standard)
        (almond-crescent-cookies-2-gold-standard)
        (almond-crescent-cookies-3-gold-standard)
        (almond-crescent-cookies-4-gold-standard)
        (almond-crescent-cookies-5-gold-standard)
        (coconut-tuiles-gold-standard)
        (mexican-wedding-cookies-gold-standard)
        (bisquick-shortcake-biscuits-gold-standard)
        (chocolate-cream-cheese-cupcakes-gold-standard)
        (black-bean-salad-2-gold-standard)
        (black-bean-salad-3-gold-standard)
        (black-bean-salad-4-gold-standard)
        (black-bean-salad-5-gold-standard)
        (classic-greek-salad-gold-standard)
        (classic-potato-salad-gold-standard)
        (cole-slaw-gold-standard)
        (cranberry-fluff-salad-gold-standard)
        (avocado-chicken-salad-gold-standard)
        (basic-chicken-salad-gold-standard)
        (broccoli-salad-gold-standard)
        (croutons-vinegar-salad-gold-standard))
|#
