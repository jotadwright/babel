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
                                                                                                                         :value 500)))))
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
                        :contents (list
                                   ;; bowls, pots & pans
                                   (make-instance 'small-bowl) (make-instance 'small-bowl) (make-instance 'small-bowl)
                                   (make-instance 'small-bowl) (make-instance 'small-bowl) (make-instance 'small-bowl)
                                   (make-instance 'small-bowl) (make-instance 'small-bowl) (make-instance 'small-bowl)
                                     
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                   (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)

                                   (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)
                                   (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)
                                   (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)

                                   (make-instance 'cooking-pot) (make-instance 'cooking-pot) (make-instance 'cooking-pot)
                                   (make-instance 'frying-pan) (make-instance 'frying-pan) (make-instance 'frying-pan)

                                   (make-instance 'jar) (make-instance 'jar) (make-instance 'jar)

                                   ;; lids & wrapping
                                   (make-instance 'small-bowl-lid) (make-instance 'small-bowl-lid) (make-instance 'small-bowl-lid)
                                   (make-instance 'medium-bowl-lid) (make-instance 'medium-bowl-lid) (make-instance 'medium-bowl-lid)
                                   (make-instance 'large-bowl-lid) (make-instance 'large-bowl-lid) (make-instance 'large-bowl-lid)
                                   (make-instance 'jar-lid) (make-instance 'jar-lid) (make-instance 'jar-lid)
                                   (make-instance 'plastic-wrap)

                                   ;; tools
                                   (make-instance 'fork) (make-instance 'fork) (make-instance 'fork)
                                   (make-instance 'fork) (make-instance 'fork) (make-instance 'fork)
                                   (make-instance 'fork) (make-instance 'fork) (make-instance 'fork)

                                   (make-instance 'table-spoon) (make-instance 'table-spoon) (make-instance 'table-spoon)
                                   (make-instance 'table-spoon) (make-instance 'table-spoon) (make-instance 'table-spoon)
                                   (make-instance 'table-spoon) (make-instance 'table-spoon) (make-instance 'table-spoon)

                                   (make-instance 'tea-spoon) (make-instance 'tea-spoon) (make-instance 'tea-spoon)
                                   (make-instance 'tea-spoon) (make-instance 'tea-spoon) (make-instance 'tea-spoon)
                                   (make-instance 'tea-spoon) (make-instance 'tea-spoon) (make-instance 'tea-spoon)

                                   (make-instance 'knife) (make-instance 'knife) (make-instance 'knife)
                                   (make-instance 'knife) (make-instance 'knife) (make-instance 'knife)
                                   (make-instance 'knife) (make-instance 'knife) (make-instance 'knife)

                                   (make-instance 'bread-knife) (make-instance 'bread-knife) (make-instance 'bread-knife)

                                   (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                   (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                   (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)

                                   (make-instance 'wooden-spoon) (make-instance 'wooden-spoon) (make-instance 'wooden-spoon)
                                   (make-instance 'wooden-spoon) (make-instance 'wooden-spoon) (make-instance 'wooden-spoon)
                                   (make-instance 'wooden-spoon) (make-instance 'wooden-spoon) (make-instance 'wooden-spoon)

                                   (make-instance 'spatula) (make-instance 'spatula) (make-instance 'spatula)
                                                                                 
                                   (make-instance 'sift) (make-instance 'sift) (make-instance 'sift)

                                   (make-instance 'egg-separator) (make-instance 'egg-separator) (make-instance 'egg-separator)
                                   
                                   (make-instance 'colander) (make-instance 'colander) (make-instance 'colander)

                                   (make-instance 'mixer) (make-instance 'mixer) (make-instance 'mixer)

                                   (make-instance 'food-processor)

                                   ;; baking equipment
                                   (make-instance 'rolling-pin) (make-instance 'rolling-pin) (make-instance 'rolling-pin)
                                   (make-instance 'baking-paper) (make-instance 'baking-paper) (make-instance 'baking-paper)
                                   (make-instance 'paper-baking-cups :items
                                                  (list (make-instance 'paper-baking-cup) (make-instance 'paper-baking-cup) (make-instance 'paper-baking-cup)
                                                        (make-instance 'paper-baking-cup) (make-instance 'paper-baking-cup) (make-instance 'paper-baking-cup)
                                                        (make-instance 'paper-baking-cup) (make-instance 'paper-baking-cup) (make-instance 'paper-baking-cup)
                                                        (make-instance 'paper-baking-cup) (make-instance 'paper-baking-cup) (make-instance 'paper-baking-cup)
                                                        (make-instance 'paper-baking-cup) (make-instance 'paper-baking-cup) (make-instance 'paper-baking-cup)))                     
                                   (make-instance 'wire-rack)
                                   (make-instance 'pan)
                                   (make-instance 'cookie-sheet)
                                   (make-instance 'muffin-tins-12)
                                   (make-instance 'baking-tray))))))

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

(activate-monitor trace-irl)

#|
(defparameter *almond-crescent-cookies-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'almond-crescent-cookies
                 :kitchen-state
                 *full-kitchen*
                 :meaning-network
                 (list '(get-kitchen ?kitchen)
                       '(fetch-and-proportion ?proportioned-butter ?ks-with-butter ?kitchen ?target-container-1 butter 230 g)
                       '(bring-to-temperature ?warm-butter ?ks-with-warm-butter ?ks-with-butter ?proportioned-butter ?room-temp-quantity ?room-temp-unit)
                       '(fetch-and-proportion ?proportioned-sugar ?ks-with-sugar ?ks-with-warm-butter ?target-container-2 white-sugar 120 g)
                       '(fetch-and-proportion ?proportioned-vanilla ?ks-with-vanilla ?ks-with-sugar ?target-container-3 vanilla-extract 1 teaspoon)
                       '(fetch-and-proportion ?proportioned-almond ?ks-with-almond ?ks-with-vanilla ?target-container-4 almond-extract 1 teaspoon)
                       '(fetch-and-proportion ?proportioned-flour ?ks-with-flour ?ks-with-almond ?target-container-5 all-purpose-flour 340 g)
                       '(fetch-and-proportion ?proportioned-almond-flour ?ks-with-almond-flour ?ks-with-flour ?target-container-6 almond-flour 120 g)
                       '(fetch-and-proportion ?proportioned-powdered-sugar ?ks-with-powdered-sugar ?ks-with-almond-flour ?target-container-7 powdered-white-sugar 30 g)
                       '(transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-powdered-sugar ?empty-container-a ?warm-butter ?quantity-a ?unit-a)
                       '(transfer-contents ?output-container-b ?rest-b ?output-ks-b ?output-ks-a ?output-container-a ?proportioned-sugar ?quantity-b ?unit-b)
                       '(beat ?beaten-mixture ?ks-with-beaten-mixture ?output-ks-b ?output-container-b ?mixing-tool)
                       '(transfer-contents ?output-container-c ?rest-c ?output-ks-c ?ks-with-beaten-mixture ?beaten-mixture ?proportioned-vanilla ?quantity-c ?unit-c)
                       '(transfer-contents ?output-container-d ?rest-d ?output-ks-d ?output-ks-c ?output-container-c ?proportioned-almond ?quantity-d ?unit-d)
                       '(mix ?intermediate-mixture ?ks-with-intermediate-mixture ?output-ks-d ?output-container-d ?mixing-tool)
                       '(transfer-contents ?output-container-e ?rest-e ?output-ks-e ?ks-with-intermediate-mixture ?intermediate-mixture ?proportioned-flour ?quantity-e ?unit-e)
                       '(transfer-contents ?output-container-f ?rest-f ?output-ks-f ?output-ks-e ?intermediate-mixture ?proportioned-almond-flour ?quantity-f ?unit-f)
                       '(mix ?dough ?ks-with-dough ?output-ks-f ?output-container-f ?mixing-tool)
                       '(portion-and-arrange ?portioned-dough ?ks-with-dough-portions ?ks-with-dough ?dough 25 g ?pattern ?countertop)
                       '(shape ?bakeable-balls ?ks-with-balls ?ks-with-dough-portions ?portioned-dough ball-shape)
                       '(shape ?bakeable-crescents ?ks-with-crescents ?ks-with-balls ?bakeable-balls crescent-shape)
                       '(fetch ?baking-tray ?ks-with-baking-tray ?ks-with-crescents baking-tray 1)
                       '(fetch ?baking-paper ?ks-with-baking-paper ?ks-with-baking-tray baking-paper 1)
                       '(line ?lined-baking-tray ?ks-with-lined-tray ?ks-with-baking-paper ?baking-tray ?baking-paper)
                       '(transfer-items ?tray-with-crescents ?ks-with-crescents-tray ?ks-with-lined-tray ?bakeable-crescents ?default-pattern ?lined-baking-tray)
                       '(bake ?baked-crescents ?ks-with-baked-crescents ?ks-with-crescents-tray ?tray-with-crescents ?oven 15 minute 175 degrees-celsius)
                       '(sprinkle ?almond-crescent-cookies ?ks-with-almond-crescent-cookies ?ks-with-baked-crescents ?baked-crescents ?proportioned-powdered-sugar))
                 :primary-output-var
                 '?almond-crescent-cookies))

(defparameter *afghan-biscuits-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'afghan-biscuits
                 :kitchen-state
                 *full-kitchen*                                    
                 :meaning-network
                 (list '(get-kitchen ?kitchen-state)
                       '(fetch-and-proportion ?proportioned-butter ?kitchen-state-with-butter ?kitchen-state ?target-container-1 butter 200 g)
                       '(bring-to-temperature ?butter-at-room-temp ?kitchen-state-with-butter-at-room-temp ?kitchen-state-with-butter ?proportioned-butter ?room-temp-quantity ?room-temp-unit)
                       '(fetch-and-proportion ?proportioned-caster-sugar ?kitchen-state-with-caster-sugar ?kitchen-state-with-butter-at-room-temp ?target-container-2 caster-sugar 100 g)
                       '(fetch-and-proportion ?proportioned-all-purpose-flour ?kitchen-state-with-all-purpose-flour ?kitchen-state-with-caster-sugar ?target-container-3 all-purpose-flour 300 g)
                       '(fetch-and-proportion ?proportioned-cocoa-powder ?kitchen-state-with-cocoa-powder ?kitchen-state-with-all-purpose-flour ?target-container-4 cocoa-powder 3 tablespoon)
                       '(fetch-and-proportion ?proportioned-corn-flakes ?kitchen-state-with-corn-flakes ?kitchen-state-with-cocoa-powder ?target-container-5 corn-flakes 300 g)
                       '(fetch-and-proportion ?proportioned-icing-sugar ?kitchen-state-with-icing-sugar ?kitchen-state-with-corn-flakes ?target-container-6 powdered-white-sugar 200 g)
                       '(fetch-and-proportion ?proportioned-icing-cocoa-powder ?kitchen-state-with-icing-cocoa-powder ?kitchen-state-with-icing-sugar ?target-container-7 cocoa-powder 30  g)
                       '(fetch-and-proportion ?proportioned-water ?kitchen-state-with-water ?kitchen-state-with-icing-cocoa-powder ?target-container-8 water 3 tablespoon)
                       '(fetch-and-proportion ?proportioned-almonds ?kitchen-state-with-almonds ?kitchen-state-with-water ?target-container-9 almond-flakes 50 g)
                       '(preheat-oven ?preheated-oven ?kitchen-state-with-preheated-oven ?kitchen-state-with-almonds ?oven 180 degrees-celsius)
                       '(fetch ?baking-tray ?kitchen-state-with-baking-tray ?kitchen-state-with-preheated-oven baking-tray 1)
                       '(fetch ?baking-paper ?kitchen-state-with-baking-paper ?kitchen-state-with-baking-tray baking-paper 1)
                       '(line ?lined-baking-tray ?kitchen-state-with-lined-baking-tray ?kitchen-state-with-baking-paper ?baking-tray ?baking-paper)
                       '(transfer-contents ?output-container-x ?rest-x ?output-kitchen-state-x ?kitchen-state-with-lined-baking-tray ?target-container-10 ?butter-at-room-temp ?quantity-x ?unit-x)
                       '(transfer-contents ?output-container-y ?rest-y ?output-kitchen-state-y ?output-kitchen-state-x ?output-container-x ?proportioned-caster-sugar ?quantity-y ?unit-y)
                       '(beat ?container-with-creamed-butter ?kitchen-state-with-creamed-butter ?output-kitchen-state-y ?output-container-y ?beating-tool)
                       '(sift ?container-with-sifted-flour ?kitchen-state-with-sifted-flour ?kitchen-state-with-creamed-butter
          ?target-container-11 ?proportioned-all-purpose-flour ?sifting-tool)
                       '(sift ?container-with-sifted-ingredients ?kitchen-state-with-sifted-ingredients ?kitchen-state-with-sifted-flour
          ?container-with-sifted-flour ?proportioned-cocoa-powder ?sifting-tool)
                       '(transfer-contents ?container-with-flour-cocoa-and-butter ?rest-z ?kitchen-state-with-flour-cocoa-and-butter-in-bowl
                       ?kitchen-state-with-sifted-ingredients ?container-with-creamed-butter ?container-with-sifted-ingredients ?quantity-z ?unit-z)
                       '(fetch ?wooden-spoon ?kitchen-state-with-wooden-spoon ?kitchen-state-with-flour-cocoa-and-butter-in-bowl wooden-spoon 1)
                       '(mix ?flour-cocoa-butter-mixture ?kitchen-state-with-flour-cocoa-butter-mixture ?kitchen-state-with-wooden-spoon ?container-with-flour-cocoa-and-butter ?wooden-spoon)
                       '(transfer-contents ?container-with-cornflakes-added ?rest-a ?kitchen-state-with-cornflakes-in-bowl ?kitchen-state-with-flour-cocoa-butter-mixture ?flour-cocoa-butter-mixture ?proportioned-corn-flakes ?quantity-a ?unit-a)
                       '(mix ?flour-cocoa-butter-cornflakes-mixture ?kitchen-state-with-cornflakes-mixture ?kitchen-state-with-cornflakes-in-bowl ?container-with-cornflakes-added ?mixing-tool)
                       '(portion-and-arrange ?portioned-dough ?kitchen-state-with-portions-on-countertop ?kitchen-state-with-cornflakes-mixture ?flour-cocoa-butter-cornflakes-mixture 30 g ?default-pattern ?countertop)
                       '(shape ?dough-balls ?kitchen-state-with-doughballs ?kitchen-state-with-portions-on-countertop ?portioned-dough ball-shape)
                       '(flatten ?flattened-dough-balls ?kitchen-state-with-flattened-doughballs ?kitchen-state-with-doughballs ?dough-balls ?rolling-pin)
                       '(transfer-items ?cookies-on-tray ?kitchen-state-with-cookies-on-tray ?kitchen-state-with-flattened-doughballs ?flattened-dough-balls 5-cm-apart ?lined-baking-tray)
                       '(bake ?baked-cookies ?kitchen-state-with-baking-cookies ?kitchen-state-with-cookies-on-tray ?cookies-on-tray ?preheated-oven 15 minute ?temp-quantity ?temp-unit)
                       '(fetch ?wire-rack ?kitchen-state-with-wire-rack ?kitchen-state-with-baking-cookies wire-rack 1)
                       '(transfer-items ?cookies-on-wire-rack ?kitchen-state-with-cookies-on-wire-rack ?kitchen-state-with-wire-rack ?baked-cookies ?default-pattern-2 ?wire-rack)
                       '(bring-to-temperature ?cooled-cookies ?kitchen-state-with-cooling-cookies ?kitchen-state-with-cookies-on-wire-rack ?cookies-on-wire-rack ?room-temp-quantity ?room-temp-unit)
                       '(fetch ?large-bowl ?kitchen-state-with-bowl ?kitchen-state-with-cooling-cookies large-bowl 1)
                       '(transfer-contents ?container-for-icing-with-sugar ?rest-b ?kitchen-state-with-container-for-icing-with-sugar ?kitchen-state-with-bowl ?large-bowl ?proportioned-icing-sugar ?quantity-b ?unit-b)
                       '(transfer-contents ?container-for-icing-with-sugar-and-cocoa ?rest-c ?kitchen-state-with-container-for-icing-with-sugar-and-cocoa ?kitchen-state-with-container-for-icing-with-sugar ?container-for-icing-with-sugar ?proportioned-icing-cocoa-powder ?quantity-c ?unit-c)
                       '(transfer-contents ?container-for-icing-with-sugar-cocoa-and-water ?rest-d ?kitchen-state-with-container-for-icing-with-sugar-cocoa-and-water ?kitchen-state-with-container-for-icing-with-sugar-and-cocoa ?container-for-icing-with-sugar ?proportioned-water ?quantity-d ?unit-d)
                       '(mix ?icing ?kitchen-with-icing-ready ?kitchen-state-with-container-for-icing-with-sugar-cocoa-and-water ?container-for-icing-with-sugar-cocoa-and-water ?mixing-tool)
                       '(fetch ?table-spoon ?kitchen-state-with-table-spoon ?kitchen-with-icing-ready table-spoon 1)
                       '(spread ?iced-cookies ?kitchen-state-with-iced-cookies ?kitchen-state-with-table-spoon  ?cooled-cookies ?icing ?table-spoon)
                       '(sprinkle ?sprinkled-cookies ?kitchen-state-with-sprinkled-cookies ?kitchen-state-with-iced-cookies ?iced-cookies ?proportioned-almonds))
                 :primary-output-var '?sprinkled-cookies))

(defparameter *best-brownies-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'best-brownies
                 :kitchen-state 
                 *full-kitchen*
                 :meaning-network
                 (list '(get-kitchen ?kitchen-state)
                       '(fetch-and-proportion ?proportioned-butter ?kitchen-state-with-butter ?kitchen-state ?new-container-1 butter 120 g)
                       '(melt ?melted-butter ?kitchen-state-with-melted-butter ?kitchen-state-with-butter ?proportioned-butter ?microwave)
                       '(fetch-and-proportion ?proportioned-sugar ?kitchen-state-with-sugar ?kitchen-state-with-melted-butter ?new-container-2 white-sugar 200 g)
                       '(fetch-and-proportion ?proportioned-eggs ?kitchen-state-with-eggs ?kitchen-state-with-sugar ?new-container-3 egg 2 piece)
                       '(fetch-and-proportion ?proportioned-flour ?kitchen-state-with-flour ?kitchen-state-with-eggs ?new-container-4 all-purpose-flour 70 g)
                       '(fetch-and-proportion ?proportioned-cocoa ?kitchen-state-with-cocoa ?kitchen-state-with-flour ?new-container-5 cocoa-powder 45 g)
                       '(fetch-and-proportion ?proportioned-salt ?kitchen-state-with-salt ?kitchen-state-with-cocoa ?new-container-6 salt 0.25 teaspoon)
                       '(fetch-and-proportion ?proportioned-vanilla ?kitchen-state-with-vanilla ?kitchen-state-with-salt ?new-container-7 vanilla-extract 1 teaspoon)
                       '(fetch-and-proportion ?proportioned-walnuts ?kitchen-state-with-walnuts ?kitchen-state-with-vanilla ?new-container-8 walnut 50 g)
                       '(cut ?chopped-walnuts ?kitchen-state-with-chopped-walnuts ?kitchen-state-with-walnuts ?proportioned-walnuts chopped ?knife)
                       '(preheat-oven ?preheated-oven ?kitchen-state-with-preheated-oven ?kitchen-state-with-chopped-walnuts ?oven 175 degrees-celsius)
                       '(fetch ?pan ?kitchen-state-with-pan ?kitchen-state-with-preheated-oven pan 1)
                       '(grease ?greased-pan ?kitchen-state-with-greased-pan ?kitchen-state-with-pan ?pan ?grease)
                       '(flour ?floured-pan ?kitchen-state-with-floured-pan ?kitchen-state-with-greased-pan ?greased-pan ?all-purpose-flour)
                       '(fetch ?medium-bowl-1 ?kitchen-state-with-medium-bowl ?kitchen-state-with-floured-pan medium-bowl 1)
                       '(transfer-contents ?output-container-x ?rest-x ?output-kitchen-state-x ?kitchen-state-with-medium-bowl ?medium-bowl-1 ?melted-butter ?quantity-x ?unit-x)
                       '(transfer-contents ?output-container-y ?rest-y ?output-kitchen-state-y ?output-kitchen-state-x ?output-container-x ?proportioned-sugar ?quantity-y ?unit-y)
                       '(beat ?beaten-mixture-bowl ?kitchen-state-with-beaten-mixture ?output-kitchen-state-y ?output-container-y ?beating-tool)
                       '(crack ?mixture-with-cracked-eggs ?kitchen-state-with-cracked-eggs ?kitchen-state-with-beaten-mixture ?proportioned-eggs ?beaten-mixture-bowl)
                       '(mix ?egg-sugar-mixture ?kitchen-state-with-egg-sugar-mixture ?kitchen-state-with-cracked-eggs ?mixture-with-cracked-eggs ?beating-tool)
                       '(transfer-contents ?output-container-z ?rest-z ?output-kitchen-state-z ?kitchen-state-with-egg-sugar-mixture ?egg-sugar-mixture ?proportioned-flour ?quantity-z ?unit-z)
                       '(transfer-contents ?output-container-a ?rest-a ?output-kitchen-state-a ?output-kitchen-state-z ?output-container-z ?proportioned-cocoa ?quantity-a ?unit-a)
                       '(transfer-contents ?output-container-b ?rest-b ?output-kitchen-state-b ?output-kitchen-state-a ?output-container-a ?proportioned-salt ?quantity-b ?unit-b)
                       '(mix ?flour-sugar-mixture-bowl ?kitchen-state-with-flour-sugar-mixture ?output-kitchen-state-b ?output-container-b ?beating-tool)
                       '(transfer-contents ?output-container-c ?rest-c ?output-kitchen-state-c ?kitchen-state-with-flour-sugar-mixture ?flour-sugar-mixture-bowl ?proportioned-vanilla ?quantity-c ?unit-c)
                       '(transfer-contents ?output-container-d ?rest-d ?output-kitchen-state-d ?output-kitchen-state-c ?output-container-c ?chopped-walnuts ?quantity-d ?unit-d)
                       '(mix ?dough ?kitchen-state-with-dough ?output-kitchen-state-d ?output-container-d ?beating-tool)
                       '(spread ?pan-with-dough ?kitchen-state-with-dough-in-pan ?kitchen-state-with-dough ?floured-pan ?dough ?scraper)
                       '(bake ?baked-brownie ?kitchen-state-with-baked-brownie ?kitchen-state-with-dough-in-pan ?pan-with-dough ?preheated-oven 25 minute ?temp-quantity ?temp-unit)
                       '(bring-to-temperature ?cooled-brownie ?kitchen-state-with-cooled-brownie ?kitchen-state-with-baked-brownie ?baked-brownie ?room-temp-quantity ?room-temp-unit)
                       '(cut ?cut-brownie ?kitchen-state-with-cut-brownie ?kitchen-state-with-cooled-brownie ?cooled-brownie squares ?knife))
                 :primary-output-var '?cut-brownie))

(defparameter *chocolate-fudge-cookies-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'chocolate-fudge-cookies
                 :kitchen-state
                 *full-kitchen*
                 :meaning-network
                 (list '(get-kitchen ?kitchen-state)
                       '(fetch-and-proportion ?proportioned-devils-food-cake-mix ?kitchen-state-with-devils-food-cake-mix ?kitchen-state ?target-container-1 devils-food-cake-mix 500 g)
                       '(fetch-and-proportion ?proportioned-eggs ?kitchen-state-with-eggs ?kitchen-state-with-devils-food-cake-mix ?target-container-2 egg 2 piece)
                       '(fetch-and-proportion ?proportioned-vegetable-oil ?kitchen-state-with-vegetable-oil ?kitchen-state-with-eggs ?target-container-3 vegetable-oil 125 ml)
                       '(fetch-and-proportion ?proportioned-semisweet-chocolate-chips ?kitchen-state-with-semisweet-chocolate-chips ?kitchen-state-with-vegetable-oil ?target-container-4 semisweet-chocolate-chips 160 g)
                       '(preheat-oven ?preheated-oven ?kitchen-state-with-preheated-oven ?kitchen-state-with-semisweet-chocolate-chips ?oven 175 degrees-celsius)
                       '(fetch ?cookie-sheet ?kitchen-state-with-cookie-sheet ?kitchen-state-with-preheated-oven cookie-sheet 1)
                       '(grease ?greased-sheet ?kitchen-state-with-greased-sheet ?kitchen-state-with-cookie-sheet ?cookie-sheet ?grease)
                       '(fetch ?medium-bowl-1 ?kitchen-state-with-medium-bowl ?kitchen-state-with-greased-sheet medium-bowl 1)
                       '(transfer-contents ?output-container-x ?rest-x ?output-kitchen-state-x ?kitchen-state-with-medium-bowl ?medium-bowl-1 ?proportioned-devils-food-cake-mix ?quantity-x ?unit-x)
                       '(crack ?output-container-y ?output-kitchen-state-y ?output-kitchen-state-x ?proportioned-eggs ?medium-bowl-1)
                       '(transfer-contents ?output-container-z ?rest-z ?output-kitchen-state-z ?output-kitchen-state-y ?output-container-y ?proportioned-vegetable-oil ?quantity-z ?unit-z)
                       '(mix ?stirred-mixture-bowl ?kitchen-state-with-stirred-mixture ?output-kitchen-state-z ?output-container-z ?mixing-tool)
                       '(transfer-contents ?output-container-with-chips ?rest-chips ?kitchen-state-with-folded-chips ?kitchen-state-with-stirred-mixture ?stirred-mixture-bowl ?proportioned-semisweet-chocolate-chips ?quantity-chips ?unit-chips)
                       '(mix ?chips-mixture-bowl ?kitchen-state-with-chips-mixture ?output-kitchen-state-z ?output-container-with-chips ?mixing-tool)
                       '(portion-and-arrange ?portioned-dough ?kitchen-state-with-portions ?kitchen-state-with-chips-mixture ?chips-mixture-bowl 20 g ?default-pattern ?countertop)
                       '(shape ?shaped-bakeables ?ks-with-dough-balls ?kitchen-state-with-portions ?portioned-dough ball-shape)
                       '(transfer-items ?cookies-on-sheet ?ks-with-dough-on-sheet ?ks-with-dough-balls ?shaped-bakeables 5-cm-apart ?greased-sheet)
                       '(bake ?baked-cookies-on-sheet ?kitchen-state-with-cookies ?ks-with-dough-on-sheet ?cookies-on-sheet ?preheated-oven 8 minute ?bake-quantity ?bake-unit)
                       '(leave-for-time ?cooling-cookies ?kitchen-state-with-cooling-cookies ?kitchen-state-with-cookies ?baked-cookies-on-sheet 5 minute)
                       '(fetch ?wire-rack ?kitchen-state-with-wire-rack ?kitchen-state-with-cookies wire-rack 1)
                       '(transfer-items ?cookies-on-wire-rack ?kitchen-state-with-cookies-on-wire-rack ?kitchen-state-with-wire-rack ?cooling-cookies ?default-pattern-2 ?wire-rack)
                       '(bring-to-temperature ?cooled-cookies ?kitchen-state-with-cooled-cookies ?kitchen-state-with-cookies-on-wire-rack ?cookies-on-wire-rack ?room-temp-quantity ?room-temp-unit))
                 :primary-output-var '?cooled-cookies))

(defparameter *easy-banana-bread-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'easy-banana-bread
                 :kitchen-state
                 *full-kitchen*
                 :meaning-network
                 (list '(get-kitchen ?kitchen-state)
                       '(fetch-and-proportion ?proportioned-butter ?kitchen-state-with-butter ?kitchen-state ?target-container-1 butter 60 g)
                       '(fetch-and-proportion ?proportioned-eggs ?kitchen-state-with-eggs ?kitchen-state-with-butter ?target-container-2 egg 2 piece)
                       '(fetch-and-proportion ?proportioned-sugar ?kitchen-state-with-sugar ?kitchen-state-with-eggs ?target-container-3 sugar 200 g)
                       '(fetch-and-proportion ?proportioned-bananas ?kitchen-state-with-bananas ?kitchen-state-with-sugar ?target-container-4 banana 3 piece)
                       '(mash ?mashed-bananas ?kitchen-state-with-mashed-bananas ?kitchen-state-with-bananas ?proportioned-bananas ?fork)
                       '(fetch-and-proportion ?proportioned-vanilla ?kitchen-state-with-vanilla ?kitchen-state-with-mashed-bananas ?target-container-5 vanilla-extract 1 teaspoon)
                       '(fetch-and-proportion ?proportioned-self-rising-flour ?kitchen-state-with-self-rising-flour ?kitchen-state-with-vanilla ?target-container-6 self-rising-flour 200 g)
                       '(transfer-contents ?output-container-x ?rest-x ?output-kitchen-state-x ?kitchen-state-with-self-rising-flour ?target-container-7 ?proportioned-butter ?quantity-x ?unit-x)
                       '(crack ?output-container-y ?output-kitchen-state-y ?output-kitchen-state-x ?proportioned-eggs ?output-container-x)
                       '(transfer-contents ?output-container-z ?rest-z ?output-kitchen-state-z ?output-kitchen-state-y ?output-container-y ?proportioned-sugar ?quantity-z ?unit-z)
                       '(beat ?creamed-mixture ?kitchen-state-with-creamed-mixture ?output-kitchen-state-z ?output-container-z ?beating-tool)
                       '(transfer-contents ?output-container-a ?rest-a ?output-kitchen-state-a ?kitchen-state-with-creamed-mixture ?creamed-mixture ?mashed-bananas ?quantity-a ?unit-a)
                       '(transfer-contents ?output-container-b ?rest-b ?output-kitchen-state-b ?output-kitchen-state-a ?output-container-a ?proportioned-vanilla ?quantity-b ?unit-b)
                       '(beat ?beaten-mixture ?kitchen-state-with-beaten-mixture ?output-kitchen-state-b ?output-container-b ?beating-tool)
                       '(transfer-contents ?output-container-c ?rest-c ?output-kitchen-state-c ?kitchen-state-with-beaten-mixture ?beaten-mixture ?proportioned-self-rising-flour ?quantity-c ?unit-c)
                       '(mix ?banana-bread-batter ?kitchen-state-with-banana-bread-batter ?output-kitchen-state-c ?output-container-c ?beating-tool)
                       '(fetch ?pan ?kitchen-state-with-pan ?kitchen-state-with-banana-bread-batter pan 1)
                       '(grease ?greased-pan ?kitchen-state-with-greased-pan ?kitchen-state-with-pan ?pan ?grease)
                       '(spread ?pan-with-batter ?kitchen-state-with-batter-in-pan ?kitchen-state-with-greased-pan ?greased-pan ?banana-bread-batter ?scraper)
                       '(bake ?baked-banana-bread ?kitchen-state-with-baked-banana-bread ?kitchen-state-with-batter-in-pan ?pan-with-batter ?oven 60 minute 165 degrees-celsius))
                 :primary-output-var '?baked-banana-bread))

(defparameter *easy-oatmeal-cookies-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'easy-oatmeal-cookies
                 :kitchen-state
                 *full-kitchen*
                 :meaning-network
                 (list '(get-kitchen ?kitchen)
                       '(fetch-and-proportion ?proportioned-raisins ?kitchen-state-with-raisins ?kitchen ?target-container-1 raisin 150 g)
                       '(fetch-and-proportion ?proportioned-water ?kitchen-state-with-water ?kitchen-state-with-raisins ?target-container-2 water 125 ml)
                       '(bring-to-temperature ?hot-water ?kitchen-state-with-hot-water ?kitchen-state-with-water ?proportioned-water 60 degrees-celsius)
                       '(fetch-and-proportion ?proportioned-flour ?kitchen-state-with-flour ?kitchen-state-with-hot-water ?target-container-3 all-purpose-flour 280 g)
                       '(fetch-and-proportion ?proportioned-baking-soda ?kitchen-state-with-baking-soda ?kitchen-state-with-flour ?target-container-4 baking-soda 1 teaspoon)
                       '(fetch-and-proportion ?proportioned-salt ?kitchen-state-with-salt ?kitchen-state-with-baking-soda ?target-container-5 salt 1 teaspoon)
                       '(fetch-and-proportion ?proportioned-oats ?kitchen-state-with-oats ?kitchen-state-with-salt ?target-container-6 oats 160 g)
                       '(fetch-and-proportion ?proportioned-cinnamon ?kitchen-state-with-cinnamon ?kitchen-state-with-oats ?target-container-7 ground-cinnamon 1 teaspoon)
                       '(fetch-and-proportion ?proportioned-nutmeg ?kitchen-state-with-nutmeg ?kitchen-state-with-cinnamon ?target-container-8 ground-nutmeg 1 teaspoon)
                       '(fetch-and-proportion ?proportioned-sugar ?kitchen-state-with-sugar ?kitchen-state-with-nutmeg ?target-container-9 brown-sugar 200 g)
                       '(fetch-and-proportion ?proportioned-walnuts ?kitchen-state-with-walnuts ?kitchen-state-with-sugar ?target-container-10 walnut 75 g)
                       '(cut ?chopped-walnuts ?kitchen-state-with-chopped-walnuts ?kitchen-state-with-walnuts ?proportioned-walnuts chopped ?knife)
                       '(fetch-and-proportion ?proportioned-eggs ?kitchen-state-with-eggs ?kitchen-state-with-chopped-walnuts ?target-container-11 egg 2 piece)
                       '(fetch-and-proportion ?proportioned-oil ?kitchen-state-with-oil ?kitchen-state-with-eggs ?target-container-12 vegetable-oil 200 ml)
                       '(fetch-and-proportion ?proportioned-vanilla ?kitchen-state-with-vanilla ?kitchen-state-with-oil ?target-container-13 vanilla-extract 1 teaspoon)
                       '(preheat-oven ?preheated-oven ?kitchen-state-with-preheating-oven ?kitchen-state-with-vanilla ?oven 175 degrees-celsius)
                       '(transfer-contents ?container-with-soaked-raisins ?empty-raisin-bowl ?kitchen-state-with-soaking-raisins ?kitchen-state-with-preheating-oven ?hot-water ?proportioned-raisins ?quantity ?unit)
                       '(transfer-contents ?flour-with-soda ?empty-soda-bowl ?kitchen-state-with-flour-and-soda ?kitchen-state-with-soaking-raisins ?proportioned-flour ?proportioned-baking-soda ?quantity-1 ?unit-1)
                       '(transfer-contents ?flour-with-soda-and-salt ?empty-salt-bowl ?kitchen-state-with-flour-soda-and-salt ?kitchen-state-with-flour-and-soda ?flour-with-soda ?proportioned-salt ?quantity-2 ?unit-2)
                       '(transfer-contents ?flour-soda-salt-cinnamon ?empty-cinnamon-bowl ?kitchen-state-with-flour-soda-salt-cinnamon ?kitchen-state-with-flour-soda-and-salt ?flour-with-soda-and-salt ?proportioned-cinnamon ?quantity-3 ?unit-3)
                       '(transfer-contents ?flour-soda-salt-cinnamon-nutmeg ?empty-nutmeg-bowl ?kitchen-state-with-flour-soda-salt-cinnamon-nutmeg ?kitchen-state-with-flour-soda-salt-cinnamon ?flour-soda-salt-cinnamon ?proportioned-nutmeg ?quantity-4 ?unit-4)
                       '(fetch ?large-bowl ?kitchen-state-with-fetched-bowl-for-sifting ?kitchen-state-with-flour-soda-salt-cinnamon-nutmeg large-bowl 1)
                       '(sift ?bowl-with-sifted-ingredients ?kitchen-state-after-sifting ?kitchen-state-with-fetched-bowl-for-sifting ?large-bowl ?flour-soda-salt-cinnamon-nutmeg ?sifting-tool)
                       '(transfer-contents ?output-container-x ?rest-x ?output-kitchen-state-x ?kitchen-state-after-sifting ?bowl-with-sifted-ingredients ?proportioned-oats ?quantity-x ?unit-x)
                       '(transfer-contents ?output-container-y ?rest-y ?output-kitchen-state-y ?output-kitchen-state-x ?output-container-x ?proportioned-sugar ?quantity-y ?unit-y)
                       '(transfer-contents ?output-container-z ?rest-z ?output-kitchen-state-z ?output-kitchen-state-y ?output-container-y ?chopped-walnuts ?quantity-z ?unit-z)
                       '(mix ?blended-in-oats-mixture ?kitchen-state-with-blended-oats-in-mixture ?output-kitchen-state-z ?output-container-z ?mixing-tool)
                       '(fetch ?bowl-for-eggs ?kitchen-state-with-fetched-bowl-for-eggs ?kitchen-state-with-blended-oats-in-mixture medium-bowl 1)
                       '(crack ?container-w-cracked-eggs ?kitchen-state-with-cracked-eggs ?kitchen-state-with-fetched-bowl-for-eggs ?proportioned-eggs ?bowl-for-eggs)
                       '(fetch ?fetched-fork ?kitchen-state-with-fetched-fork ?kitchen-state-with-cracked-eggs fork 1)
                       '(beat ?container-w-beaten-eggs ?kitchen-state-w-beaten-eggs ?kitchen-state-with-fetched-fork ?container-w-cracked-eggs ?fetched-fork)
                       '(transfer-contents ?output-container-a ?rest-a ?output-kitchen-state-a ?kitchen-state-w-beaten-eggs ?container-w-beaten-eggs ?proportioned-oil ?quantity-a ?unit-a)
                       '(transfer-contents ?output-container-b ?rest-b ?output-kitchen-state-b ?output-kitchen-state-a ?output-container-a ?proportioned-vanilla ?quantity-b ?unit-b)
                       '(transfer-contents ?container-w-eggs-oil-vanilla-raisins ?rest-c ?output-kitchen-state-c ?output-kitchen-state-b ?output-container-b ?container-with-soaked-raisins ?quantity-c ?unit-c)
                       '(transfer-contents ?container-with-flour-and-mixture ?rest-d ?output-kitchen-state-d ?output-kitchen-state-c ?flour-soda-salt-cinnamon-nutmeg ?container-w-eggs-oil-vanilla-raisins ?quantity-d ?unit-d)
                       '(mix ?dough ?kitchen-state-with-dough ?output-kitchen-state-d ?container-with-flour-and-mixture ?mixing-tool)
                       '(fetch ?cookie-sheet ?kitchen-state-with-cookie-sheet ?kitchen-state-with-dough cookie-sheet 1)
                       '(portion-and-arrange ?portioned-dough ?kitchen-state-with-portions ?kitchen-state-with-cookie-sheet ?dough 1 tablespoon ?pattern ?countertop)
                       '(transfer-items ?sheet-with-dough ?kitchen-state-with-dough-on-sheet ?kitchen-state-with-portions ?portioned-dough 5-cm-apart ?cookie-sheet)
                       '(bake ?baked-cookies ?kitchen-state-with-baked-cookies ?kitchen-state-with-dough-on-sheet ?sheet-with-dough ?preheated-oven 10 minute ?bake-quantity ?bake-unit))
                 :primary-output-var '?baked-cookies))

(defparameter *whole-wheat-ginger-snaps-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'whole-wheat-ginger-snaps
                 :kitchen-state
                 *full-kitchen*
                 :meaning-network
                 (list '(get-kitchen ?kitchen-state)
                       '(fetch-and-proportion ?proportioned-butter ?kitchen-state-with-butter ?kitchen-state ?target-container-1 butter 225 g)
                       '(fetch-and-proportion ?proportioned-sugar ?kitchen-state-with-sugar ?kitchen-state-with-butter ?target-container-2 white-sugar 300 g)
                       '(fetch-and-proportion ?proportioned-eggs ?kitchen-state-with-eggs ?kitchen-state-with-sugar ?target-container-3 egg 2 piece)
                       '(crack ?container-w-cracked-eggs ?kitchen-state-with-cracked-eggs ?kitchen-state-with-eggs ?proportioned-eggs ?bowl-for-eggs)
                       '(beat ?beaten-eggs ?kitchen-state-with-beaten-eggs ?kitchen-state-with-cracked-eggs ?container-w-cracked-eggs ?beating-tool)
                       '(fetch-and-proportion ?proportioned-molasses ?kitchen-state-with-molasses ?kitchen-state-with-beaten-eggs ?target-container-4 molasses 280 g)
                       '(fetch-and-proportion ?proportioned-whole-wheat-flour ?kitchen-state-with-whole-wheat-flour ?kitchen-state-with-molasses ?target-container-5  whole-wheat-flour 480 g)
                       '(fetch-and-proportion ?proportioned-baking-soda ?kitchen-state-with-baking-soda ?kitchen-state-with-whole-wheat-flour ?target-container-6 baking-soda 1 tablespoon)
                       '(fetch-and-proportion ?proportioned-baking-powder ?kitchen-state-with-baking-powder ?kitchen-state-with-baking-soda ?target-container-7 baking-powder 2 teaspoon)
                       '(fetch-and-proportion ?proportioned-ground-ginger ?kitchen-state-with-ground-ginger ?kitchen-state-with-baking-powder ?target-container-8 ground-ginger 1 tablespoon)
                       '(fetch-and-proportion ?proportioned-ground-nutmeg ?kitchen-state-with-ground-nutmeg ?kitchen-state-with-ground-ginger ?target-container-9  ground-nutmeg 1.5 teaspoon)
                       '(fetch-and-proportion ?proportioned-ground-cinnamon ?kitchen-state-with-ground-cinnamon  ?kitchen-state-with-ground-nutmeg ?target-container-10  ground-cinnamon 1.5 teaspoon)
                       '(fetch-and-proportion ?proportioned-ground-cloves ?kitchen-state-with-ground-cloves ?kitchen-state-with-ground-cinnamon ?target-container-11  ground-cloves 1.5 teaspoon)
                       '(fetch-and-proportion ?proportioned-ground-allspice ?kitchen-state-with-ground-allspice ?kitchen-state-with-ground-cloves ?target-container-12  ground-allspice 1.5 teaspoon)
                       '(fetch-and-proportion ?proportioned-decoration-sugar ?kitchen-state-with-sugar-for-decoration ?kitchen-state-with-ground-allspice ?target-container-13  white-sugar 300 g)
                       '(preheat-oven ?preheated-oven ?kitchen-state-with-preheated-oven ?kitchen-state-with-sugar-for-decoration ?oven 175 degrees-celsius)
                       '(fetch ?cookie-sheet ?kitchen-state-with-cookie-sheet ?kitchen-state-with-preheated-oven cookie-sheet 1)
                       '(grease ?greased-sheet ?kitchen-state-with-greased-sheet ?kitchen-state-with-cookie-sheet ?cookie-sheet ?grease)
                       '(fetch ?large-bowl ?kitchen-state-with-fetched-bowl ?kitchen-state-with-greased-sheet large-bowl 1)
                       '(transfer-contents ?butter-in-large-bowl ?empty-butter-bowl ?kitchen-state-with-butter-in-large-bowl ?kitchen-state-with-fetched-bowl ?large-bowl ?proportioned-butter ?quantity-butter ?unit-butter)
                       '(transfer-contents ?butter-sugar-bowl ?empty-sugar-bowl ?kitchen-state-with-butter-sugar-mix ?kitchen-state-with-butter-in-large-bowl ?butter-in-large-bowl ?proportioned-sugar ?quantity-sugar ?unit-sugar)
                       '(beat ?butter-sugar-cream ?kitchen-state-with-creamed-mix ?kitchen-state-with-butter-sugar-mix ?butter-sugar-bowl ?mixing-tool)
                       '(transfer-contents ?mix-and-eggs ?empty-egg-bowl ?kitchen-state-with-eggs-in-mix ?kitchen-state-with-creamed-mix ?butter-sugar-cream ?beaten-eggs ?quantity-eggs ?unit-eggs)
                       '(transfer-contents ?mix-eggs-and-molasses ?empty-molasses-bowl ?kitchen-state-with-molasses-in-mix ?kitchen-state-with-eggs-in-mix ?mix-and-eggs ?proportioned-molasses ?quantity-molasses ?unit-molasses)
                       '(mix ?cream-eggs-molasses-mix ?kitchen-state-with-cream-eggs-molasses-mix ?kitchen-state-with-molasses-in-mix ?mix-eggs-and-molasses ?beating-tool)
                       '(transfer-contents ?output-container-a ?rest-a ?output-kitchen-state-a ?kitchen-state-with-cream-eggs-molasses-mix ?empty-container ?proportioned-whole-wheat-flour ?quantity-a ?unit-a)
                       '(transfer-contents ?output-container-b ?rest-b ?output-kitchen-state-b ?output-kitchen-state-a ?output-container-a ?proportioned-baking-soda ?quantity-b ?unit-b)
                       '(transfer-contents ?output-container-c ?rest-c ?output-kitchen-state-c ?output-kitchen-state-b ?output-container-b ?proportioned-baking-powder ?quantity-c ?unit-c)
                       '(transfer-contents ?output-container-d ?rest-d ?output-kitchen-state-d ?output-kitchen-state-c ?output-container-c ?proportioned-ground-ginger ?quantity-d ?unit-d)
                       '(transfer-contents ?output-container-e ?rest-e ?output-kitchen-state-e ?output-kitchen-state-d ?output-container-d ?proportioned-ground-nutmeg ?quantity-e ?unit-e)
                       '(transfer-contents ?output-container-f ?rest-f ?output-kitchen-state-f ?output-kitchen-state-e ?output-container-e ?proportioned-ground-cinnamon ?quantity-f ?unit-f)
                       '(transfer-contents ?output-container-g ?rest-g ?output-kitchen-state-g ?output-kitchen-state-f ?output-container-f ?proportioned-ground-cloves ?quantity-g ?unit-g)
                       '(transfer-contents ?output-container-h ?rest-h ?output-kitchen-state-h ?output-kitchen-state-g ?output-container-g ?proportioned-ground-allspice ?quantity-h ?unit-h)
                       '(mix ?dry-mixture ?kitchen-state-with-dry-mixture ?output-kitchen-state-h ?output-container-h ?mixing-tool)
                       '(transfer-contents ?molasses-dry-mixture ?rest-molasses-dry-mix ?kitchen-state-with-molasses-dry-mixture ?kitchen-state-with-dry-mixture ?cream-eggs-molasses-mix ?dry-mixture ?quantity-stir ?unit-sir)
                       '(mix ?dough ?kitchen-state-with-dough ?kitchen-state-with-molasses-dry-mixture ?molasses-dry-mixture ?mixing-tool)
                       '(portion-and-arrange ?portioned-dough ?kitchen-state-with-portions ?kitchen-state-with-dough ?dough 25 g ?pattern ?countertop)
                       '(shape ?shaped-bakeables ?kitchen-state-with-shaped-bakeables ?kitchen-state-with-portions ?portioned-dough ball-shape)
                       '(dip ?dipped-bakeables ?kitchen-state-with-dipped-bakeables ?kitchen-state-with-shaped-bakeables ?shaped-bakeables ?proportioned-decoration-sugar)
                       '(transfer-items ?bakeables-on-sheet ?kitchen-out-bakeables-on-sheet ?kitchen-state-with-dipped-bakeables ?dipped-bakeables 5-cm-apart ?greased-sheet)
                       '(bake ?baked-snaps ?kitchen-out-with-baked-snaps ?kitchen-out-bakeables-on-sheet ?bakeables-on-sheet ?preheated-oven 10 minute ?preheated-quantity ?preheated-unit)
                       '(fetch ?wire-rack ?kitchen-state-with-wire-rack ?kitchen-out-with-baked-snaps wire-rack 1)
                       '(transfer-items ?snaps-on-wire-rack ?kitchen-state-with-snaps-on-wire-rack ?kitchen-state-with-wire-rack ?baked-snaps ?default-pattern ?wire-rack)
                       '(bring-to-temperature ?cooled-snaps ?kitchen-state-with-cooled-snaps ?kitchen-state-with-snaps-on-wire-rack ?snaps-on-wire-rack ?room-temp-quantity ?room-temp-unit))
                 :primary-output-var '?cooled-snaps))

(defparameter *cucumber-slices-with-dill-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'cucumber-slices-with-dill
                 :kitchen-state
                 *full-kitchen*
                 :meaning-network
                 (list '(get-kitchen ?kitchen)
                       '(fetch-and-proportion ?proportioned-cucumbers ?kitchen-state-with-cucumbers ?kitchen ?target-container-1 cucumber 4 piece)
                       '(cut ?sliced-cucumbers ?kitchen-state-with-sliced-cucumbers ?kitchen-state-with-cucumbers ?proportioned-cucumbers slices ?knife)
                       '(fetch-and-proportion ?proportioned-onions ?kitchen-state-with-onions ?kitchen-state-with-sliced-cucumbers ?target-container-2 onion 1 piece)
                       '(cut ?sliced-onions ?kitchen-state-with-sliced-onions ?kitchen-state-with-onions ?proportioned-onions slices ?knife)
                       '(fetch-and-proportion ?proportioned-dill-weed ?kitchen-state-with-dill-weed ?kitchen-state-with-sliced-onions ?target-container-3 dried-dill-weed 1 tablespoon)
                       '(fetch-and-proportion ?proportioned-white-sugar ?kitchen-state-with-white-sugar ?kitchen-state-with-dill-weed ?target-container-4 white-sugar 200 g)
                       '(fetch-and-proportion ?proportioned-white-vinegar ?kitchen-state-with-white-vinegar ?kitchen-state-with-white-sugar ?target-container-5 white-vinegar 120 ml)
                       '(fetch-and-proportion ?proportioned-water ?kitchen-state-with-water ?kitchen-state-with-white-vinegar ?target-container-6 water 120 ml)
                       '(fetch-and-proportion ?proportioned-salt ?kitchen-state-with-salt ?kitchen-state-with-water ?target-container-7 salt 1 teaspoon)
                       '(fetch ?large-bowl ?kitchen-state-with-fetched-large-bowl ?kitchen-state-with-salt large-bowl 1)
                       '(transfer-contents ?output-a ?rest-a ?kitchen-out-a ?kitchen-state-with-fetched-large-bowl ?large-bowl ?proportioned-cucumbers ?quantity-a ?unit-a)
                       '(transfer-contents ?output-b ?rest-b ?kitchen-out-b ?kitchen-out-a ?output-a ?proportioned-onions ?quantity-b ?unit-b)
                       '(transfer-contents ?output-c ?rest-c ?kitchen-out-c ?kitchen-out-b ?output-b ?proportioned-dill-weed ?quantity-c ?unit-c)
                       '(mingle ?cucumber-mixture ?kitchen-state-with-cucumber-mixture ?kitchen-out-c ?output-c ?mingling-tool)
                       '(fetch ?medium-bowl ?kitchen-state-with-fetched-medium-bowl ?kitchen-state-with-cucumber-mixture medium-bowl 1)
                       '(transfer-contents ?output-d ?rest-d ?kitchen-out-d ?kitchen-state-with-fetched-medium-bowl ?medium-bowl ?proportioned-white-sugar ?quantity-d ?unit-d)
                       '(transfer-contents ?output-e ?rest-e ?kitchen-out-e ?kitchen-out-d ?output-d ?proportioned-white-vinegar ?quantity-e ?unit-e)
                       '(transfer-contents ?output-f ?rest-f ?kitchen-out-f ?kitchen-out-e ?output-e ?proportioned-water ?quantity-f ?unit-f)
                       '(transfer-contents ?output-g ?rest-g ?kitchen-out-g ?kitchen-out-f ?output-f ?proportioned-salt ?quantity-g ?unit-g)
                       '(mix ?liquid-mixture ?kitchen-state-with-liquid-mixture ?kitchen-out-g ?output-g ?mixing-tool)
                       '(transfer-contents ?output-h ?rest-h ?kitchen-out-h ?kitchen-state-with-liquid-mixture ?cucumber-mixture ?liquid-mixture ?quantity-pour ?unit-pour)
                       '(cover ?covered-mixture ?kitchen-state-with-covered-mixture ?kitchen-out-h ?output-h ?bowl-lid)
                       '(refrigerate ?cooled-mixture ?kitchen-state-with-cooled-mixture ?kitchen-state-with-covered-mixture ?covered-mixture ?fridge 2 hour)
                       '(uncover ?served-salad ?cover ?kitchen-state-with-served-salad ?kitchen-state-with-cooled-mixture ?cooled-mixture))
                 :primary-output-var '?served-salad))

(defparameter *easy-cherry-tomato-corn-salad-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'easy-cherry-tomato-corn-salad
                 :kitchen-state
                 *full-kitchen*
                 :meaning-network
                 (list '(get-kitchen ?kitchen)
                       '(fetch-and-proportion ?proportioned-basil ?kitchen-state-with-basil ?kitchen ?target-container-1 fresh-basil 5 g)
                       '(cut ?minced-basil ?kitchen-state-with-cut-basil ?kitchen-state-with-basil ?proportioned-basil minced ?knife)
                       '(fetch-and-proportion ?olive-oil ?kitchen-state-with-olive-oil ?kitchen-state-with-cut-basil ?target-container-2 olive-oil 3 tablespoon)
                       '(fetch-and-proportion ?lime-juice ?kitchen-state-with-lime-juice ?kitchen-state-with-olive-oil ?target-container-3 lime-juice 2 teaspoon)
                       '(fetch-and-proportion ?white-sugar ?kitchen-state-with-white-sugar ?kitchen-state-with-lime-juice ?target-container-4 white-sugar 1 teaspoon)
                       '(fetch-and-proportion ?salt ?kitchen-state-with-salt ?kitchen-state-with-white-sugar ?target-container-5 salt 0.5 teaspoon)
                       '(fetch-and-proportion ?pepper ?kitchen-state-with-pepper ?kitchen-state-with-salt ?target-container-6 ground-black-pepper 0.25 teaspoon)
                       '(fetch-and-proportion ?frozen-corn ?kitchen-state-with-frozen-corn ?kitchen-state-with-pepper ?target-container-7 frozen-corn 350 g)
                       '(bring-to-temperature ?thawed-corn ?kitchen-state-with-thawed-corn ?kitchen-state-with-frozen-corn ?frozen-corn ?room-temp-quantity ?room-temp-unit)
                       '(fetch-and-proportion ?cherry-tomatoes ?kitchen-state-with-cherry-tomatoes ?kitchen-state-with-thawed-corn ?target-container-8 cherry-tomato 300 g)
                       '(cut ?cut-tomatoes ?kitchen-state-with-cut-tomatoes ?kitchen-state-with-cherry-tomatoes ?cherry-tomatoes halved ?knife)
                       '(fetch-and-proportion ?cucumber ?kitchen-state-with-cucumber ?kitchen-state-with-cut-tomatoes ?target-container-9 cucumber 160 g)
                       '(peel ?peeled-cucumber ?cucumber-peels ?kitchen-state-with-peeled-cucumber ?kitchen-state-with-cucumber ?cucumber ?knife)
                       '(seed ?seeded-cucumber ?cucumber-seeds ?kitchen-state-with-seeded-cucumber ?kitchen-state-with-peeled-cucumber ?peeled-cucumber ?knife)
                       '(cut ?chopped-cucumber ?kitchen-state-with-chopped-cucumber ?kitchen-state-with-seeded-cucumber ?seeded-cucumber slices ?knife)
                       '(fetch-and-proportion ?jalapeno ?kitchen-state-with-jalapeno  ?kitchen-state-with-chopped-cucumber ?target-container-10 jalapeno 1 piece)
                       '(seed ?seeded-jalapeno ?jalapeno-seeds ?kitchen-state-with-seeded-jalapeno ?kitchen-state-with-jalapeno ?jalapeno ?knife)
                       '(cut ?chopped-jalapeno ?kitchen-state-with-chopped-jalapeno ?kitchen-state-with-seeded-jalapeno ?seeded-jalapeno slices ?knife)
                       '(fetch-and-proportion ?shallot ?kitchen-state-with-shallot ?kitchen-state-with-chopped-jalapeno ?target-container-11 shallot 2 piece)
                       '(cut ?cut-shallot ?kitchen-state-with-cut-shallot ?kitchen-state-with-shallot ?shallot minced ?knife)
                       '(fetch ?jar ?kitchen-state-with-fetched-jar ?kitchen-state-with-cut-shallot jar 1)
                       '(transfer-contents ?output-container-a ?rest-a ?output-kitchen-state-a ?kitchen-state-with-fetched-jar ?jar ?minced-basil ?quantity-a ?unit-a)
                       '(transfer-contents ?output-container-b ?rest-b ?output-kitchen-state-b ?output-kitchen-state-a ?output-container-a ?olive-oil ?quantity-b ?unit-b)
                       '(transfer-contents ?output-container-c ?rest-c ?output-kitchen-state-c ?output-kitchen-state-b ?output-container-b ?lime-juice ?quantity-c ?unit-c)
                       '(transfer-contents ?output-container-d ?rest-d ?output-kitchen-state-d ?output-kitchen-state-c ?output-container-c ?white-sugar ?quantity-d ?unit-d)
                       '(transfer-contents ?output-container-e ?rest-e ?output-kitchen-state-e ?output-kitchen-state-d ?output-container-d ?salt ?quantity-e ?unit-e)
                       '(transfer-contents ?output-container-f ?rest-f ?output-kitchen-state-f ?output-kitchen-state-e ?output-container-e ?pepper ?quantity-f ?unit-f)
                       '(cover ?covered-jar ?kitchen-state-with-covered-jar ?output-kitchen-state-f ?output-container-f ?jar-lid)
                       '(shake ?salad-dressing ?kitchen-state-with-dressing ?kitchen-state-with-covered-jar ?covered-jar)
                       '(fetch ?large-bowl ?kitchen-state-with-fetched-large-bowl ?kitchen-state-with-dressing large-bowl 1)
                       '(transfer-contents ?output-container-g ?rest-g ?output-kitchen-state-g ?kitchen-state-with-fetched-large-bowl ?large-bowl ?thawed-corn ?quantity-g ?unit-g)
                       '(transfer-contents ?output-container-h ?rest-h ?output-kitchen-state-h ?output-kitchen-state-g ?output-container-g ?chopped-cucumber ?quantity-h ?unit-h)
                       '(transfer-contents ?output-container-i ?rest-i ?output-kitchen-state-i ?output-kitchen-state-h ?output-container-h ?chopped-jalapeno ?quantity-i ?unit-i)
                       '(transfer-contents ?output-container-j ?rest-j ?output-kitchen-state-j ?output-kitchen-state-i ?output-container-i ?cut-shallot ?quantity-j ?unit-j)
                       '(mingle ?salad-base ?kitchen-state-with-salad-base ?output-kitchen-state-j ?output-container-j ?wooden-spoon)
                       '(uncover ?uncovered-jar ?used-jar-lid ?kitchen-state-with-uncovered-jar ?kitchen-state-with-salad-base ?covered-jar)
                       '(sprinkle ?drizzled-salad-base ?kitchen-state-with-drizzled-salad-base ?kitchen-state-with-uncovered-jar ?salad-base ?salad-dressing)
                       '(mingle ?salad ?kitchen-state-with-salad ?kitchen-state-with-drizzled-salad-base ?drizzled-salad-base ?wooden-spoon)
                       '(refrigerate ?cooled-salad ?kitchen-state-with-cooled-salad ?kitchen-state-with-salad ?salad ?fridge ?cooling-quantity ?cooling-unit))
                 :primary-output-var '?cooled-salad))

(defparameter *black-bean-and-sweet-potato-salad-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'black-bean-and-sweet-potato-salad
                 :kitchen-state
                 *full-kitchen*
                 :meaning-network
                 (list '(get-kitchen ?kitchen)
                       '(fetch-and-proportion ?proportioned-sweet-potatoes ?kitchen-state-with-sweet-potatoes ?kitchen ?target-container-1 sweet-potato 450 g)
                       '(peel ?peeled-sweet-potatoes ?sweet-potato-peels ?kitchen-state-with-peeled-sweet-potatoes ?kitchen-state-with-sweet-potatoes ?proportioned-sweet-potatoes ?knife)
                       '(cut ?sweet-potato-cubes ?kitchen-state-with-sweet-potato-cubes ?kitchen-state-with-peeled-sweet-potatoes ?peeled-sweet-potatoes two-cm-cubes ?knife)
                       '(fetch-and-proportion ?one-tablespoon-olive-oil ?kitchen-state-with-one-tablespoon-olive-oil ?kitchen-state-with-sweet-potato-cubes ?target-container-1-tablespoon olive-oil 1 tablespoon)
                       '(fetch-and-proportion ?two-tablespoons-olive-oil ?kitchen-state-with-two-tablespoons-olive-oil ?kitchen-state-with-one-tablespoon-olive-oil ?target-container-2-tablespoons olive-oil 2 tablespoon)
                       '(fetch-and-proportion ?proportioned-ground-cumin ?kitchen-state-with-ground-cumin ?kitchen-state-with-two-tablespoons-olive-oil ?target-container-3 ground-cumin 0.5 tablespoon)
                       '(fetch-and-proportion ?proportioned-red-pepper-flakes ?kitchen-state-with-red-pepper-flakes ?kitchen-state-with-ground-cumin ?target-container-4 red-pepper-flakes 0.25 teaspoon)
                       '(fetch-and-proportion ?proportioned-salt ?kitchen-state-with-salt ?kitchen-state-with-red-pepper-flakes ?target-container-5 coarse-salt 1 g)
                       '(fetch-and-proportion ?proportioned-pepper ?kitchen-state-with-pepper ?kitchen-state-with-salt ?target-container-6 ground-black-pepper 1 g)
                       '(fetch-and-proportion ?lime-juice ?kitchen-state-with-lime-juice ?kitchen-state-with-pepper ?target-container-7 lime-juice 2 tablespoon)
                       '(fetch-and-proportion ?proportioned-black-beans ?kitchen-state-with-black-beans ?kitchen-state-with-lime-juice ?target-container-8 black-bean 400 g)
                       '(fetch-and-proportion ?proportioned-red-onion ?kitchen-state-with-red-onion ?kitchen-state-with-black-beans ?target-container-9 red-onion 0.5 piece)
                       '(cut ?finely-chopped-red-onion ?kitchen-state-with-finely-chopped-onion ?kitchen-state-with-red-onion ?proportioned-red-onion fine-slices ?knife)
                       '(fetch-and-proportion ?proportioned-fresh-cilantro ?kitchen-state-with-fresh-cilantro ?kitchen-state-with-finely-chopped-onion ?target-container-10  fresh-cilantro 8 g)
                       '(cut ?chopped-fresh-cilantro ?kitchen-state-with-fresh-chopped-cilantro ?kitchen-state-with-fresh-cilantro ?proportioned-fresh-cilantro slices ?knife)
                       '(preheat-oven ?preheated-oven ?kitchen-state-with-preheated-oven ?kitchen-state-with-fresh-chopped-cilantro ?oven 230 degrees-celsius)
                       '(fetch ?baking-tray ?kitchen-state-with-baking-tray ?kitchen-state-with-preheated-oven baking-tray 1)
                       '(transfer-contents ?output-container-a ?rest-a ?output-kitchen-state-a ?kitchen-state-with-baking-tray ?baking-tray ?sweet-potato-cubes ?quantity-a ?unit-a)
                       '(sprinkle ?drizzled-potatoes ?kitchen-state-with-drizzled-potatoes ?output-kitchen-state-a ?output-container-a ?one-tablespoon-olive-oil)
                       '(transfer-contents ?output-container-b ?rest-b ?output-kitchen-state-b ?kitchen-state-with-drizzled-potatoes ?drizzled-potatoes ?proportioned-ground-cumin ?quantity-b ?unit-b)
                       '(transfer-contents ?output-container-c ?rest-c ?output-kitchen-state-c ?output-kitchen-state-b ?output-container-b ?proportioned-red-pepper-flakes ?quantity-c ?unit-c)
                       '(transfer-contents ?output-container-d ?rest-d ?output-kitchen-state-d ?output-kitchen-state-c ?output-container-c ?proportioned-salt ?quantity-d ?unit-d)
                       '(transfer-contents ?output-container-e ?rest-e ?output-kitchen-state-e ?output-kitchen-state-d ?output-container-d ?proportioned-pepper ?quantity-e ?unit-e)
                       '(mingle ?tossed-potatoes ?kitchen-state-with-tossed-potatoes ?output-kitchen-state-e ?output-container-e ?wooden-spoon)
                       '(bake ?baked-potatoes ?kitchen-state-with-baked-potatoes ?kitchen-state-with-tossed-potatoes ?tossed-potatoes ?preheated-oven 25 minute ?bake-quantity ?bake-unit)
                       '(fetch ?large-bowl ?kitchen-state-with-fetched-large-bowl ?kitchen-state-with-baked-potatoes large-bowl 1)
                       '(fetch ?whisk ?kitchen-state-with-fetched-whisk ?kitchen-state-with-fetched-large-bowl whisk 1)
                       '(transfer-contents ?output-container-f ?rest-f ?output-kitchen-state-f ?kitchen-state-with-fetched-whisk ?large-bowl ?two-tablespoons-olive-oil ?quantity-f ?unit-f)
                       '(transfer-contents ?output-container-g ?rest-g ?output-kitchen-state-g ?output-kitchen-state-f ?output-container-f ?lime-juice ?quantity-g ?unit-g)
                       '(mix ?whisked-mixture ?kitchen-state-with-whisked-mixture ?output-kitchen-state-g ?output-container-g ?whisk)
                       '(transfer-contents ?output-container-h ?rest-h ?output-kitchen-state-h ?output-kitchen-state-g ?whisked-mixture ?baked-potatoes ?quantity-h ?unit-h)
                       '(transfer-contents ?output-container-i ?rest-i ?output-kitchen-state-i ?output-kitchen-state-h ?output-container-h ?proportioned-black-beans ?quantity-i ?unit-i)
                       '(transfer-contents ?output-container-j ?rest-j ?output-kitchen-state-j ?output-kitchen-state-i ?output-container-i ?finely-chopped-red-onion ?quantity-j ?unit-j)
                       '(transfer-contents ?output-container-k ?rest-k ?output-kitchen-state-k ?output-kitchen-state-j ?output-container-j ?proportioned-fresh-cilantro ?quantity-k ?unit-k)
                       '(mingle ?salad ?kitchen-state-with-salad ?output-kitchen-state-k ?output-container-k ?wooden-spoon))
                 :primary-output-var '?salad))

; Extra Almond Crescent Cookies
(defparameter *almond-crescent-cookies-2-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'almond-crescent-cookies-2
                 :kitchen-state
                 *full-kitchen*
                 :meaning-network
                 (list '(get-kitchen ?kitchen)
                        '(fetch-and-proportion ?proportioned-butter ?ks-with-butter ?kitchen ?target-container-1 butter 230 g)
                        '(bring-to-temperature ?warm-butter ?ks-with-warm-butter ?ks-with-butter ?proportioned-butter ?room-temp-quantity ?room-temp-unit)
                        '(fetch-and-proportion ?proportioned-sugar ?ks-with-sugar ?ks-with-warm-butter ?target-container-2 white-sugar 200 g)
                        '(fetch-and-proportion ?proportioned-vanilla ?ks-with-vanilla ?ks-with-sugar ?target-container-3 vanilla-extract 1 teaspoon)
                        '(fetch-and-proportion ?proportioned-almond ?ks-with-almond ?ks-with-vanilla ?target-container-4 almond-extract 2 teaspoon)
                        '(fetch-and-proportion ?proportioned-flour ?ks-with-flour ?ks-with-almond ?target-container-5 all-purpose-flour 280 g)
                        '(fetch-and-proportion ?proportioned-almond-flour ?ks-with-almond-flour ?ks-with-flour ?target-container-6 almond-flour 120 g)
                        '(fetch-and-proportion ?proportioned-powdered-sugar ?ks-with-powdered-sugar ?ks-with-almond-flour ?target-container-7 powdered-white-sugar 120 g)
                        '(preheat-oven ?preheated-oven ?ks-with-preheated-oven ?ks-with-powdered-sugar ?oven 175 degrees-celsius)
                        '(fetch ?cookie-sheet ?ks-with-cookie-sheet ?ks-with-preheated-oven cookie-sheet 1)
                        '(grease ?greased-sheet ?ks-with-greased-sheet ?ks-with-cookie-sheet ?cookie-sheet ?grease)
                        '(fetch ?mixer ?ks-with-mixer ?ks-with-greased-sheet mixer 1)
                        '(transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-mixer ?empty-container-a ?warm-butter ?quantity-a ?unit-a)
                        '(transfer-contents ?output-container-b ?rest-b ?output-ks-b ?output-ks-a ?output-container-a ?proportioned-sugar ?quantity-b ?unit-b)
                        '(beat ?beaten-mixture ?ks-with-beaten-mixture ?output-ks-b ?output-container-b ?mixer)
                        '(transfer-contents ?output-container-c ?rest-c ?output-ks-c ?ks-with-beaten-mixture ?beaten-mixture ?proportioned-vanilla ?quantity-c ?unit-c)
                        '(transfer-contents ?output-container-d ?rest-d ?output-ks-d ?output-ks-c ?output-container-c ?proportioned-almond ?quantity-d ?unit-d)
                        '(beat ?intermediate-mixture ?ks-with-intermediate-mixture ?output-ks-d ?output-container-d ?mixer)
                        '(transfer-contents ?output-container-e ?rest-e ?output-ks-e ?ks-with-intermediate-mixture ?intermediate-mixture ?proportioned-flour ?quantity-e ?unit-e)
                        '(transfer-contents ?output-container-f ?rest-f ?output-ks-f ?output-ks-e ?intermediate-mixture ?proportioned-almond-flour ?quantity-f ?unit-f)
                        '(mix ?dough ?ks-with-dough ?output-ks-f ?output-container-f ?mixing-tool)
                        '(portion-and-arrange ?portioned-dough ?ks-with-dough-portions ?ks-with-dough ?dough 25 g ?pattern ?countertop)
                        '(shape ?bakeable-balls ?ks-with-balls ?ks-with-dough-portions ?portioned-dough ball-shape)
                        '(shape ?bakeable-crescents ?ks-with-crescents ?ks-with-balls ?bakeable-balls crescent-shape)
                        '(transfer-items ?tray-with-crescents ?ks-with-crescents-tray ?ks-with-crescents ?bakeable-crescents ?default-pattern ?greased-sheet)
                        '(bake ?baked-crescents ?ks-with-baked-crescents ?ks-with-crescents-tray ?tray-with-crescents ?oven 12 minute ?temp-qty ?temp-unit)
                        '(fetch ?small-bowl ?ks-with-small-bowl ?ks-with-baked-crescents small-bowl 1)
                        '(sift ?sifted-powdered-sugar ?ks-after-sifting ?ks-with-small-bowl ?small-bowl ?proportioned-powdered-sugar ?sifting-tool)
                        '(dip ?dipped-cookies ?ks-with-dipped-cookies ?ks-after-sifting ?baked-crescents ?sifted-powdered-sugar)
                        '(fetch ?wire-rack ?ks-with-wire-rack ?ks-with-dipped-cookies wire-rack 1)
                        '(transfer-items ?cookies-on-wire-rack ?ks-with-cookies-on-wire-rack ?ks-with-wire-rack ?dipped-cookies ?default-pattern ?wire-rack)
                        '(bring-to-temperature ?cooled-cookies ?ks-with-cooled-cookies ?ks-with-cookies-on-wire-rack ?cookies-on-wire-rack ?room-temp-quantity-2 ?room-temp-unit-2))
                 :primary-output-var
                 '?cooled-cookies))

(defparameter *almond-crescent-cookies-3-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'almond-crescent-cookies-3
                 :kitchen-state
                 *full-kitchen*
                 :meaning-network
                 (list '(get-kitchen ?kitchen)
                       '(fetch-and-proportion ?proportioned-butter ?ks-with-butter ?kitchen ?target-container-1 salted-butter 120 g)
                       '(bring-to-temperature ?warm-butter ?ks-with-warm-butter ?ks-with-butter ?proportioned-butter ?room-temp-qty ?room-temp-unit)
                       '(fetch-and-proportion ?proportioned-sugar ?ks-with-sugar ?ks-with-warm-butter ?target-container-2 powdered-white-sugar 40 g)
                       '(fetch-and-proportion ?proportioned-dusting-sugar ?ks-with-dusting-sugar ?ks-with-sugar ?target-container-3 powdered-white-sugar 30 g)
                       '(fetch-and-proportion ?proportioned-vanilla ?ks-with-vanilla ?ks-with-dusting-sugar ?target-container-4 vanilla-extract 1 teaspoon)
                       '(fetch-and-proportion ?proportioned-almond-extract ?ks-with-almond ?ks-with-vanilla ?target-container-5 almond-extract 1 teaspoon)
                       '(fetch-and-proportion ?proportioned-salt ?ks-with-salt ?ks-with-almond ?target-container-6 salt 0.125 teaspoon)
                       '(fetch-and-proportion ?proportioned-flour ?ks-with-flour ?ks-with-salt ?target-container-7 all-purpose-flour 90 g)
                       '(sift ?sifted-flour ?ks-with-sifted-flour ?ks-with-flour ?large-bowl ?proportioned-flour ?sifting-tool)
                       '(fetch-and-proportion ?proportioned-tbsp-flour ?ks-with-tbsp-flour ?ks-with-sifted-flour ?target-container-8 all-purpose-flour 2 tablespoon)
                       '(sift ?sifted-tbsp-flour ?ks-with-sifted-tbsp-flour ?ks-with-tbsp-flour ?large-bowl-2 ?proportioned-tbsp-flour ?sifting-tool)
                       '(fetch-and-proportion ?proportioned-almonds ?ks-with-almonds ?ks-with-sifted-tbsp-flour ?target-container-9 almond 70 g)
                       '(cut ?finely-chopped-almonds ?ks-with-finely-chopped-almonds ?ks-with-almonds ?proportioned-almonds finely-chopped ?knife)
                       '(preheat-oven ?preheated-oven ?ks-with-preheated-oven ?ks-with-finely-chopped-almonds ?oven 165 degrees-celsius)
                       '(transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-preheated-oven ?empty-container-a ?warm-butter ?quantity-a ?unit-a)
                       '(transfer-contents ?output-container-b ?rest-b ?output-ks-b ?output-ks-a ?output-container-a ?proportioned-sugar ?quantity-b ?unit-b)
                       '(fetch ?mixer ?ks-with-mixer ?output-ks-b mixer 1)
                       '(beat ?beaten-mixture ?ks-with-beaten-mixture ?ks-with-mixer ?output-container-b ?mixer)
                       '(transfer-contents ?output-container-c ?rest-c ?output-ks-c ?ks-with-beaten-mixture ?beaten-mixture ?proportioned-vanilla ?quantity-c ?unit-c)
                       '(transfer-contents ?output-container-d ?rest-d ?output-ks-d ?output-ks-c ?output-container-c ?proportioned-almond-extract ?quantity-d ?unit-d)
                       '(transfer-contents ?output-container-e ?rest-e ?output-ks-e ?output-ks-d ?output-container-d ?proportioned-salt ?quantity-e ?unit-e)
                       '(mix ?intermediate-mixture ?ks-with-intermediate-mixture ?output-ks-e ?output-container-e ?mixing-tool)
                       '(transfer-contents ?output-container-f ?rest-f ?output-ks-f ?ks-with-intermediate-mixture ?intermediate-mixture ?sifted-flour ?quantity-f ?unit-f)
                       '(transfer-contents ?output-container-g ?rest-g ?output-ks-g ?output-ks-f ?output-container-f ?sifted-tbsp-flour ?quantity-g ?unit-g)
                       '(transfer-contents ?output-container-h ?rest-h ?output-ks-h ?output-ks-g ?output-container-g ?finely-chopped-almonds ?quantity-h ?unit-h)
                       '(mix ?dough ?ks-with-dough ?output-ks-h ?output-container-h ?mixing-tool)
                       '(portion-and-arrange ?portioned-dough ?ks-with-dough-portions ?ks-with-dough ?dough 25 g ?pattern ?countertop)
                       '(shape ?bakeable-crescents ?ks-with-crescents ?ks-with-dough-portions ?portioned-dough crescent-shape)
                       '(fetch ?baking-tray ?ks-with-baking-tray ?ks-with-crescents baking-tray 1)
                       '(transfer-items ?tray-with-crescents ?ks-with-crescents-tray ?ks-with-baking-tray ?bakeable-crescents 5-cm-apart ?baking-tray)
                       '(bake ?baked-crescents ?ks-with-baked-crescents ?ks-with-crescents-tray ?tray-with-crescents ?preheated-oven 15 minute ?bake-temp-qty ?bake-temp-unit)
                       '(leave-for-time ?cooling-cookies ?ks-with-cooling-cookies ?ks-with-baked-crescents ?baked-crescents 5 minute)
                       '(fetch ?wire-rack ?ks-with-wire-rack ?ks-with-cooling-cookies wire-rack 1)
                       '(transfer-items ?cookies-on-wire-rack ?ks-with-cookies-on-wire-rack ?ks-with-wire-rack ?cooling-cookies ?default-pattern ?wire-rack)
                       '(bring-to-temperature ?cooled-cookies ?ks-with-cooled-cookies ?ks-with-cookies-on-wire-rack ?cookies-on-wire-rack ?room-temp-qty-2 ?room-temp-unit-2)
                       '(sift ?sifted-sugar ?ks-with-sifted-sugar ?ks-with-cooled-cookies ?large-bowl ?proportioned-dusting-sugar ?sifting-tool)
                       '(sprinkle ?almond-crescent-cookies ?ks-with-almond-crescent-cookies ?ks-with-sifted-sugar ?cooled-cookies ?sifted-sugar))
                 :primary-output-var
                 '?almond-crescent-cookies))

(defparameter *almond-crescent-cookies-4-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'almond-crescent-cookies-4
                 :kitchen-state
                 *full-kitchen*
                 :meaning-network
                 (list '(get-kitchen ?kitchen)
                       '(fetch-and-proportion ?proportioned-butter ?ks-with-butter ?kitchen ?target-container-1 butter 230 g)
                       '(fetch-and-proportion ?proportioned-vanilla ?ks-with-vanilla ?ks-with-butter ?target-container-2 vanilla 2 teaspoon)
                       '(fetch-and-proportion ?proportioned-powdered-sugar ?ks-with-sugar ?ks-with-vanilla ?target-container-3 powdered-white-sugar 100 g)
                       '(fetch-and-proportion ?proportioned-water ?ks-with-water ?ks-with-sugar ?target-container-4 water 1 tablespoon)
                       '(fetch-and-proportion ?proportioned-flour ?ks-with-flour ?ks-with-water ?target-container-5 all-purpose-flour 240 g)
                       '(fetch-and-proportion ?proportioned-almonds ?ks-with-almonds ?ks-with-flour ?target-container-6 almond 140 g)
                       '(cut ?chopped-almonds ?ks-with-chopped-almonds ?ks-with-almonds ?proportioned-almonds chopped ?knife)
                       '(transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-chopped-almonds ?empty-container-a ?proportioned-butter ?quantity-a ?unit-a)
                       '(transfer-contents ?output-container-b ?rest-b ?output-ks-b ?output-ks-a ?output-container-a ?proportioned-vanilla ?quantity-b ?unit-b)
                       '(transfer-contents ?output-container-c ?rest-c ?output-ks-c ?output-ks-b ?output-container-b ?proportioned-powdered-sugar ?quantity-c ?unit-c)
                       '(beat ?beaten-mixture ?ks-with-beaten-mixture ?output-ks-c ?output-container-c ?mixing-tool)
                       '(transfer-contents ?output-container-d ?rest-d ?output-ks-d ?ks-with-beaten-mixture ?beaten-mixture ?proportioned-flour ?quantity-d ?unit-d)
                       '(transfer-contents ?output-container-e ?rest-e ?output-ks-e ?output-ks-d ?output-container-d ?proportioned-water ?quantity-e ?unit-e)
                       '(mix ?dough ?ks-with-dough ?output-ks-e ?output-container-e ?mixing-tool)
                       '(transfer-contents ?output-container-f ?rest-f ?output-ks-f ?ks-with-dough ?dough ?chopped-almonds ?quantity-f ?unit-f)
                       '(mingle ?dough-with-almonds ?ks-with-almonds-dough ?output-ks-f ?output-container-f ?mixing-tool)
                       '(portion-and-arrange ?portioned-dough ?ks-with-dough-portions ?ks-with-almonds-dough ?dough-with-almonds 30 g ?pattern ?countertop)
                       '(shape ?bakeable-crescents ?ks-with-crescents ?ks-with-dough-portions ?portioned-dough crescent-shape)
                       '(fetch ?cookie-sheet ?ks-with-cookie-sheet ?ks-with-crescents cookie-sheet 1)
                       '(grease ?greased-sheet ?ks-with-greased-sheet ?ks-with-cookie-sheet ?cookie-sheet ?grease)
                       '(transfer-items ?tray-with-crescents ?ks-with-crescents-tray ?ks-with-greased-sheet ?bakeable-crescents ?default-pattern ?greased-sheet)
                       '(bake ?baked-crescents ?ks-with-baked-crescents ?ks-with-crescents-tray ?tray-with-crescents ?oven 20 minute 150 degrees-celsius))
                 :primary-output-var
                 '?baked-crescents))

(defparameter *almond-crescent-cookies-5-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'almond-crescent-cookies-5
                 :kitchen-state
                 *full-kitchen*
                 :meaning-network
                 (list '(get-kitchen ?kitchen)
                       '(fetch-and-proportion ?proportioned-butter ?ks-with-butter ?kitchen ?target-container-1 butter 230 g)
                       '(bring-to-temperature ?warm-butter ?ks-with-warm-butter ?ks-with-butter ?proportioned-butter ?room-temp-quantity ?room-temp-unit)
                       '(fetch-and-proportion ?proportioned-sugar ?ks-with-sugar ?ks-with-warm-butter ?target-container-2 white-sugar 100 g)
                       '(fetch-and-proportion ?proportioned-egg ?ks-with-egg ?ks-with-sugar ?target-container-3 egg 1 piece)
                       '(crack ?cracked-egg ?ks-with-cracked-egg ?ks-with-egg ?proportioned-egg ?empty-bowl-1)
                       '(separate-eggs ?egg-yolk ?egg-white ?ks-with-separated-egg ?ks-with-cracked-egg ?cracked-egg ?empty-bowl-2 ?empty-bowl-3 ?egg-separator)
                       '(bring-to-temperature ?warm-egg ?ks-with-warm-egg ?ks-with-separated-egg ?egg-yolk ?room-temp-quantity-2 ?room-temp-unit-2)
                       '(fetch-and-proportion ?proportioned-vanilla ?ks-with-vanilla ?ks-with-warm-egg ?target-container-4 vanilla-extract 1 teaspoon)
                       '(fetch-and-proportion ?proportioned-almond-flour ?ks-with-flour ?ks-with-vanilla ?target-container-5 almond-flour 25 g)
                       '(fetch-and-proportion ?proportioned-flour ?ks-with-almond-flour ?ks-with-flour ?target-container-6 all-purpose-flour 200 g)
                       '(fetch-and-proportion ?proportioned-powdered-sugar ?ks-with-powdered-sugar ?ks-with-almond-flour ?target-container-7 powdered-white-sugar 30 g)
                       '(preheat-oven ?preheated-oven ?ks-with-preheated-oven ?ks-with-powdered-sugar ?oven 175 degrees-celsius)
                       '(fetch ?baking-tray ?ks-with-baking-tray ?ks-with-preheated-oven baking-tray 1)
                       '(fetch ?large-bowl ?ks-with-large-bowl ?ks-with-baking-tray large-bowl 1)
                       '(transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-large-bowl ?large-bowl ?warm-butter ?quantity-a ?unit-a)
                       '(transfer-contents ?output-container-b ?rest-b ?output-ks-b ?output-ks-a ?output-container-a ?proportioned-sugar ?quantity-b ?unit-b)
                       '(beat ?beaten-mixture ?ks-with-beaten-mixture ?output-ks-b ?output-container-b ?mixing-tool)
                       '(transfer-contents ?output-container-c ?rest-c ?output-ks-c ?ks-with-beaten-mixture ?beaten-mixture ?warm-egg ?quantity-c ?unit-c)
                       '(transfer-contents ?output-container-d ?rest-d ?output-ks-d ?output-ks-c ?output-container-c ?proportioned-vanilla ?quantity-d ?unit-d)
                       '(beat ?intermediate-mixture ?ks-with-intermediate-mixture ?output-ks-d ?output-container-d ?mixing-tool)
                       '(transfer-contents ?output-container-e ?rest-e ?output-ks-e ?ks-with-intermediate-mixture ?intermediate-mixture ?proportioned-almond-flour ?quantity-e ?unit-e)
                       '(transfer-contents ?output-container-f ?rest-f ?output-ks-f ?output-ks-e ?output-container-e ?proportioned-flour ?quantity-f ?unit-f)
                       '(mix ?dough ?ks-with-dough ?output-ks-f ?output-container-f ?mixing-tool)
                       '(portion-and-arrange ?portioned-dough ?ks-with-dough-portions ?ks-with-dough ?dough 25 g ?pattern ?countertop)
                       '(shape ?bakeable-crescents ?ks-with-crescents ?ks-with-dough-portions ?portioned-dough crescent-shape)
                       '(transfer-items ?tray-with-crescents ?ks-with-crescents-tray ?ks-with-crescents ?bakeable-crescents ?default-pattern ?baking-tray)
                       '(bake ?baked-crescents ?ks-with-baked-crescents ?ks-with-crescents-tray ?tray-with-crescents ?preheated-oven 20 minute ?bake-temp-qty ?bake-temp-unit)
                       '(dip ?dipped-hot-crescents ?ks-with-dipped-hot-crescents ?ks-with-baked-crescents ?baked-crescents ?proportioned-powdered-sugar)
                       '(bring-to-temperature ?cooled-crescents ?ks-with-cooled-crescents ?ks-with-dipped-hot-crescents ?dipped-hot-crescents ?room-temp-quantity-2 ?room-temp-unit-2)
                       '(fetch ?large-bowl-2 ?ks-with-large-bowl-2 ?ks-with-cooled-crescents large-bowl 1)
                       '(transfer-contents ?output-container-g ?rest-g ?output-ks-g ?ks-with-large-bowl-2 ?large-bowl-2 ?cooled-crescents ?quantity-g ?unit-g)
                       '(cover ?covered-cookies ?ks-with-covered-cookies ?output-ks-g ?output-container-g ?cover))
                 :primary-output-var
                 '?covered-cookies))

; extra baking recipes

(defparameter *coconut-tuiles-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'coconut-tuiles
                 :kitchen-state
                 *full-kitchen*
                 :meaning-network
                 (list '(get-kitchen ?kitchen)
                       '(fetch-and-proportion ?proportioned-butter ?ks-with-butter ?kitchen ?target-container-1 butter 230 g)
                       '(bring-to-temperature ?warm-butter ?ks-with-warm-butter ?ks-with-butter ?proportioned-butter ?room-temp-quantity ?room-temp-unit)
                       '(fetch-and-proportion ?proportioned-sugar ?ks-with-sugar ?ks-with-warm-butter ?target-container-2 white-sugar 100 g)
                       '(fetch-and-proportion ?proportioned-egg-white ?ks-with-egg-white ?ks-with-sugar ?target-container-3 egg-white 80 g)
                       '(beat ?beaten-egg-white ?ks-with-beaten-egg-white ?ks-with-egg-white ?proportioned-egg-white ?mixing-tool)
                       '(fetch-and-proportion ?proportioned-flour ?ks-with-flour ?ks-with-beaten-egg-white ?target-container-4 all-purpose-flour 3 tablespoon)
                       '(fetch-and-proportion ?proportioned-coconut-34 ?ks-with-coconut-34 ?ks-with-flour ?target-container-5 shredded-coconut 150 g)
                       '(fetch-and-proportion ?proportioned-coconut-14 ?ks-with-coconut-14 ?ks-with-coconut-34 ?target-container-6 shredded-coconut 50 g)
                       '(preheat-oven ?preheated-oven ?ks-with-preheated-oven ?ks-with-coconut-14 ?oven 200 degrees-celsius)
                       '(fetch ?mixer ?ks-with-mixer ?ks-with-preheated-oven mixer 1)
                       '(beat ?beaten-butter ?ks-with-beaten-butter ?ks-with-mixer ?warm-butter ?mixer)
                       '(transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-beaten-butter ?empty-bowl ?beaten-butter ?quantity-a ?unit-a)
                       '(transfer-contents ?output-container-b ?rest-b ?output-ks-b ?output-ks-a ?output-container-a ?proportioned-sugar ?quantity-b ?unit-b)
                       '(beat ?beaten-sugar-mixture ?ks-with-beaten-sugar-mixture ?output-ks-b ?proportioned-sugar ?mixer)
                       '(transfer-contents ?output-container-white ?rest-white ?output-ks-white ?ks-with-beaten-sugar-mixture ?beaten-sugar-mixture ?proportioned-egg-white ?quantity-white ?unit-white)
                       '(beat ?beaten-egg-mixture ?ks-with-beaten-egg-mixture ?output-ks-white ?output-container-white ?mixing-tool)
                       '(transfer-contents ?output-container-c ?rest-c ?output-ks-c ?ks-with-beaten-egg-mixture ?beaten-egg-mixture ?proportioned-flour ?quantity-c ?unit-c)
                       '(transfer-contents ?output-container-d ?rest-d ?output-ks-d ?output-ks-c ?output-container-c ?proportioned-coconut-34 ?quantity-d ?unit-d)
                       '(mix ?dough ?ks-with-dough ?output-ks-d ?output-container-d ?mixing-tool)
                       '(fetch ?baking-tray ?ks-with-baking-tray ?ks-with-dough baking-tray 1)
                       '(grease ?greased-tray ?ks-with-greased-tray ?ks-with-baking-tray ?baking-tray ?grease)
                       '(portion-and-arrange ?portioned-dough ?ks-with-dough-portions ?ks-with-greased-tray ?dough 25 g 5-cm-apart ?greased-tray)
                       '(sprinkle ?sprinkled-dough ?ks-with-sprinkled-dough ?ks-with-dough-portions ?portioned-dough ?proportioned-coconut-14)
                       '(bake ?baked-rounds ?ks-with-baked-rounds ?ks-with-sprinkled-dough ?sprinkled-dough ?oven 6 minute 200 degrees-celsius)
                       '(leave-for-time ?cooling-rounds ?ks-with-cooling-rounds ?ks-with-baked-rounds ?baked-rounds 0.5 minute)
                       '(fetch ?wire-rack ?ks-with-wire-rack ?ks-with-cooling-rounds wire-rack 1)
                       '(transfer-items ?rounds-on-wire-rack ?ks-with-rounds-on-wire-rack ?ks-with-wire-rack ?cooling-rounds ?default-pattern ?wire-rack)
                       '(bring-to-temperature ?cooled-rounds ?ks-with-cooled-cookies ?ks-with-rounds-on-wire-rack ?rounds-on-wire-rack ?room-temp-qty-2 ?room-temp-unit-2))
                 :primary-output-var
                 '?cooled-rounds))

(defparameter *mexican-wedding-cookies-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'mexican-wedding-cookies
                 :kitchen-state
                 *full-kitchen*
                 :meaning-network
                 (list '(get-kitchen ?kitchen)
                       '(fetch-and-proportion ?proportioned-butter ?ks-with-butter ?kitchen ?target-container-1 butter 230 g)
                       '(bring-to-temperature ?warm-butter ?ks-with-warm-butter ?ks-with-butter ?proportioned-butter ?room-temp-quantity ?room-temp-unit)
                       '(fetch-and-proportion ?proportioned-powdered-sugar ?ks-with-powdered-sugar ?ks-with-warm-butter ?target-container-2 powdered-white-sugar 120 g)
                       '(fetch-and-proportion ?proportioned-flour ?ks-with-flour ?ks-with-powdered-sugar ?target-container-3 all-purpose-flour 240 g)
                       '(sift ?sifted-flour ?ks-with-sifted-flour ?ks-with-flour ?target-container-4 ?proportioned-flour ?sifting-tool)
                       '(fetch-and-proportion ?proportioned-walnut ?ks-with-walnut ?ks-with-sifted-flour ?target-container-5 walnut 120 g)
                       '(grind ?ground-walnut ?ks-with-ground-walnut ?ks-with-walnut ?proportioned-walnut ?grinding-tool)
                       '(fetch-and-proportion ?proportioned-vanilla ?ks-with-vanilla ?ks-with-ground-walnut ?target-container-7 vanilla 1 teaspoon)
                       '(fetch-and-proportion ?additional-powdered-sugar ?ks-with-additional-sugar ?ks-with-vanilla ?target-container-8 powdered-white-sugar 60 g)
                       '(transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-additional-sugar ?empty-container ?warm-butter ?quantity-a ?unit-a)
                       '(transfer-contents ?output-container-b ?rest-b ?output-ks-b ?output-ks-a ?output-container-a ?proportioned-powdered-sugar ?quantity-b ?unit-b)
                       '(transfer-contents ?output-container-c ?rest-c ?output-ks-c ?output-ks-b ?output-container-b ?sifted-flour ?quantity-c ?unit-c)
                       '(transfer-contents ?output-container-d ?rest-d ?output-ks-d ?output-ks-c ?output-container-c ?ground-walnut ?quantity-d ?unit-d)
                       '(transfer-contents ?output-container-e ?rest-e ?output-ks-e ?output-ks-d ?output-container-d ?proportioned-vanilla ?quantity-e ?unit-e)
                       '(mix ?dough ?ks-with-dough ?output-ks-e ?output-container-e ?mixing-tool)
                       '(portion-and-arrange ?portioned-dough ?ks-with-dough-portions ?ks-with-dough ?dough 25 g ?pattern ?countertop)
                       '(shape ?bakeable-balls ?ks-with-balls ?ks-with-dough-portions ?portioned-dough ball-shape)
                       '(fetch ?cookie-sheet ?ks-with-cookie-sheet ?ks-with-balls cookie-sheet 1)
                       '(transfer-items ?tray-with-balls ?ks-with-balls-tray ?ks-with-cookie-sheet ?bakeable-balls ?default-pattern ?cookie-sheet)
                       '(bake ?baked-balls ?ks-with-baked-balls ?ks-with-balls-tray ?tray-with-balls ?oven 10 minute 180 degrees-celsius)
                       '(dip ?mexican-wedding-cookies ?ks-with-mexican-wedding-cookies ?ks-with-baked-balls ?baked-balls ?additional-powdered-sugar))
                 :primary-output-var
                 '?mexican-wedding-cookies))

(defparameter *bisquick-shortcake-biscuits-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'bisquick-shortcake-biscuits
                 :kitchen-state
                 *full-kitchen*
                 :meaning-network
                 (list '(get-kitchen ?kitchen)
                       '(fetch-and-proportion ?proportioned-bisquick ?ks-with-bisquick ?kitchen ?target-container-1 bisquick-baking-mix 260 g)
                       '(fetch-and-proportion ?proportioned-butter ?ks-with-butter ?ks-with-bisquick ?target-container-2 butter 3 tablespoon)
                       '(fetch-and-proportion ?proportioned-coconut-oil ?ks-with-coconut-oil ?ks-with-butter ?target-container-3 coconut-oil 3 tablespoon)
                       '(fetch-and-proportion ?proportioned-milk ?ks-with-milk ?ks-with-coconut-oil ?target-container-4 milk 250 ml)
                       '(fetch-and-proportion ?proportioned-sugar ?ks-with-sugar ?ks-with-milk ?target-container-5 white-sugar 3 tablespoon)
                       '(preheat-oven ?preheated-oven ?ks-with-preheated-oven ?ks-with-sugar ?oven 220 degrees-celsius)
                       '(fetch ?medium-bowl ?ks-with-medium-bowl ?ks-with-preheated-oven medium-bowl 1)
                       '(transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-medium-bowl ?medium-bowl ?proportioned-bisquick ?quantity-a ?unit-a)
                       '(transfer-contents ?output-container-b ?rest-b ?output-ks-b ?output-ks-a ?medium-bowl ?proportioned-sugar ?quantity-b ?unit-b)
                       '(mix ?intermediate-mixture ?ks-with-intermediate-mixture ?output-ks-b ?output-container-b ?mixing-tool)
                       '(transfer-contents ?output-container-c ?rest-c ?output-ks-c ?ks-with-intermediate-mixture ?intermediate-mixture ?proportioned-milk ?quantity-c ?unit-c)
                       '(transfer-contents ?output-container-d ?rest-d ?output-ks-d ?output-ks-c ?output-container-c ?proportioned-coconut-oil ?quantity-d ?unit-d)
                       '(mix ?dough ?ks-with-dough ?output-ks-d ?output-container-d ?mixing-tool)
                       '(fetch ?cookie-sheet ?ks-with-cookie-sheet ?ks-with-dough cookie-sheet 1)
                       '(grease ?greased-sheet ?ks-with-greased-sheet ?ks-with-cookie-sheet ?cookie-sheet ?proportioned-butter)
                       '(portion-and-arrange ?portioned-dough ?ks-with-portioned-dough ?ks-with-greased-sheet ?dough 100 g ?pattern ?countertop)
                       '(transfer-items ?portions-on-sheet ?ks-with-portions-on-sheet ?ks-with-portioned-dough ?portioned-dough ?default-pattern ?greased-sheet)
                       '(bake ?baked-biscuit ?ks-with-baked-biscuit ?ks-with-portions-on-sheet ?portions-on-sheet ?oven 10 minute 220 degrees-celsius)
                       '(fetch ?wire-rack ?ks-with-wire-rack ?ks-with-baked-biscuit wire-rack 1)
                       '(transfer-items ?biscuit-on-wire-rack ?ks-with-biscuit-on-wire-rack ?ks-with-wire-rack ?baked-biscuit ?default-pattern-2 ?wire-rack)
                       '(leave-for-time ?cooling-biscuit ?ks-with-cooling-biscuit ?ks-with-biscuit-on-wire-rack ?biscuit-on-wire-rack 5 minute)
                       '(fetch ?bread-knife ?ks-with-bread-knife ?ks-with-cooling-biscuit bread-knife 1)
                       '(cut ?bisquick-shortcake-biscuits ?ks-with-bisquick-shortcake-biscuits ?ks-with-bread-knife ?cooling-biscuit slices ?bread-knife))
                 :primary-output-var
                 '?bisquick-shortcake-biscuits))

(defparameter *chocolate-cream-cheese-cupcakes-environment*
  ;; error!!
  (make-instance 'simulation-environment
                 :recipe-id 'chocolate-cream-cheese-cupcakes
                 :kitchen-state
                 *full-kitchen*
                 :meaning-network
                 (list '(get-kitchen ?kitchen)
                       '(fetch-and-proportion ?proportioned-cheese ?ks-with-cheese ?kitchen ?target-container-1 cream-cheese 230 g)
                       '(bring-to-temperature ?softened-cheese ?ks-with-softened-cheese ?ks-with-cheese ?proportioned-cheese ?room-temp-quantity ?room-temp-unit)
                       '(fetch-and-proportion ?proportioned-egg ?ks-with-egg ?ks-with-softened-cheese ?target-container-2 egg 1 piece)
                       '(crack ?cracked-egg ?ks-with-cracked-egg ?ks-with-egg ?proportioned-egg ?empty-bowl)
                       '(beat ?beaten-egg ?ks-with-beaten-egg ?ks-with-cracked-egg ?cracked-egg ?mixing-tool)
                       '(fetch-and-proportion ?proportioned-sugar-70 ?ks-with-sugar-70 ?ks-with-beaten-egg ?target-container-3 white-sugar 70 g)
                       '(fetch-and-proportion ?proportioned-salt-1 ?ks-with-salt-1 ?ks-with-sugar-70 ?target-container-4 salt 0.5 teaspoon)
                       '(fetch-and-proportion ?proportioned-chocolate-chips ?ks-with-chocolate-chips ?ks-with-salt-1 ?target-container-5 chocolate-chips 180 g)
                       '(fetch-and-proportion ?proportioned-sugar-210 ?ks-with-sugar-210 ?ks-with-chocolate-chips ?target-container-6 white-sugar 210 g)
                       '(fetch-and-proportion ?proportioned-flour ?ks-with-flour ?ks-with-sugar-210 ?target-container-7 all-purpose-flour 180 g)
                       '(fetch-and-proportion ?proportioned-cocoa ?ks-with-cocoa ?ks-with-flour ?target-container-8 cocoa-powder 30 g)
                       '(fetch-and-proportion ?proportioned-baking-soda ?ks-with-baking-soda ?ks-with-cocoa ?target-container-9 baking-soda 1 teaspoon)
                       '(fetch-and-proportion ?proportioned-salt-2 ?ks-with-salt-2 ?ks-with-baking-soda ?target-container-10 salt 0.5 teaspoon)
                       '(fetch-and-proportion ?proportioned-water ?ks-with-water ?ks-with-salt-2 ?target-container-11 water 250 ml)
                       '(fetch-and-proportion ?proportioned-oil ?ks-with-oil ?ks-with-water ?target-container-12 oil 120 ml)
                       '(fetch-and-proportion ?proportioned-vinegar ?ks-with-vinegar ?ks-with-oil ?target-container-13 vinegar 1 tablespoon)
                       '(fetch-and-proportion ?proportioned-vanilla ?ks-with-vanilla ?ks-with-vinegar ?target-container-14 vanilla 1 teaspoon)
                       '(fetch ?small-bowl ?ks-with-small-bowl ?ks-with-vanilla small-bowl 1)
                       '(transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-small-bowl ?small-bowl ?softened-cheese ?quantity-a ?unit-a)
                       '(transfer-contents ?output-container-b ?rest-b ?output-ks-b ?output-ks-a ?output-container-a ?beaten-egg ?quantity-b ?unit-b)
                       '(transfer-contents ?output-container-c ?rest-c ?output-ks-c ?output-ks-b ?output-container-b ?proportioned-sugar-70 ?quantity-c ?unit-c)
                       '(transfer-contents ?output-container-d ?rest-d ?output-ks-d ?output-ks-c ?output-container-c ?proportioned-salt-1 ?quantity-d ?unit-d)
                       '(mix ?intermediate-mixture ?ks-with-intermediate-mixture ?output-ks-d ?output-container-d ?mixing-tool)
                       '(transfer-contents ?output-container-e ?rest-e ?output-ks-e ?ks-with-intermediate-mixture ?intermediate-mixture ?proportioned-chocolate-chips ?quantity-e ?unit-e)
                       '(mingle ?chips-mixture ?ks-with-chips-mixture ?output-ks-e ?output-container-e ?mingling-tool)
                       '(transfer-contents ?output-container-f ?rest-f ?output-ks-f ?ks-with-chips-mixture ?empty-large-bowl ?proportioned-sugar-210 ?quantity-f ?unit-f)
                       '(transfer-contents ?output-container-g ?rest-g ?output-ks-g ?output-ks-f ?output-container-f ?proportioned-flour ?quantity-g ?unit-g)
                       '(transfer-contents ?output-container-h ?rest-h ?output-ks-h ?output-ks-g ?output-container-g ?proportioned-cocoa ?quantity-h ?unit-h)
                       '(transfer-contents ?output-container-i ?rest-i ?output-ks-i ?output-ks-h ?output-container-h ?proportioned-baking-soda ?quantity-i ?unit-i)
                       '(transfer-contents ?output-container-j ?rest-j ?output-ks-j ?output-ks-i ?output-container-i ?proportioned-salt-2 ?quantity-j ?unit-j)
                       '(transfer-contents ?output-container-k ?rest-k ?output-ks-k ?output-ks-j ?output-container-j ?proportioned-water ?quantity-k ?unit-k)
                       '(transfer-contents ?output-container-l ?rest-l ?output-ks-l ?output-ks-k ?output-container-k ?proportioned-oil ?quantity-l ?unit-l)
                       '(transfer-contents ?output-container-m ?rest-m ?output-ks-m ?output-ks-l ?output-container-l ?proportioned-vinegar ?quantity-m ?unit-m)
                       '(transfer-contents ?output-container-n ?rest-n ?output-ks-n ?output-ks-m ?output-container-m ?proportioned-vanilla ?quantity-n ?unit-n)
                       '(mix ?dough ?ks-with-dough ?output-ks-n ?output-container-n ?mixing-tool)
                       '(fetch ?muffin-tins ?ks-with-muffin-tins ?ks-with-dough muffin-tins 1)
                       '(line ?lined-tins ?ks-with-lined-tins ?ks-with-muffin-tins ?muffin-tins paper-baking-cups)
                       '(portion-and-arrange ?portioned-dough ?ks-with-dough-portions ?ks-with-lined-tins ?dough ?portioning-quantity ?portioning-unit ?pattern ?lined-tins)
                       '(bake ?baked-cupcakes ?ks-with-baked-cupcakes ?ks-with-dough-portions ?portioned-dough ?oven 20 minute 180 degrees-celsius))
                 :primary-output-var
                 '?baked-cupcakes))

;; extra salads
(defparameter *black-bean-salad-2-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'black-bean-salad-2
                 :kitchen-state
                 *full-kitchen*
                 :meaning-network
                 (list '(get-kitchen ?kitchen)
                       '(fetch-and-proportion ?proportioned-lime-juice ?ks-with-lime-juice ?kitchen ?target-container-1 lime-juice 3 teaspoon)
                       '(fetch-and-proportion ?proportioned-garlic ?ks-with-garlic ?ks-with-lime-juice ?target-container-2 garlic 1 piece)
                       '(cut ?minced-garlic ?ks-with-minced-garlic ?ks-with-garlic ?proportioned-garlic minced ?knife)
                       '(fetch-and-proportion ?proportioned-cumin ?ks-with-cumin ?ks-with-minced-garlic ?target-container-3 ground-cumin 0.75 teaspoon)
                       '(fetch-and-proportion ?proportioned-salt ?ks-with-salt ?ks-with-cumin ?target-container-4 salt 0.5 teaspoon)
                       '(fetch-and-proportion ?proportioned-pepper ?ks-with-pepper ?ks-with-salt ?target-container-5 ground-black-pepper 0.25 teaspoon)
                       '(fetch-and-proportion ?proportioned-olive-oil ?ks-with-olive-oil ?ks-with-pepper ?target-container-6 olive-oil 2 tablespoon)
                       '(fetch-and-proportion ?proportioned-black-beans ?ks-with-black-beans ?ks-with-olive-oil ?target-container-7 black-bean 425 g)
                       '(wash ?washed-black-beans ?ks-with-washed-black-beans ?ks-with-black-beans ?proportioned-black-beans)
                       '(fetch-and-proportion ?proportioned-cherry-tomatoes ?ks-with-cherry-tomatoes ?ks-with-washed-black-beans ?target-container-8 cherry-tomato 300 g)
                       '(fetch-and-proportion ?proportioned-scallions ?ks-with-scallions ?ks-with-cherry-tomatoes ?target-container-9 green-onion 4 piece)
                       '(cut ?sliced-scallions ?ks-with-sliced-scallions ?ks-with-scallions ?proportioned-scallions slices ?knife)
                       '(fetch-and-proportion ?proportioned-red-pepper ?ks-with-red-pepper ?ks-with-sliced-scallions ?target-container-10 red-chili-pepper 1 piece)
                       '(cut ?cut-red-pepper ?ks-with-cut-red-pepper ?ks-with-red-pepper ?proportioned-red-pepper fine-slices ?knife)
                       '(fetch-and-proportion ?proportioned-cilantro ?ks-with-cilantro ?ks-with-cut-red-pepper ?target-container-11 fresh-cilantro 3 tablespoon)
                       '(cut ?chopped-cilantro ?ks-with-chopped-cilantro ?ks-with-cilantro ?proportioned-cilantro chopped ?knife)
                       '(fetch ?small-bowl ?ks-with-small-bowl ?ks-with-chopped-cilantro small-bowl 1)
                       '(transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-small-bowl ?small-bowl ?proportioned-lime-juice ?quantity-a ?unit-a)
                       '(transfer-contents ?output-container-b ?rest-b ?output-ks-b ?output-ks-a ?output-container-a ?minced-garlic ?quantity-b ?unit-b)
                       '(transfer-contents ?output-container-c ?rest-c ?output-ks-c ?output-ks-b ?output-container-b ?proportioned-cumin ?quantity-c ?unit-c)
                       '(transfer-contents ?output-container-d ?rest-d ?output-ks-d ?output-ks-c ?output-container-c ?proportioned-salt ?quantity-d ?unit-d)
                       '(transfer-contents ?output-container-e ?rest-e ?output-ks-e ?output-ks-d ?output-container-d ?proportioned-pepper ?quantity-e ?unit-e)
                       '(mix ?dressing ?ks-with-dressing ?output-ks-e ?output-container-e ?mixing-tool)
                       '(transfer-contents ?output-container-f ?rest-f ?output-ks-f ?ks-with-dressing ?output-container-e ?proportioned-olive-oil ?quantity-f ?unit-f)
                       '(mix ?stirred-mixture ?ks-with-stirred-mixture ?output-ks-f ?output-container-f ?mixing-tool)
                       '(fetch ?large-bowl ?ks-with-large-bowl ?ks-with-stirred-mixture large-bowl 1)
                       '(transfer-contents ?output-container-g ?rest-g ?output-ks-g ?ks-with-large-bowl ?large-bowl ?washed-black-beans ?quantity-g ?unit-g)
                       '(transfer-contents ?output-container-h ?rest-h ?output-ks-h ?output-ks-g ?output-container-g ?proportioned-cherry-tomatoes ?quantity-h ?unit-h)
                       '(transfer-contents ?output-container-i ?rest-i ?output-ks-i ?output-ks-h ?output-container-h ?sliced-scallions ?quantity-i ?unit-i)
                       '(transfer-contents ?output-container-j ?rest-j ?output-ks-j ?output-ks-i ?output-container-i ?cut-red-pepper ?quantity-j ?unit-j)
                       '(transfer-contents ?output-container-k ?rest-k ?output-ks-k ?output-ks-j ?output-container-j ?chopped-cilantro ?quantity-k ?unit-k)
                       '(mingle ?combined-mixture ?ks-with-combined-mixture ?output-ks-k ?output-container-k ?mingling-tool)
                       '(transfer-contents ?output-container-l ?rest-l ?output-ks-l ?ks-with-combined-mixture ?combined-mixture ?dressing ?quantity-l ?unit-l)
                       '(mingle ?salad ?ks-with-salad ?output-ks-l ?output-container-l ?mingling-tool)
                       '(leave-for-time ?served-salad ?ks-with-served-salad ?ks-with-salad ?salad 15 minute))
                 :primary-output-var
                 '?served-salad))

(defparameter *black-bean-salad-3-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'black-bean-salad-3
                 :kitchen-state
                 *full-kitchen*
                 :meaning-network
                 (list '(get-kitchen ?kitchen)
                       '(fetch-and-proportion ?proportioned-black-beans ?ks-with-black-beans ?kitchen ?target-container-1 black-bean 400 g)
                       '(fetch-and-proportion ?proportioned-corn ?ks-with-corn ?ks-with-black-beans ?target-container-2 corn 400 g)
                       '(fetch-and-proportion ?proportioned-onion ?ks-with-onion ?ks-with-corn ?target-container-3 onion 1 piece)
                       '(cut ?chopped-onion ?ks-with-chopped-onion ?ks-with-onion ?proportioned-onion chopped ?knife)
                       '(fetch-and-proportion ?proportioned-tomato ?ks-with-tomatoes ?ks-with-chopped-onion ?target-container-4 tomato 2 piece)
                       '(cut ?chopped-tomato ?ks-with-chopped-tomato ?ks-with-tomatoes ?proportioned-tomato chopped ?knife)
                       '(fetch-and-proportion ?proportioned-chili ?ks-with-chili ?ks-with-chopped-tomato ?target-container-5 green-chili-pepper 60 g)
                       '(cut ?diced-chili ?ks-with-diced-chili ?ks-with-chili ?proportioned-chili diced ?knife)
                       '(fetch-and-proportion ?proportioned-dressing ?ks-with-dressing ?ks-with-diced-chili ?target-container-6 trader-joes-cilantro-salad-dressing 3 tablespoon)
                       '(fetch-and-proportion ?proportioned-tabasco ?ks-with-tabasco ?ks-with-dressing ?target-container-7 tabasco 0.5 teaspoon)
                       '(fetch-and-proportion ?proportioned-pepper ?ks-with-pepper ?ks-with-tabasco ?target-container-8 ground-black-pepper 300 g)
                       '(transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-pepper ?empty-bowl ?proportioned-black-beans ?quantity-a ?unit-a)
                       '(transfer-contents ?output-container-b ?rest-b ?output-ks-b ?output-ks-a ?output-container-a ?proportioned-corn ?quantity-b ?unit-b)
                       '(transfer-contents ?output-container-c ?rest-c ?output-ks-c ?output-ks-b ?output-container-b ?chopped-onion ?quantity-c ?unit-c)
                       '(transfer-contents ?output-container-d ?rest-d ?output-ks-d ?output-ks-c ?output-container-c ?chopped-tomato ?quantity-d ?unit-d)
                       '(transfer-contents ?output-container-e ?rest-e ?output-ks-e ?output-ks-d ?output-container-d ?diced-chili ?quantity-e ?unit-e)
                       '(transfer-contents ?output-container-f ?rest-f ?output-ks-f ?output-ks-e ?output-container-e ?proportioned-dressing ?quantity-f ?unit-f)
                       '(transfer-contents ?output-container-g ?rest-g ?output-ks-g ?output-ks-f ?output-container-f ?proportioned-tabasco ?quantity-g ?unit-g)
                       '(transfer-contents ?output-container-h ?rest-h ?output-ks-h ?output-ks-g ?output-container-g ?proportioned-pepper ?quantity-h ?unit-h)
                       '(mingle ?salad ?ks-with-salad ?output-ks-h ?output-container-h ?mingling-tool)
                       '(refrigerate ?cooled-salad ?ks-with-cooled-salad ?ks-with-salad ?salad ?fridge 1 hour))
                 :primary-output-var
                 '?cooled-salad))

(defparameter *black-bean-salad-4-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'black-bean-salad-4
                 :kitchen-state
                 *full-kitchen*
                 :meaning-network
                 (list '(get-kitchen ?kitchen)
                       '(fetch-and-proportion ?proportioned-black-beans ?ks-with-black-beans ?kitchen ?target-container-1 black-bean 400 g)
                       '(wash ?washed-black-beans ?ks-with-washed-black-beans ?ks-with-black-beans ?proportioned-black-beans)
                       '(fetch-and-proportion ?proportioned-bell-pepper ?ks-with-bell-pepper ?ks-with-washed-black-beans ?target-container-2 red-bell-pepper 150 g)
                       '(seed ?seeded-bell-pepper ?bell-pepper-seeds ?ks-with-seeded-bell-pepper ?ks-with-bell-pepper ?proportioned-bell-pepper ?knife)
                       '(cut ?diced-bell-pepper ?ks-with-diced-bell-pepper ?ks-with-seeded-bell-pepper ?seeded-bell-pepper diced ?knife)
                       '(fetch-and-proportion ?proportioned-onion ?ks-with-onion ?ks-with-diced-bell-pepper ?target-container-3 green-onion 6 piece)
                       '(cut ?sliced-onion ?ks-with-sliced-onion ?ks-with-onion ?proportioned-onion fine-slices ?knife)
                       '(fetch-and-proportion ?proportioned-jalapeno ?ks-with-jalapeno ?ks-with-sliced-onion ?target-container-4 jalapeno 1 piece)
                       '(seed ?seeded-jalapeno ?jalapeno-seeds ?ks-with-seeded-jalapeno ?ks-with-jalapeno ?proportioned-jalapeno ?knife)
                       '(cut ?minced-jalapeno ?ks-with-minced-jalapeno ?ks-with-seeded-jalapeno ?seeded-jalapeno minced ?knife)
                       '(fetch-and-proportion ?proportioned-cilantro ?ks-with-cilantro ?ks-with-minced-jalapeno ?target-container-5 fresh-cilantro 20 g)
                       '(cut ?chopped-cilantro ?ks-with-chopped-cilantro ?ks-with-cilantro ?proportioned-cilantro minced ?knife)
                       '(fetch-and-proportion ?proportioned-lime-juice ?ks-with-lime-juice ?ks-with-chopped-cilantro ?target-container-6 lime-juice 250 ml)
                       '(fetch-and-proportion ?proportioned-olive-oil ?ks-with-olive-oil ?ks-with-lime-juice ?target-container-7 olive-oil 1 tablespoon)
                       '(fetch-and-proportion ?proportioned-mango ?ks-with-mango ?ks-with-olive-oil ?target-container-8 mango 330 g)
                       '(cut ?diced-mango ?ks-with-diced-mango ?ks-with-mango ?proportioned-mango diced ?knife)
                       '(fetch-and-proportion ?proportioned-salt ?ks-with-salt ?ks-with-diced-mango ?target-container-9 salt 0.5 teaspoon)
                       '(fetch ?large-bowl ?ks-with-large-bowl ?ks-with-salt large-bowl 1)
                       '(transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-large-bowl ?large-bowl ?washed-black-beans ?quantity-a ?unit-a)
                       '(transfer-contents ?output-container-b ?rest-b ?output-ks-b ?output-ks-a ?output-container-a ?diced-bell-pepper ?quantity-b ?unit-b)
                       '(transfer-contents ?output-container-c ?rest-c ?output-ks-c ?output-ks-b ?output-container-b ?sliced-onion ?quantity-c ?unit-c)
                       '(transfer-contents ?output-container-d ?rest-d ?output-ks-d ?output-ks-c ?output-container-c ?minced-jalapeno ?quantity-d ?unit-d)
                       '(transfer-contents ?output-container-e ?rest-e ?output-ks-e ?output-ks-d ?output-container-d ?chopped-cilantro ?quantity-e ?unit-e)
                       '(mingle ?bean-mix ?ks-with-bean-mix ?output-ks-e ?output-container-e ?mingling-tool)
                       '(fetch ?small-bowl ?ks-with-small-bowl ?ks-with-bean-mix small-bowl 1)
                       '(transfer-contents ?output-container-f ?rest-f ?output-ks-f ?ks-with-small-bowl ?small-bowl ?proportioned-olive-oil ?quantity-f ?unit-f)
                       '(transfer-contents ?output-container-g ?rest-g ?output-ks-g ?output-ks-f ?output-container-f ?proportioned-lime-juice ?quantity-g ?unit-g)
                       '(mix ?liquid-mixture ?ks-with-liquid-mixture ?output-ks-g ?output-container-g ?mixing-tool)
                       '(transfer-contents ?output-container-h ?rest-h ?output-ks-h ?ks-with-liquid-mixture ?bean-mix ?liquid-mixture ?quantity-h ?unit-h)
                       '(mingle ?black-bean-salad ?ks-with-black-bean-salad ?output-ks-h ?output-container-h ?mingling-tool)
                       '(transfer-contents ?output-container-i ?rest-i ?output-ks-i ?ks-with-black-bean-salad ?black-bean-salad ?diced-mango ?quantity-i ?unit-i)
                       '(mingle ?black-bean-mango-salad ?ks-with-black-bean-mango-salad ?output-ks-i ?output-container-i ?mingling-tool)
                       '(sprinkle ?seasoned-salad ?ks-with-seasoned-salad ?ks-with-black-bean-mango-salad ?black-bean-mango-salad ?proportioned-salt))
                 :primary-output-var
                 '?seasoned-salad))

(defparameter *black-bean-salad-5-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'black-bean-salad-5
                 :kitchen-state
                 *full-kitchen*
                 :meaning-network
                 (list '(get-kitchen ?kitchen)
                       '(fetch-and-proportion ?proportioned-black-beans ?ks-with-black-beans ?kitchen ?target-container-1 black-bean 400 g)
                       '(wash ?washed-black-beans ?ks-with-washed-black-beans ?ks-with-black-beans ?proportioned-black-beans)
                       '(fetch-and-proportion ?proportioned-frozen-corn ?ks-with-frozen-corn ?ks-with-washed-black-beans ?target-container-2 frozen-corn 250 g)
                       '(bring-to-temperature ?defrosted-corn ?ks-with-defrosted-corn ?ks-with-frozen-corn ?proportioned-frozen-corn ?room-temp-quantity ?room-temp-unit)
                       '(fetch-and-proportion ?proportioned-scallions ?ks-with-scallions ?ks-with-defrosted-corn ?target-container-3 green-onion 30 g)
                       '(cut ?chopped-scallions ?ks-with-chopped-scallions ?ks-with-scallions ?proportioned-scallions chopped ?knife)
                       '(fetch-and-proportion ?proportioned-jalapeno ?ks-with-jalapeno ?ks-with-chopped-scallions ?target-container-4 jalapeno 1 piece)
                       '(seed ?seeded-jalapeno ?jalapeno-seeds ?ks-with-seeded-jalapeno ?ks-with-jalapeno ?proportioned-jalapeno ?knife)
                       '(cut ?minced-jalapeno ?ks-with-minced-jalapeno ?ks-with-seeded-jalapeno ?seeded-jalapeno minced ?knife)
                       '(fetch-and-proportion ?proportioned-bell-pepper ?ks-with-bell-pepper ?ks-with-minced-jalapeno ?target-container-5 red-bell-pepper 1 piece)
                       '(seed ?seeded-bell-peper ?bell-pepper-seeds ?ks-with-seeded-bell-pepper ?ks-with-bell-pepper ?proportioned-bell-pepper ?knife)
                       '(cut ?chopped-bell-pepper ?ks-with-chopped-bell-pepper ?ks-with-seeded-bell-pepper ?seeded-bell-peper chopped ?knife)
                       '(fetch-and-proportion ?proportioned-lime-juice ?ks-with-lime-juice ?ks-with-chopped-bell-pepper ?target-container-6 lime-juice 2 tablespoon)
                       '(fetch-and-proportion ?proportioned-olive-oil ?ks-with-olive-oil ?ks-with-lime-juice ?target-container-7 olive-oil 1 tablespoon)
                       '(fetch-and-proportion ?proportioned-avocado ?ks-with-avocado ?ks-with-olive-oil ?target-container-8 avocado 1 piece)
                       '(cut ?chopped-avocado ?ks-with-chopped-avocado ?ks-with-avocado ?proportioned-avocado chopped ?knife)
                       '(fetch-and-proportion ?proportioned-sugar ?ks-with-sugar ?ks-with-chopped-avocado ?target-container-9 white-sugar 1 teaspoon)
                       '(fetch-and-proportion ?proportioned-salt ?ks-with-salt ?ks-with-sugar ?target-container-10 salt 0.5 teaspoon)
                       '(fetch-and-proportion ?proportioned-pepper ?ks-with-pepper ?ks-with-salt ?target-container-11 ground-black-pepper 0.5 teaspoon)
                       '(fetch-and-proportion ?proportioned-cilantro ?ks-with-cilantro ?ks-with-pepper ?target-container-12 fresh-cilantro 10 g)
                       '(cut ?chopped-cilantro ?ks-with-chopped-cilantro ?ks-with-cilantro ?proportioned-cilantro diced ?knife)
                       '(fetch ?large-bowl ?ks-with-large-bowl ?ks-with-chopped-cilantro large-bowl 1)
                       '(transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-large-bowl ?large-bowl ?washed-black-beans ?quantity-a ?unit-a)
                       '(transfer-contents ?output-container-b ?rest-b ?output-ks-b ?output-ks-a ?output-container-a ?defrosted-corn ?quantity-b ?unit-b)
                       '(transfer-contents ?output-container-c ?rest-c ?output-ks-c ?output-ks-b ?output-container-b ?chopped-scallions ?quantity-c ?unit-c)
                       '(transfer-contents ?output-container-d ?rest-d ?output-ks-d ?output-ks-c ?output-container-c ?minced-jalapeno ?quantity-d ?unit-d)
                       '(transfer-contents ?output-container-e ?rest-e ?output-ks-e ?output-ks-d ?output-container-d ?chopped-bell-pepper ?quantity-e ?unit-e)
                       '(transfer-contents ?output-container-f ?rest-f ?output-ks-f ?output-ks-e ?output-container-e ?proportioned-lime-juice ?quantity-f ?unit-f)
                       '(transfer-contents ?output-container-g ?rest-g ?output-ks-g ?output-ks-f ?output-container-f ?proportioned-olive-oil ?quantity-g ?unit-g)
                       '(mingle ?bean-mix ?ks-with-bean-mix ?output-ks-g ?output-container-g ?mingling-tool)
                       '(transfer-contents ?output-container-h ?rest-h ?output-ks-h ?ks-with-bean-mix ?bean-mix ?chopped-avocado ?quantity-h ?unit-h)
                       '(mingle ?salad-base ?ks-with-salad-base ?output-ks-h ?output-container-h ?mingling-tool)
                       '(transfer-contents ?output-container-i ?rest-i ?output-ks-i ?ks-with-salad-base ?salad-base ?proportioned-salt ?quantity-i ?unit-i)
                       '(transfer-contents ?output-container-j ?rest-j ?output-ks-j ?ks-with-bean-mix ?output-container-i ?proportioned-pepper ?quantity-j ?unit-j)
                       '(mingle ?seasoned-salad ?ks-with-seasoned-salad ?output-ks-j ?output-container-j ?mingling-tool)
                       '(sprinkle ?sugar-salad ?ks-with-sugar-salad ?ks-with-seasoned-salad ?seasoned-salad ?proportioned-sugar)
                       '(refrigerate ?cooled-salad ?ks-with-cooled-salad ?ks-with-sugar-salad ?sugar-salad ?fridge ?cooling-quantity ?cooling-unit)
                       '(transfer-contents ?output-container-k ?rest-k ?output-ks-k ?ks-with-cooled-salad ?cooled-salad ?chopped-cilantro ?quantity-k ?unit-k)
                       '(mingle ?black-bean-salad ?ks-with-black-bean-salad ?output-ks-k ?output-container-k ?mingling-tool))
                 :primary-output-var
                 '?black-bean-salad))

(defparameter *classic-greek-salad-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'classic-greek-salad
                 :kitchen-state
                 *full-kitchen*
                 :meaning-network
                 (list '(get-kitchen ?kitchen)
                       '(fetch-and-proportion ?proportioned-cucumber ?ks-with-cucumber ?kitchen ?target-container-1 cucumber 1 piece)
                       '(fetch-and-proportion ?proportioned-tomato ?ks-with-tomato ?ks-with-cucumber ?target-container-2 tomato 1 piece)
                       '(fetch-and-proportion ?proportioned-radish ?ks-with-radish ?ks-with-tomato ?target-container-3 radish 3 piece)
                       '(fetch-and-proportion ?proportioned-onion ?ks-with-onion ?ks-with-radish ?target-container-4 red-onion 1 piece)
                       '(fetch-and-proportion ?proportioned-bell-pepper ?ks-with-bell-pepper ?ks-with-onion ?target-container-5 red-bell-pepper 1 piece)
                       '(fetch-and-proportion ?proportioned-cheese ?ks-with-cheese ?ks-with-bell-pepper ?target-container-6 feta-cheese 115 g)
                       '(cut ?cubed-cheese ?ks-with-cubed-cheese ?ks-with-cheese ?proportioned-cheese cubes ?knife)
                       '(fetch-and-proportion ?proportioned-olives ?ks-with-olives ?ks-with-cubed-cheese ?target-container-7 black-olive 90 g)
                       '(seed ?pitted-olives ?olive-pits ?ks-with-pitted-olives ?ks-with-olives ?proportioned-olives ?knife)
                       '(fetch-and-proportion ?proportioned-olive-oil ?ks-with-olive-oil ?ks-with-pitted-olives ?target-container-8 olive-oil 4 tablespoon)
                       '(fetch-and-proportion ?proportioned-lemon-juice ?ks-with-lemon-juice ?ks-with-olive-oil ?target-container-9 lemon-juice 8 tablespoon)
                       '(fetch-and-proportion ?proportioned-garlic ?ks-with-garlic ?ks-with-lemon-juice ?target-container-10 garlic 1 piece)
                       '(cut ?minced-garlic ?ks-with-minced-garlic ?ks-with-garlic ?proportioned-garlic minced ?knife)
                       '(fetch-and-proportion ?proportioned-oregano ?ks-with-oregano ?ks-with-minced-garlic ?target-container-11 fresh-oregano 1.5 teaspoon)
                       '(fetch-and-proportion ?proportioned-lettuce ?ks-with-lettuce ?ks-with-oregano ?target-container-12 romaine-lettuce 1 piece)
                       '(wash ?washed-lettuce ?ks-with-washed-lettuce ?ks-with-lettuce ?proportioned-lettuce)
                       '(cut ?chopped-lettuce ?ks-with-chopped-lettuce ?ks-with-washed-lettuce ?washed-lettuce chopped ?knife)
                       '(fetch-and-proportion ?proportioned-salt ?ks-with-salt ?ks-with-chopped-lettuce ?target-container-13 salt 1/2 teaspoon)
                       '(fetch-and-proportion ?proportioned-pepper ?ks-with-pepper ?ks-with-salt ?target-container-14 ground-black-pepper 1/2 teaspoon)
                       '(peel ?peeled-cucumber ?cucumber-peels ?ks-with-peeled-cucumber ?ks-with-pepper ?proportioned-cucumber ?knife)
                       '(cut ?sliced-cucumber ?ks-with-sliced-cucumber ?ks-with-peeled-cucumber ?peeled-cucumber slices ?knife)
                       '(seed ?seeded-tomato ?tomato-core ?ks-with-seeded-tomato ?ks-with-sliced-cucumber ?proportioned-tomato ?knife)
                       '(cut ?chopped-tomato ?ks-with-chopped-tomato ?ks-with-seeded-tomato ?proportioned-tomato chopped ?knife)
                       '(cut ?sliced-radish ?ks-with-sliced-radish ?ks-with-chopped-tomato ?proportioned-radish fine-slices ?knife)
                       '(cut ?sliced-onion ?ks-with-sliced-onion ?ks-with-sliced-radish ?proportioned-onion fine-slices ?knife)
                       '(cut ?sliced-bell-pepper ?ks-with-sliced-bell-pepper ?ks-with-sliced-onion ?proportioned-onion fine-slices ?knife)
                       '(fetch ?large-bowl ?ks-with-large-bowl ?ks-with-sliced-bell-pepper large-bowl 1)
                       '(transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-large-bowl ?large-bowl ?sliced-cucumber ?quantity-a ?unit-a)
                       '(transfer-contents ?output-container-b ?rest-b ?output-ks-b ?output-ks-a ?output-container-a ?chopped-tomato ?quantity-b ?unit-b)
                       '(transfer-contents ?output-container-c ?rest-c ?output-ks-c ?output-ks-b ?output-container-b ?sliced-radish ?quantity-c ?unit-c)
                       '(transfer-contents ?output-container-d ?rest-d ?output-ks-d ?output-ks-c ?output-container-c ?sliced-onion ?quantity-d ?unit-d)
                       '(mingle ?veggies-mix ?ks-with-veggies-mix ?output-ks-d ?output-container-d ?wooden-spoon)
                       '(transfer-contents ?output-container-e ?rest-e ?output-ks-e ?ks-with-veggies-mix ?veggies-mix ?cubed-cheese ?quantity-e ?unit-e)
                       '(transfer-contents ?output-container-f ?rest-f ?output-ks-f ?output-ks-e ?output-container-e ?pitted-olives ?quantity-f ?unit-f)
                       '(mingle ?cheesy-veggies-mix ?ks-with-cheesy-veggies-mix ?output-ks-f ?output-container-f ?wooden-spoon)
                       '(fetch ?small-bowl ?ks-with-small-bowl ?ks-with-cheesy-veggies-mix small-bowl 1)
                       '(transfer-contents ?output-container-g ?rest-g ?output-ks-g ?ks-with-small-bowl ?small-bowl ?proportioned-olive-oil ?quantity-g ?unit-g)
                       '(transfer-contents ?output-container-h ?rest-h ?output-ks-h ?output-ks-g ?output-container-g ?proportioned-lemon-juice ?quantity-h ?unit-h)
                       '(transfer-contents ?output-container-i ?rest-i ?output-ks-i ?output-ks-h ?output-container-h ?minced-garlic ?quantity-i ?unit-i)
                       '(transfer-contents ?output-container-j ?rest-j ?output-ks-j ?output-ks-i ?output-container-i ?proportioned-oregano ?quantity-j ?unit-j)
                       '(mix ?dressing ?ks-with-dressing ?output-ks-j ?output-container-j ?whisk)
                       '(transfer-contents ?output-container-k ?rest-k ?output-ks-k ?ks-with-dressing ?cheesy-veggies-mix ?dressing ?quantity-k ?unit-k)
                       '(mingle ?salad ?ks-with-salad ?output-ks-k ?output-container-k ?wooden-spoon)
                       '(sprinkle ?salt-salad ?ks-with-salt-salad ?ks-with-salad ?salad ?proportioned-salt)
                       '(sprinkle ?greek-salad ?ks-with-greek-salad ?ks-with-salt-salad ?salt-salad ?proportioned-pepper))
                 :primary-output-var
                 '?greek-salad))

(defparameter *classic-potato-salad-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'classic-potato-salad
                 :kitchen-state
                 *full-kitchen*
                 :meaning-network
                 (list '(get-kitchen ?kitchen)
                       '(fetch-and-proportion ?proportioned-potatoes ?ks-with-potatoes ?kitchen ?target-container-1 potato 8 piece)
                       '(fetch-and-proportion ?proportioned-mayo ?ks-with-mayo ?ks-with-potatoes ?target-container-2 mayonnaise 350 g)
                       '(fetch-and-proportion ?proportioned-cider-vinegar ?ks-with-cider-vinegar ?ks-with-mayo ?target-container-3 cider-vinegar 2 tablespoon)
                       '(fetch-and-proportion ?proportioned-sugar ?ks-with-sugar ?ks-with-cider-vinegar ?target-container-4 white-sugar 2 tablespoon)
                       '(fetch-and-proportion ?proportioned-mustard ?ks-with-mustard ?ks-with-sugar ?target-container-5 yellow-mustard 1 tablespoon)
                       '(fetch-and-proportion ?proportioned-salt-1 ?ks-with-salt-1 ?ks-with-mustard ?target-container-6 salt 1/2 teaspoon)
                       '(fetch-and-proportion ?proportioned-salt-2 ?ks-with-salt-2 ?ks-with-salt-1 ?target-container-7 salt 1/2 teaspoon)
                       '(fetch-and-proportion ?proportioned-garlic-powder ?ks-with-garlic-powder ?ks-with-salt-2 ?target-container-8 garlic-powder 1 teaspoon)
                       '(fetch-and-proportion ?proportioned-pepper ?ks-with-pepper ?ks-with-garlic-powder ?target-container-9 ground-black-pepper 1/2 teaspoon)
                       '(fetch-and-proportion ?proportioned-celery ?ks-with-celery ?ks-with-pepper ?target-container-10 celery 2 piece)
                       '(cut ?sliced-celery ?ks-with-sliced-celery ?ks-with-celery ?proportioned-celery slices ?knife)
                       '(fetch-and-proportion ?proportioned-onion ?ks-with-onion ?ks-with-sliced-celery ?target-container-11 onion 50 g)
                       '(cut ?minced-onion ?ks-with-minced-onion ?ks-with-onion ?proportioned-onion minced ?knife)
                       '(fetch-and-proportion ?proportioned-eggs ?ks-with-eggs ?ks-with-minced-onion ?target-container-12 hard-boiled-egg 5 piece)
                       '(fetch-and-proportion ?proportioned-paprika ?ks-with-paprika ?ks-with-eggs ?target-container-13 paprika-powder 1/2 teaspoon)
                       '(fetch ?cooking-pot ?ks-with-cooking-pot ?ks-with-paprika cooking-pot 1)
                       '(fetch-and-proportion ?proportioned-water ?ks-with-water ?ks-with-cooking-pot ?target-container-14 water 500 ml)
                       '(peel ?peeled-potato ?potato-peel ?ks-with-peeled-potato ?ks-with-water ?proportioned-potatoes ?knife)    
                       '(transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-peeled-potato ?cooking-pot ?proportioned-water ?quantity-a ?unit-a)
                       '(transfer-contents ?output-container-b ?rest-b ?output-ks-b ?output-ks-a ?output-container-a ?proportioned-salt-1 ?quantity-b ?unit-b)
                       '(transfer-contents ?output-container-c ?rest-c ?output-ks-c ?output-ks-b ?output-container-b ?peeled-potato ?quantity-c ?unit-c)
                       '(boil ?boiled-potatoes ?ks-with-boiled-potatoes ?output-ks-c ?output-container-c ?stove ?heating-mode ?boiling-time-qty ?boiling-time-unit)
                       '(drain ?drained-potato ?rest-liquid ?ks-with-drained-potatoes ?ks-with-boiled-potatoes ?boiled-potatoes ?colander)
                       '(bring-to-temperature ?cooled-potatoes ?ks-with-cooled-potatoes ?ks-with-drained-potatoes ?drained-potato ?room-temp-quantity ?room-temp-unit)
                       '(fetch ?large-bowl ?ks-with-large-bowl ?ks-with-cooled-potatoes large-bowl 1)
                       '(cut ?diced-potato ?ks-with-diced-potato ?ks-with-large-bowl ?drained-potato diced ?knife)
                       '(transfer-contents ?diced-potatoes-in-large-bowl ?rest-d ?output-ks-d ?ks-with-diced-potato ?large-bowl ?diced-potato ?quantity-d ?unit-d)
                       '(transfer-contents ?output-container-e ?rest-e ?output-ks-e ?output-ks-d ?empty-bowl ?proportioned-mayo ?quantity-e ?unit-e)
                       '(transfer-contents ?output-container-f ?rest-f ?output-ks-f ?output-ks-e ?output-container-e ?proportioned-cider-vinegar ?quantity-f ?unit-f)  
                       '(transfer-contents ?output-container-g ?rest-g ?output-ks-g ?output-ks-f ?output-container-f ?proportioned-sugar ?quantity-g ?unit-g)
                       '(transfer-contents ?output-container-h ?rest-h ?output-ks-h ?output-ks-g ?output-container-g ?proportioned-mustard ?quantity-h ?unit-h)
                       '(transfer-contents ?output-container-i ?rest-i ?output-ks-i ?output-ks-h ?output-container-h ?proportioned-salt-2 ?quantity-i ?unit-i)
                       '(transfer-contents ?output-container-j ?rest-j ?output-ks-j ?output-ks-i ?output-container-i ?proportioned-garlic-powder ?quantity-j ?unit-j)
                       '(transfer-contents ?output-container-k ?rest-k ?output-ks-k ?output-ks-j ?output-container-j ?proportioned-pepper ?quantity-k ?unit-k)
                       '(mix ?dressing ?ks-with-dressing ?output-ks-k ?output-container-k ?whisk)
                       '(transfer-contents ?output-container-l ?rest-l ?output-ks-l ?ks-with-dressing ?diced-potatoes-in-large-bowl ?dressing ?quantity-l ?unit-l)
                       '(transfer-contents ?output-container-m ?rest-m ?output-ks-m ?output-ks-l ?output-container-l ?sliced-celery ?quantity-m ?unit-m)
                       '(transfer-contents ?output-container-n ?rest-n ?output-ks-n ?output-ks-m ?output-container-m ?minced-onion ?quantity-n ?unit-n)
                       '(mingle ?salad ?ks-with-salad ?output-ks-n ?output-container-n ?wooden-spoon)
                       '(transfer-contents ?output-container-o ?rest-o ?output-ks-o ?ks-with-salad ?salad ?proportioned-eggs ?quantity-o ?unit-o)
                       '(mingle ?salad-with-eggs ?ks-with-egg-salad ?output-ks-o ?output-container-o ?wooden-spoon)
                       '(sprinkle ?potato-salad ?ks-with-potato-salad ?ks-with-egg-salad ?salad-with-eggs ?proportioned-paprika))
                 :primary-output-var
                       '?potato-salad))

(defparameter *cole-slaw-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'cole-slaw
                 :kitchen-state
                 *full-kitchen*
                 :meaning-network
                 (list '(get-kitchen ?kitchen)
                       '(fetch-and-proportion ?proportioned-cabbage ?ks-with-cabbage ?kitchen ?target-container-1 green-cabbage 1 piece)
                       '(fetch-and-proportion ?proportioned-carrots ?ks-with-carrots ?ks-with-cabbage ?target-container-2 carrot 2 piece)
                       '(fetch-and-proportion ?proportioned-onion ?ks-with-onion ?ks-with-carrots ?target-container-3 onion 1 piece)
                       '(fetch-and-proportion ?proportioned-green-pepper ?ks-with-green-pepper ?ks-with-onion ?target-container-4 green-chili-pepper 2 tablespoon)
                       '(fetch-and-proportion ?proportioned-sugar ?ks-with-sugar ?ks-with-green-pepper ?target-container-5 white-sugar 200 g)
                       '(fetch-and-proportion ?proportioned-vinegar ?ks-with-vinegar ?ks-with-sugar ?target-container-6 cider-vinegar 230 g)
                       '(fetch-and-proportion ?proportioned-salt ?ks-with-salt ?ks-with-vinegar ?target-container-7 salt 1 teaspoon)
                       '(fetch-and-proportion ?proportioned-horseradish ?ks-with-horseradish ?ks-with-salt ?target-container-8 grated-horseradish 1 tablespoon)
                       '(fetch-and-proportion ?proportioned-mustard-seed ?ks-with-mustard-seed ?ks-with-horseradish ?target-container-10 mustard-seed 1 teaspoon)
                       '(fetch-and-proportion ?proportioned-celery-seed ?ks-with-celery-seed ?ks-with-mustard-seed ?target-container-11 celery-seed 1 teaspoon)
                       '(fetch-and-proportion ?proportioned-turmeric ?ks-with-turmeric ?ks-with-celery-seed ?target-container-12 turmeric-powder 1 teaspoon)
                       '(cut ?shredded-cabbage ?ks-with-shredded-cabbage ?ks-with-turmeric ?proportioned-cabbage shredded ?knife)
                       '(cut ?shredded-carrots ?ks-with-shredded-carrots ?ks-with-shredded-cabbage ?proportioned-carrots shredded ?knife)
                       '(transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-shredded-carrots ?empty-container ?shredded-cabbage ?quantity-a ?unit-a)
                       '(transfer-contents ?output-container-b ?rest-b ?output-ks-b ?output-ks-a ?output-container-a ?shredded-carrots ?quantity-b ?unit-b)
                       '(mingle ?cabbage-mix ?ks-with-cabbage-mix ?output-ks-b ?output-container-b ?wooden-spoon)
                       '(cut ?chopped-cabbage-mix ?ks-with-chopped-cabbage-mix ?ks-with-cabbage-mix ?cabbage-mix chopped ?knife)
                       '(cut ?chopped-green-pepper ?ks-with-chopped-green-pepper ?ks-with-chopped-cabbage-mix ?proportioned-green-pepper chopped ?knife)
                       '(cut ?chopped-onion ?ks-with-chopped-onion ?ks-with-chopped-green-pepper ?proportioned-onion chopped ?knife)
                       '(transfer-contents ?output-container-c ?rest-c ?output-ks-c ?ks-with-chopped-onion ?chopped-cabbage-mix ?chopped-green-pepper ?quantity-c ?unit-c)
                       '(transfer-contents ?output-container-d ?rest-d ?output-ks-d ?output-ks-c ?output-container-c ?chopped-onion ?quantity-d ?unit-d)
                       '(mingle ?slaw ?ks-with-slaw ?output-ks-d ?output-container-d ?wooden-spoon)
                       '(fetch ?cooking-pot ?ks-with-cooking-pot ?ks-with-slaw cooking-pot 1)
                       '(transfer-contents ?output-container-e ?rest-e ?output-ks-e ?ks-with-cooking-pot ?cooking-pot ?proportioned-vinegar ?quantity-e ?unit-e)
                       '(transfer-contents ?output-container-f ?rest-f ?output-ks-f ?output-ks-e ?output-container-e ?proportioned-sugar ?quantity-f ?unit-f)
                       '(transfer-contents ?output-container-g ?rest-g ?output-ks-g ?output-ks-f ?output-container-f ?proportioned-salt ?quantity-g ?unit-g)
                       '(transfer-contents ?output-container-h ?rest-h ?output-ks-h ?output-ks-g ?output-container-g ?proportioned-horseradish ?quantity-h ?unit-h)
                       '(transfer-contents ?output-container-i ?rest-i ?output-ks-i ?output-ks-h ?output-container-h ?proportioned-mustard-seed ?quantity-i ?unit-i)
                       '(transfer-contents ?output-container-j ?rest-j ?output-ks-j ?output-ks-i ?output-container-i ?proportioned-celery-seed ?quantity-j ?unit-j)
                       '(transfer-contents ?output-container-k ?rest-k ?output-ks-k ?output-ks-j ?output-container-j ?proportioned-turmeric ?quantity-k ?unit-k)
                       '(boil ?dressing ?ks-with-dressing ?output-ks-k ?output-container-k ?stove ?heating-mode ?boiling-time-qty ?boiling-time-unit)
                       '(transfer-contents ?output-container-l ?rest-l ?output-ks-l ?ks-with-dressing ?slaw ?dressing ?quantity-l ?unit-l)
                       '(refrigerate ?cole-slaw ?ks-with-cole-slaw ?output-ks-l ?output-container-l ?fridge ?cooling-quantity ?cooling-unit))
                 :primary-output-var
                       '?cole-slaw))

(defparameter *cranberry-fluff-salad-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'cranberry-fluff-salad
                 :kitchen-state
                 *full-kitchen*
                 :meaning-network
                 (list '(get-kitchen ?kitchen)
                       '(fetch-and-proportion ?proportioned-cranberries ?ks-with-cranberries ?kitchen ?target-container-1 cranberry 200 g)
                       '(cut ?chopped-cranberries ?ks-with-chopped-cranberries ?ks-with-cranberries ?proportioned-cranberries chopped ?knife)
                       '(fetch-and-proportion ?proportioned-sugar ?ks-with-sugar ?ks-with-chopped-cranberries ?target-container-2 white-sugar 125 g)
                       '(fetch-and-proportion ?proportioned-pineapple ?ks-with-pineapple ?ks-with-sugar ?target-container-3 crushed-pineapple-in-syrup 240 g)
                       '(drain ?drained-pineapple ?rest-liquid ?ks-with-drained-pineapple ?ks-with-pineapple ?proportioned-pineapple ?colander)
                       '(fetch-and-proportion ?proportioned-marshmallow ?ks-with-marshmallow ?ks-with-drained-pineapple ?target-container-4 marshmallow 450 g)
                       '(fetch-and-proportion ?proportioned-cream ?ks-with-heavy-cream ?ks-with-marshmallow ?target-container-5 heavy-cream 230 g)
                       '(beat ?whipped-cream ?ks-with-whipped-cream ?ks-with-heavy-cream ?proportioned-cream ?whisk)
                       '(fetch-and-proportion ?proportioned-walnut ?ks-with-walnut ?ks-with-whipped-cream ?target-container-6 walnut 60 g)
                       '(cut ?chopped-walnut ?ks-with-chopped-walnut ?ks-with-walnut ?proportioned-walnut chopped ?knife)
                       '(transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-chopped-walnut ?empty-container ?chopped-cranberries ?quantity-a ?unit-a)
                       '(transfer-contents ?output-container-b ?rest-b ?output-ks-b ?output-ks-a ?output-container-a ?proportioned-sugar ?quantity-b ?unit-b)
                       '(transfer-contents ?output-container-c ?rest-c ?output-ks-c ?output-ks-b ?output-container-b ?drained-pineapple ?quantity-c ?unit-c)
                       '(mingle ?intermediate-mix ?ks-with-intermediate-mix ?output-ks-c ?output-container-c ?mingling-tool)
                       '(leave-for-time ?rested-mix ?ks-with-rested-mix ?ks-with-intermediate-mix ?intermediate-mix 0.5 hour)
                       '(transfer-contents ?output-container-d ?rest-d ?output-ks-d ?ks-with-rested-mix ?rested-mix ?whipped-cream ?quantity-d ?unit-d)
                       '(transfer-contents ?output-container-e ?rest-e ?output-ks-e ?output-ks-d ?output-container-d ?chopped-walnut ?quantity-e ?unit-e)
                       '(mingle ?salad ?ks-with-salad ?output-ks-e ?output-container-e ?mingling-tool)
                       '(refrigerate ?cranberry-fluff-salad ?ks-with-cranberry-fluff-salad ?ks-with-salad ?salad ?fridge 8 hour))
                 :primary-output-var
                 '?cranberry-fluff-salad))

(defparameter *avocado-chicken-salad-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'avocado-chicken-salad
                 :kitchen-state
                 *full-kitchen*
                 :meaning-network
                 (list '(get-kitchen ?kitchen)
                       '(fetch-and-proportion ?proportioned-chicken ?ks-with-chicken ?kitchen ?target-container-1 cooked-chicken 140 g)
                       '(cut ?chopped-chicken ?ks-with-chopped-chicken ?ks-with-chicken ?proportioned-chicken chopped ?knife)
                       '(fetch-and-proportion ?proportioned-avocado ?ks-with-avocado ?ks-with-chopped-chicken ?target-container-2 avocado 1 piece)
                       '(seed ?seeded-avocado ?avocado-seed ?ks-with-seeded-avocado ?ks-with-avocado ?proportioned-avocado ?knife)
                       '(peel ?peeled-avocado ?avocado-peel ?ks-with-peeled-avocado ?ks-with-seeded-avocado ?seeded-avocado ?knife)
                       '(fetch-and-proportion ?proportioned-apple ?ks-with-apple ?ks-with-peeled-avocado ?target-container-3 apple 1 piece)
                       '(peel ?peeled-apple ?apple-peel ?ks-with-peeled-apple ?ks-with-apple ?proportioned-apple ?knife)
                       '(seed ?seeded-apple ?apple-seed ?ks-with-seeded-apple ?ks-with-peeled-apple ?peeled-apple ?knife)
                       '(cut ?chopped-apple ?ks-with-chopped-apple ?ks-with-seeded-apple ?seeded-apple chopped ?knife)
                       '(fetch-and-proportion ?proportioned-celery ?ks-with-celery ?ks-with-chopped-apple ?target-container-4 celery 25 g)
                       '(cut ?chopped-celery ?ks-with-chopped-celery ?ks-with-celery ?proportioned-celery finely-chopped ?knife)
                       '(fetch-and-proportion ?proportioned-onion ?ks-with-onion ?ks-with-chopped-celery ?target-container-5 red-onion 15 g)
                       '(cut ?chopped-onion ?ks-with-chopped-onion ?ks-with-onion ?proportioned-onion finely-chopped ?knife)
                       '(fetch-and-proportion ?proportioned-cilantro ?ks-with-cilantro ?ks-with-chopped-onion ?target-container-6 fresh-cilantro 2 tablespoon)
                       '(cut ?chopped-cilantro ?ks-with-chopped-cilantro ?ks-with-cilantro ?proportioned-cilantro finely-chopped ?knife)
                       '(fetch-and-proportion ?proportioned-lime-juice ?ks-with-lime-juice ?ks-with-chopped-cilantro ?target-container-7 lime-juice 2 teaspoon)
                       '(fetch-and-proportion ?proportioned-salt ?ks-with-salt ?ks-with-lime-juice ?target-container-9 coarse-salt 1/2 teaspoon)
                       '(fetch-and-proportion ?proportioned-pepper ?ks-with-pepper ?ks-with-salt ?target-container-10 ground-black-pepper 1/4 teaspoon)
                       '(fetch ?fork ?ks-with-fetched-fork ?ks-with-pepper fork 1)
                       '(mash ?mashed-avocado ?ks-with-mashed-avocado ?ks-with-fetched-fork ?peeled-avocado ?fork)
                       '(fetch ?medium-bowl ?ks-with-fetched-medium-bowl ?ks-with-mashed-avocado medium-bowl 1)
                       '(transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-fetched-medium-bowl ?medium-bowl ?chopped-chicken ?quantity-a ?unit-a)
                       '(transfer-contents ?output-container-b ?rest-b ?output-ks-b ?output-ks-a ?output-container-a ?mashed-avocado ?quantity-b ?unit-b)
                       '(transfer-contents ?output-container-c ?rest-c ?output-ks-c ?output-ks-b ?output-container-b ?chopped-apple ?quantity-c ?unit-c)
                       '(transfer-contents ?output-container-d ?rest-d ?output-ks-d ?output-ks-c ?output-container-c ?chopped-celery ?quantity-d ?unit-d)
                       '(transfer-contents ?output-container-e ?rest-e ?output-ks-e ?output-ks-d ?output-container-d ?chopped-onion ?quantity-e ?unit-e)
                       '(mingle ?salad ?ks-with-salad ?output-ks-e ?output-container-e ?fork)
                       '(transfer-contents ?output-container-f ?rest-f ?output-ks-f ?ks-with-salad ?salad ?chopped-cilantro ?quantity-f ?unit-f)
                       '(transfer-contents ?output-container-g ?rest-g ?output-ks-g ?output-ks-f ?output-container-f ?proportioned-lime-juice ?quantity-g ?unit-g)
                       '(transfer-contents ?output-container-h ?rest-h ?output-ks-h ?output-ks-g ?output-container-g ?proportioned-salt ?quantity-h ?unit-h)
                       '(transfer-contents ?output-container-i ?rest-i ?output-ks-i ?output-ks-h ?output-container-h ?proportioned-pepper ?quantity-i ?unit-i)
                       '(fetch-and-proportion ?proportioned-olive-oil ?ks-with-olive-oil ?output-ks-i ?target-container-12 olive-oil 1 teaspoon)
                       '(transfer-contents ?output-container-j ?rest-j ?output-ks-j ?ks-with-olive-oil ?output-container-i ?proportioned-olive-oil ?quantity-j ?unit-j)
                       '(mingle ?seasoned-salad ?ks-with-seasoned-salad ?output-ks-j ?output-container-j ?fork)
                       '(fetch ?large-bowl ?ks-with-fetched-large-bowl ?ks-with-seasoned-salad large-bowl 1)
                       '(transfer-contents ?output-container-k ?rest-k ?output-ks-k ?ks-with-fetched-large-bowl ?large-bowl ?seasoned-salad ?quantity-k ?unit-k)
                       '(fetch ?plastic-wrap ?ks-with-fetched-plastic-wrap ?output-ks-k plastic-wrap 1)
                       '(cover ?covered-salad ?ks-with-covered-salad ?ks-with-fetched-plastic-wrap ?output-container-k ?plastic-wrap)
                       '(refrigerate ?cooled-salad ?ks-with-cooled-salad ?ks-with-covered-salad ?covered-salad ?fridge ?cooling-quantity ?cooling-unit))
                 :primary-output-var
                 '?cooled-salad))

(defparameter *basic-chicken-salad-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'basic-chicken-salad
                 :kitchen-state
                 *full-kitchen*
                 :meaning-network
                 (list '(get-kitchen ?kitchen)
                       '(fetch-and-proportion ?proportioned-almonds ?ks-with-almonds ?kitchen ?target-container-1 almond 140 g)
                       '(fetch-and-proportion ?proportioned-mayo ?ks-with-mayo ?ks-with-almonds ?target-container-2 mayonnaise 230 g)
                       '(fetch-and-proportion ?proportioned-lemon-juice ?ks-with-lemon-juice ?ks-with-mayo ?target-container-3 lemon-juice 1 tablespoon)
                       '(fetch-and-proportion ?proportioned-pepper ?ks-with-pepper ?ks-with-lemon-juice ?target-container-4 ground-black-pepper 0.25 teaspoon)
                       '(fetch-and-proportion ?proportioned-chicken ?ks-with-chicken ?ks-with-pepper ?target-container-5 cooked-chicken 150 g)
                       '(cut ?chopped-chicken ?ks-with-chopped-chicken ?ks-with-chicken ?proportioned-chicken chopped ?knife)
                       '(fetch-and-proportion ?proportioned-celery ?ks-with-celery ?ks-with-chopped-chicken ?target-container-6 celery 1 piece)
                       '(cut ?chopped-celery ?ks-with-chopped-celery ?ks-with-celery ?proportioned-celery chopped ?knife)
                       '(fetch ?frying-pan ?ks-with-frying-pan ?ks-with-chopped-celery frying-pan 1)
                       '(grease ?greased-pan ?ks-with-greased-pan ?ks-with-frying-pan ?frying-pan ?grease)
                       '(transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-greased-pan ?frying-pan ?proportioned-almonds ?quantity-a ?unit-a)
                       '(fry ?fried-almonds ?ks-with-fried-almonds ?output-ks-a ?output-container-a ?stove medium-high-heat ?frying-time-qty ?frying-time-unit)
                       '(fetch ?medium-bowl ?ks-with-medium-bowl ?ks-with-fried-almonds medium-bowl 1)
                       '(transfer-contents ?output-container-b ?rest-b ?output-ks-b ?ks-with-medium-bowl ?medium-bowl ?proportioned-mayo ?quantity-b ?unit-b)
                       '(transfer-contents ?output-container-c ?rest-c ?output-ks-c ?output-ks-b ?output-container-b ?proportioned-lemon-juice ?quantity-c ?unit-c)
                       '(transfer-contents ?output-container-d ?rest-d ?output-ks-d ?output-ks-c ?output-container-c ?proportioned-pepper ?quantity-d ?unit-d)
                       '(mix ?sauce-mixture ?ks-with-sauce-mixture ?output-ks-d ?output-container-d ?mixing-tool)
                       '(transfer-contents ?output-container-e ?rest-e ?output-ks-e ?ks-with-sauce-mixture ?sauce-mixture ?chopped-chicken ?quantity-e ?unit-e)
                       '(transfer-contents ?output-container-f ?rest-f ?output-ks-f ?output-ks-e ?output-container-e ?fried-almonds ?quantity-f ?unit-f)
                       '(transfer-contents ?output-container-g ?rest-g ?output-ks-g ?output-ks-f ?output-container-f ?proportioned-celery ?quantity-g ?unit-g)
                       '(mingle ?basic-chicken-salad ?ks-with-basic-chicken-salad ?output-ks-g ?output-container-g ?wooden-spoon))
                 :primary-output-var
                 '?basic-chicken-salad))

(defparameter *broccoli-salad-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'broccoli-salad
                 :kitchen-state
                 *full-kitchen*
                 :meaning-network
                 (list '(get-kitchen ?kitchen)
                       '(fetch-and-proportion ?proportioned-broccoli ?ks-with-broccoli ?kitchen ?target-container-1 broccoli 1 piece)
                       '(fetch-and-proportion ?proportioned-onion ?ks-with-onion ?ks-with-broccoli ?target-container-2 red-onion 50 g)
                       '(cut ?chopped-onion ?ks-with-chopped-onion ?ks-with-onion ?proportioned-onion chopped ?knife)
                       '(fetch-and-proportion ?proportioned-bacon ?ks-with-bacon ?ks-with-chopped-onion ?target-container-3 cooked-bacon 450 g)
                       '(fetch-and-proportion ?proportioned-vinegar ?ks-with-vinegar ?ks-with-bacon ?target-container-4 cider-vinegar 2.5 tablespoon)
                       '(fetch-and-proportion ?proportioned-mayo ?ks-with-mayo ?ks-with-vinegar ?target-container-5 mayonnaise 230 g)
                       '(fetch-and-proportion ?proportioned-sugar ?ks-with-sugar ?ks-with-mayo ?target-container-6 white-sugar 70 g)
                       '(fetch-and-proportion ?proportioned-cheese ?ks-with-grated-cheese ?ks-with-sugar ?target-container-7 grated-mozzarella 170 g)
                       '(cut ?chopped-bacon ?ks-with-chopped-bacon ?ks-with-grated-cheese ?proportioned-bacon chopped ?knife)
                       '(cut ?chopped-broccoli ?ks-with-chopped-broccoli ?ks-with-chopped-bacon ?proportioned-broccoli chopped ?knife)
                       '(fetch ?large-bowl-1 ?ks-with-large-bowl-1 ?ks-with-chopped-broccoli large-bowl 1)
                       '(transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-large-bowl-1 ?large-bowl-1 ?chopped-broccoli ?quantity-a ?unit-a)
                       '(transfer-contents ?output-container-b ?rest-b ?output-ks-b ?output-ks-a ?output-container-a ?chopped-onion ?quantity-b ?unit-b)
                       '(transfer-contents ?output-container-c ?rest-c ?output-ks-c ?output-ks-b ?output-container-b ?proportioned-cheese ?quantity-c ?unit-c)
                       '(mingle ?broccoli-mixture ?ks-with-broccoli-mixture ?output-ks-c ?output-container-c ?mingling-tool)
                       '(fetch ?large-bowl-2 ?ks-with-large-bowl-2 ?ks-with-broccoli-mixture large-bowl 1)
                       '(transfer-contents ?output-container-d ?rest-d ?output-ks-d ?ks-with-large-bowl-2 ?large-bowl-2 ?proportioned-vinegar ?quantity-d ?unit-d)
                       '(transfer-contents ?output-container-e ?rest-e ?output-ks-e ?output-ks-d ?output-container-d ?proportioned-sugar ?quantity-e ?unit-e)
                       '(transfer-contents ?output-container-f ?rest-f ?output-ks-f ?output-ks-e ?output-container-e ?proportioned-mayo ?quantity-f ?unit-f)
                       '(mix ?dressing ?ks-with-dressing ?output-ks-f ?output-container-f ?mixing-tool)
                       '(transfer-contents ?output-container-g ?rest-g ?output-ks-g ?ks-with-dressing ?broccoli-mixture ?dressing ?quantity-g ?unit-g)
                       '(mingle ?broccoli-salad ?ks-with-broccoli-salad ?output-ks-g ?output-container-g ?mingling-tool)
                       '(refrigerate ?cooled-salad ?ks-with-cooled-salad ?ks-with-broccoli-salad ?broccoli-salad ?fridge 24 hour))
                 :primary-output-var
                 '?proportioned-broccoli))

(defparameter *croutons-vinegar-salad-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'croutons-vinegar-salad
                 :kitchen-state
                 *full-kitchen*
                 :meaning-network
                 (list '(get-kitchen ?kitchen)
                       '(fetch-and-proportion ?proportioned-bread ?ks-with-bread ?kitchen ?target-container-1 white-bread-slice 1 piece)
                       '(fetch-and-proportion ?proportioned-olive-oil-tbsp ?ks-with-olive-oil-tbsp ?ks-with-bread ?target-container-2 extra-virgin-olive-oil 2 tablespoon)
                       '(fetch-and-proportion ?proportioned-dill ?ks-with-dried-dill ?ks-with-olive-oil-tbsp ?target-container-3 dried-dill-weed 0.25 teaspoon)
                       '(fetch-and-proportion ?proportioned-garlic ?ks-with-garlic ?ks-with-dried-dill ?target-container-4 garlic-powder 0.25 teaspoon)
                       '(fetch-and-proportion ?proportioned-lettuce ?ks-with-lettuce ?ks-with-garlic ?target-container-6 romaine-lettuce 225 g)
                       '(fetch-and-proportion ?proportioned-greens ?ks-with-greens ?ks-with-lettuce ?target-container-7 mixed-greens 115 g)
                       '(fetch-and-proportion ?proportioned-cherry-tomato ?ks-with-cherry-tomato ?ks-with-greens ?target-container-8 cherry-tomato 115 g)
                       '(cut ?sliced-tomato ?ks-with-sliced-tomato ?ks-with-cherry-tomato ?proportioned-cherry-tomato slices ?knife)
                       '(fetch-and-proportion ?proportioned-radish ?ks-with-radish ?ks-with-sliced-tomato ?target-container-9 radish 5 piece)
                       '(cut ?sliced-radish ?ks-with-sliced-radish ?ks-with-radish ?proportioned-radish slices ?knife)
                       '(fetch-and-proportion ?proportioned-onion ?ks-with-onion ?ks-with-sliced-radish ?target-container-10 red-onion 1 piece)
                       '(cut ?sliced-onion ?ks-with-sliced-onion ?ks-with-onion ?proportioned-onion fine-slices ?knife)
                       '(fetch-and-proportion ?proportioned-carrot ?ks-with-carrot ?ks-with-sliced-onion ?target-container-11 carrot 1 piece)
                       '(cut ?shredded-carrot ?ks-with-shredded-carrot ?ks-with-carrot ?proportioned-carrot shredded ?knife)
                       '(fetch-and-proportion ?proportioned-olive-oil-ml ?ks-with-olive-oil-ml ?ks-with-shredded-carrot ?target-container-14 extra-virgin-olive-oil 250 ml)
                       '(fetch-and-proportion ?proportioned-vinegar ?ks-with-vinegar ?ks-with-olive-oil-ml ?target-container-15 red-wine-vinegar 6 tablespoon)
                       '(fetch-and-proportion ?proportioned-sugar ?ks-with-sugar ?ks-with-vinegar ?target-container-16 white-sugar 1.5 teaspoon)
                       '(preheat-oven ?heated-oven ?ks-with-heated-oven ?ks-with-sugar ?oven 190 degrees-celsius)
                       '(cut ?bread-cubes ?ks-with-bread-cubes ?ks-with-heated-oven ?proportioned-bread two-cm-cubes ?knife)
                       '(fetch ?large-bowl-1 ?ks-with-large-bowl-1 ?ks-with-bread-cubes large-bowl 1)
                       '(transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-large-bowl-1 ?large-bowl-1 ?bread-cubes ?quantity-a ?unit-a)
                       '(transfer-contents ?output-container-b ?rest-b ?output-ks-b ?output-ks-a ?output-container-a ?proportioned-olive-oil-tbsp ?quantity-b ?unit-b)
                       '(sprinkle ?bread-with-sprinkled-dill ?ks-with-sprinkled-dill ?output-ks-b ?output-container-b ?proportioned-dill)
                       '(mingle ?sprinkled-bread ?ks-with-sprinkled-bread ?ks-with-sprinkled-dill ?bread-with-sprinkled-dill ?mingling-tool)
                       '(fetch ?baking-tray ?ks-with-baking-tray ?ks-with-sprinkled-bread baking-tray 1)
                       '(transfer-contents ?output-container-d ?rest-d ?output-ks-d ?ks-with-baking-tray ?baking-tray ?sprinkled-bread ?quantity-d ?unit-d)
                       '(sprinkle ?bread-with-garlic ?ks-with-sprinkled-garlic ?output-ks-d ?output-container-d ?proportioned-garlic)
                       '(bake ?baked-croutons ?ks-with-baked-croutons ?ks-with-sprinkled-garlic ?bread-with-garlic ?oven 12 minute ?baking-temp-qty ?baking-temp-unit)
                       '(fetch ?large-bowl-2 ?ks-with-large-bowl-2 ?ks-with-baked-croutons large-bowl 1)
                       '(transfer-contents ?output-container-e ?rest-e ?output-ks-e ?ks-with-large-bowl-2 ?large-bowl-2 ?proportioned-lettuce ?quantity-e ?unit-e)
                       '(transfer-contents ?output-container-f ?rest-f ?output-ks-f ?output-ks-e ?output-container-e ?proportioned-greens ?quantity-f ?unit-f)
                       '(transfer-contents ?output-container-g ?rest-g ?output-ks-g ?output-ks-f ?output-container-f ?sliced-tomato ?quantity-g ?unit-g)
                       '(transfer-contents ?output-container-h ?rest-h ?output-ks-h ?output-ks-g ?output-container-g ?sliced-radish ?quantity-h ?unit-h)
                       '(transfer-contents ?output-container-i ?rest-i ?output-ks-i ?output-ks-h ?output-container-h ?sliced-onion ?quantity-i ?unit-i)
                       '(transfer-contents ?output-container-j ?rest-j ?output-ks-j ?output-ks-i ?output-container-i ?shredded-carrot ?quantity-j ?unit-j)
                       '(mingle ?veggies-mix ?ks-with-veggies-mix ?output-ks-j ?output-container-j ?mingling-tool)
                       '(cover ?covered-salad ?ks-with-covered-salad ?ks-with-veggies-mix ?veggies-mix ?plastic-wrap)
                       '(refrigerate ?cooled-salad ?ks-with-cooled-salad ?ks-with-covered-salad ?covered-salad ?fridge ?cooling-quantity ?cooling-unit)
                       '(fetch ?jar ?ks-with-fetched-jar ?ks-with-cooled-salad jar 1)
                       '(fetch ?jar-lid ?ks-with-fetched-jar-lid ?ks-with-fetched-jar jar-lid 1)
                       '(transfer-contents ?output-container-m ?rest-m ?output-ks-m ?ks-with-fetched-jar-lid ?jar ?proportioned-olive-oil-ml ?quantity-m ?unit-m)
                       '(transfer-contents ?output-container-n ?rest-n ?output-ks-n ?output-ks-m ?output-container-m ?proportioned-vinegar ?quantity-n ?unit-n)
                       '(transfer-contents ?output-container-o ?rest-o ?output-ks-o ?output-ks-n ?output-container-n ?proportioned-sugar ?quantity-o ?unit-o)
                       '(cover ?covered-jar ?ks-with-covered-jar ?output-ks-o ?output-container-o ?jar-lid)
                       '(shake ?salad-dressing ?ks-with-dressing ?ks-with-covered-jar ?covered-jar)
                       '(refrigerate ?cooled-dressing ?ks-with-cooled-dressing ?ks-with-dressing ?salad-dressing ?fridge-2 ?cooling-quantity-2 ?cooling-unit-2)
                       '(uncover ?uncovered-salad ?used-plastic-wrap ?ks-with-uncovered-salad ?ks-with-cooled-dressing ?cooled-salad)
                       '(uncover ?uncovered-jar ?used-jar-lid ?ks-with-uncovered-jar ?ks-with-uncovered-salad ?covered-jar)
                       '(transfer-contents ?output-container-r ?rest-r ?output-ks-r ?ks-with-uncovered-jar ?uncovered-salad ?uncovered-jar ?quantity-r ?unit-r)
                       '(mingle ?dressed-salad ?ks-with-dressed-salad ?output-ks-r ?output-container-r ?mingling-tool)
                       '(sprinkle ?croutons-vinegar-salad ?ks-with-croutons-vinegar-salad ?ks-with-dressed-salad ?dressed-salad ?baked-croutons))
                 :primary-output-var
                 '?croutons-vinegar-salad))

; list of all available simulation environments
(defparameter *simulation-environments*
  (list *almond-crescent-cookies-environment*
        *afghan-biscuits-environment*
        *best-brownies-environment*
        *chocolate-fudge-cookies-environment*
        *easy-banana-bread-environment*
        *easy-oatmeal-cookies-environment*
        *whole-wheat-ginger-snaps-environment*
        *cucumber-slices-with-dill-environment*
        *easy-cherry-tomato-corn-salad-environment*
        *black-bean-and-sweet-potato-salad-environment*
        ; extra almond crescent cookies
        *almond-crescent-cookies-2-environment*
        *almond-crescent-cookies-3-environment*
        *almond-crescent-cookies-4-environment*
        *almond-crescent-cookies-5-environment*
        ; extra baking recipes
        *coconut-tuiles-environment*
        *mexican-wedding-cookies-environment*
        *bisquick-shortcake-biscuits-environment*
        *chocolate-cream-cheese-cupcakes-environment*
        ; extra black bean salad recipes
        *black-bean-salad-2-environment*
        *black-bean-salad-3-environment*
        *black-bean-salad-4-environment*
        *black-bean-salad-5-environment*
        ; extra salad recipes
        *classic-greek-salad-environment*
        *classic-potato-salad-environment*
        *cole-slaw-environment*
        *cranberry-fluff-salad-environment*
        *avocado-chicken-salad-environment*
        *basic-chicken-salad-environment*
        *broccoli-salad-environment*
        *croutons-vinegar-salad-environment*
        ))
|#