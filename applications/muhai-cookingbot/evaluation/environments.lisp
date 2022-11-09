(in-package :muhai-cookingbot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simulation Environments ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass simulation-environment ()
  ((recipe-id :type symbol :initarg :recipe-id :accessor recipe-id :initform nil)
   (kitchen-state :type kitchen-state :initarg :kitchen-state :accessor kitchen-state)
   (meaning-network :type list :initarg :meaning-network :accessor meaning-network :initform '())
   (final-node :type irl-program-processor-node :accessor final-node)
   (primary-output-var :type symbol :initarg :primary-output-var :accessor primary-output-var :initform nil)
   (output-node :type irl-program-processor-node :accessor output-node))
  (:documentation "Class wrapping all information for setting up and evaluating an environment."))

(defmethod initialize-instance :after ((simulation-environment simulation-environment) &key)
  "Execute the simulation environment's network once and already store the solution (to prevent multiple re-executions)."
  (when (meaning-network simulation-environment)
    (let ((extended-mn (append-meaning-and-irl-bindings (meaning-network simulation-environment) nil)))
      (init-kitchen-state simulation-environment)
      (multiple-value-bind (bindings nodes) (evaluate-irl-program extended-mn nil)
         ; we only expect there to be one solution
        (setf (final-node simulation-environment) (first nodes)))))
  (when (and (final-node simulation-environment) (primary-output-var simulation-environment)) 
    (let ((node (final-node simulation-environment))
          (var-to-find (primary-output-var simulation-environment)))
      (loop for output-var = (second (irl::primitive-under-evaluation node))
            when (eql output-var var-to-find)
              do (setf (output-node simulation-environment) node)
            (setf node (parent node))
            while (and node (not (output-node simulation-environment)))))))

(defmethod init-kitchen-state ((simulation-environment simulation-environment))
  "Set initial kitchen state to be used in simulation to the one of the given environment."
  (setf *initial-kitchen-state* (kitchen-state simulation-environment)))

(defparameter *almond-crescent-cookies-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'almond-crescent-cookies
                 :kitchen-state
                 (make-instance
                  'kitchen-state
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
                                                                      :used T
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
                                                  (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)
                                                  (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                                  (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                                  (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)

                                                  ;; tools
                                                  (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                                  (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                                  (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)

                                                  ;; baking equipment
                                                  (make-instance 'baking-tray)
                                                  (make-instance 'baking-paper)))))
                 :meaning-network
                 (list '(get-kitchen ?kitchen-state-4)
                       '(bring-to-temperature ?ingredient-at-room-temperature-7 ?output-kitchen-state-47 ?kitchen-state-out-34 ?ingredient-out-6 18 degrees-celsius)
                       '(fetch-and-proportion ?ingredient-out-6 ?kitchen-state-out-34 ?kitchen-state-4 ?target-container-6 butter 226 g)
                       '(fetch-and-proportion ?ingredient-out-13 ?kitchen-state-out-77 ?output-kitchen-state-47 ?target-container-13 white-sugar 116 g)
                       '(fetch-and-proportion ?ingredient-out-20 ?kitchen-state-out-117 ?kitchen-state-out-77 ?target-container-20 vanilla-extract 4 g)
                       '(fetch-and-proportion ?ingredient-out-27 ?kitchen-state-out-158 ?kitchen-state-out-117 ?target-container-27 almond-extract 4 g)
                       '(fetch-and-proportion ?ingredient-out-33 ?kitchen-state-out-193 ?kitchen-state-out-158 ?target-container-33 all-purpose-flour 340 g)
                       '(fetch-and-proportion ?ingredient-out-39 ?kitchen-state-out-230 ?kitchen-state-out-193 ?target-container-39 almond-flour 112 g)
                       '(fetch-and-proportion ?ingredient-out-45 ?kitchen-state-out-267 ?kitchen-state-out-230 ?target-container-45 powdered-white-sugar 29 g)
                       '(beat ?output-container-298 ?output-kitchen-state-347 ?input-kitchen-state-298 ?input-container-249 ?tool-150)
                       '(transfer-contents ?input-container-249 ?rest-y-106 ?input-kitchen-state-298 ?output-kitchen-state-x-53 ?output-container-?x-53 ?ingredient-out-13 ?quantity-y-106 ?unit-y-106)
                       '(transfer-contents ?output-container-?x-53 ?rest-x-106 ?output-kitchen-state-x-53 ?kitchen-state-out-267 ?empty-container-53 ?ingredient-at-room-temperature-7 ?quantity-x-106 ?unit-x-106)
                       '(mix ?output-container-338 ?output-kitchen-state-394 ?input-kitchen-state-338 ?input-container-282 ?tool-169)
                       '(transfer-contents ?input-container-282 ?rest-y-208 ?input-kitchen-state-338 ?intermediate-ks-104 ?output-container-after-adding-x-104 ?ingredient-out-20 ?quantity-y-208 ?unit-y-208)
                       '(transfer-contents ?output-container-after-adding-x-104 ?rest-x-208 ?intermediate-ks-104 ?output-kitchen-state-347 ?output-container-298 ?ingredient-out-27 ?quantity-x-208 ?unit-x-208)
                       '(transfer-contents ?output-container-686 ?rest-y-229 ?output-kitchen-state-800 ?intermediate-ks-115 ?output-container-after-adding-x-115 ?ingredient-out-33 ?quantity-y-229 ?unit-y-229)
                       '(transfer-contents ?output-container-after-adding-x-115 ?rest-x-229 ?intermediate-ks-115 ?output-kitchen-state-394 ?output-container-338 ?ingredient-out-39 ?quantity-x-229 ?unit-x-229)
                       '(mix ?output-container-699 ?output-kitchen-state-815 ?output-kitchen-state-800 ?output-container-686 ?tool-349)
                       '(shape ?shaped-bakeables-262 ?kitchen-state-out-786 ?kitchen-state-with-portions-on-tray-130 ?portioned-dough-130 ball-shape)
                       '(portion-and-arrange ?portioned-dough-130 ?kitchen-state-with-portions-on-tray-130 ?output-kitchen-state-815 ?output-container-699 25 g ?pattern-130 ?lined-baking-tray-259)
                       '(shape ?shaped-bakeables-264 ?kitchen-state-out-792 ?kitchen-state-out-786 ?shaped-bakeables-262 crescent-shape)
                       '(transfer-items ?things-placed-136 ?kitchen-out-136 ?kitchen-state-out-805 ?shaped-bakeables-264 ?lined-baking-tray-269)
                       '(line ?lined-baking-tray-269 ?kitchen-state-out-805 ?kitchen-state-out-792 baking-tray baking-paper)
                       '(bake ?thing-baked-148 ?kitchen-state-out-887 ?kitchen-out-136 ?things-placed-136 ?oven-to-bake-in-148 15 minute 175 degrees-celsius)
                       '(sprinkle ?sprinkled-object-151 ?kitchen-state-out-901 ?kitchen-state-out-887 ?thing-baked-148 ?ingredient-out-45))
                 :primary-output-var
                 '?sprinkled-object-151))

(defparameter *afghan-biscuits-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'afghan-biscuits
                 :kitchen-state  (make-instance 
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
                                                                                                                                                             :value 500)))))
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
                                                                       (make-instance 'wooden-spoon)
                                                                       (make-instance 'wooden-spoon)
                                                                       (make-instance 'table-spoon)
                                                                       (make-instance 'table-spoon)
                                                                       (make-instance 'table-spoon)
                                                                       (make-instance 'rolling-pin)
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
                                                                       (make-instance 'medium-bowl)
                                                                       (make-instance 'medium-bowl)
                                                                       (make-instance 'medium-bowl)
                                                                       (make-instance 'medium-bowl)
                                                                       (make-instance 'medium-bowl)
                                                                       (make-instance 'medium-bowl)
                                                                       (make-instance 'medium-bowl)
                                                                       (make-instance 'large-bowl)
                                                                       (make-instance 'large-bowl)
                                                                       (make-instance 'large-bowl)
                                                                       (make-instance 'large-bowl)
                                                                       (make-instance 'large-bowl)
                                                                       ))))
                 :meaning-network
                 (list '(get-kitchen ?kitchen-state)
                       '(fetch-and-proportion ?proportioned-butter ?kitchen-state-with-butter ?kitchen-state ?target-container-1 butter 200 g)
                       '(bring-to-temperature ?butter-at-room-temp ?kitchen-state-with-butter-at-room-temp ?kitchen-state-with-butter ?proportioned-butter 18 degrees-celsius)
                       '(fetch-and-proportion ?proportioned-caster-sugar ?kitchen-state-with-caster-sugar ?kitchen-state-with-butter ?target-container-2 caster-sugar 100 g)
                       '(fetch-and-proportion ?proportioned-all-purpose-flour ?kitchen-state-with-all-purpose-flour ?kitchen-state-with-caster-sugar ?target-container-3 all-purpose-flour 301 g)
                       '(fetch-and-proportion ?proportioned-cocoa-powder ?kitchen-state-with-cocoa-powder ?kitchen-state-with-all-purpose-flour ?target-container-4 cocoa-powder 38 g)
                       '(fetch-and-proportion ?proportioned-corn-flakes ?kitchen-state-with-corn-flakes ?kitchen-state-with-cocoa-powder ?target-container-5 corn-flakes 301 g)
                       '(fetch-and-proportion ?proportioned-icing-sugar ?kitchen-state-with-icing-sugar ?kitchen-state-with-corn-flakes ?target-container-6 icing-sugar 201 g)
                       '(fetch-and-proportion ?proportioned-icing-cocoa-powder ?kitchen-state-with-icing-cocoa-powder ?kitchen-state-with-icing-sugar ?target-container-7 cocoa-powder 26 g)
                       '(fetch-and-proportion ?proportioned-water ?kitchen-state-with-water ?kitchen-state-with-icing-cocoa-powder ?target-container-8 water 0.04436 l)
                       '(fetch-and-proportion ?proportioned-almonds ?kitchen-state-with-almonds ?kitchen-state-with-water ?target-container-9 almond-flakes 50 g)
                       '(preheat-oven ?preheated-oven ?kitchen-state-with-preheated-oven ?kitchen-state-with-almonds 175 degrees-celsius)
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
                       '(portion-and-arrange ?portioned-dough ?kitchen-state-with-portions-on-countertop ?kitchen-state-with-cornflakes-mixture ?flour-cocoa-butter-cornflakes-mixture 30 g ?arrangement-pattern ?countertop)
                       '(shape ?dough-balls ?kitchen-state-with-doughballs ?kitchen-state-with-portions-on-countertop ?portioned-dough ball-shape)
                       '(flatten ?flattened-dough-balls ?kitchen-state-with-flattened-doughballs ?kitchen-state-with-doughballs ?dough-balls ?rolling-pin)
                       '(transfer-items ?cookies-on-tray ?kitchen-state-with-cookies-on-tray ?kitchen-state-with-flattened-doughballs ?flattened-dough-balls ?lined-baking-tray)
                       '(bake ?baked-cookies ?kitchen-state-with-baking-cookies ?kitchen-state-with-cookies-on-tray ?cookies-on-tray ?preheated-oven 15 minute ?temp-quantity ?temp-unit)
                       '(fetch ?wire-rack ?kitchen-state-with-wire-rack ?kitchen-state-with-baking-cookies wire-rack 1)
                       '(transfer-items ?cookies-on-wire-rack ?kitchen-state-with-cookies-on-wire-rack ?kitchen-state-with-wire-rack ?baked-cookies ?wire-rack)
                       '(bring-to-temperature ?cooled-cookies ?kitchen-state-with-cooling-cookies ?kitchen-state-with-cookies-on-wire-rack ?cookies-on-wire-rack 18 degrees-celsius)
                       '(fetch ?medium-bowl ?kitchen-state-with-bowl ?kitchen-state-with-cooling-cookies medium-bowl 1)
                       '(transfer-contents ?container-for-icing-with-sugar ?rest-b ?kitchen-state-with-container-for-icing-with-sugar ?kitchen-state-with-bowl ?medium-bowl ?proportioned-icing-sugar ?quantity-b ?unit-b)
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
                 :kitchen-state (make-instance 'kitchen-state
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
                                                                                     :contents (list (make-instance 'vanilla-extract :amount
                                                                                                                    (make-instance 'amount
                                                                                                                                   :unit (make-instance 'g)
                                                                                                                                   :quantity (make-instance 'quantity
                                                                                                                                                            :value 100)))))
                                                                      (make-instance 'medium-bowl
                                                                                     :contents (list (make-instance 'chopped-walnut :amount
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
                                                                                     :contents (list (make-instance 'cocoa-powder :amount
                                                                                                                    (make-instance 'amount
                                                                                                                                   :unit (make-instance 'g)
                                                                                                                                   :quantity (make-instance 'quantity
                                                                                                                                                            :value 500)))))
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
                                                                 (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                                                 (make-instance 'spatula) (make-instance 'knife)

                                                                 ;; baking equipment
                                                                 (make-instance 'baking-tray)
                                                                 (make-instance 'pan)
                                                                 (make-instance 'baking-paper)))))
                                  :meaning-network
                                  (list '(get-kitchen ?kitchen-state)
                                        '(fetch-and-proportion ?proportioned-butter ?kitchen-state-with-butter ?kitchen-state ?new-container-1 butter 113 g)
                                        '(melt ?melted-butter ?kitchen-state-with-melted-butter ?kitchen-state-with-butter ?proportioned-butter)
                                        '(fetch-and-proportion ?proportioned-sugar ?kitchen-state-with-sugar ?kitchen-state-with-melted-butter ?new-container-2 white-sugar 201 g)
                                        '(fetch-and-proportion ?proportioned-eggs ?kitchen-state-with-eggs ?kitchen-state-with-sugar ?new-container-3 egg 2 piece)
                                        '(fetch-and-proportion ?proportioned-flour ?kitchen-state-with-flour ?kitchen-state-with-eggs ?new-container-4 all-purpose-flour 68 g)
                                        '(fetch-and-proportion ?proportioned-cocoa ?kitchen-state-with-cocoa ?kitchen-state-with-flour ?new-container-5 cocoa-powder 45 g)
                                        '(fetch-and-proportion ?proportioned-salt ?kitchen-state-with-salt ?kitchen-state-with-cocoa ?new-container-6 salt 1.5 g)
                                        '(fetch-and-proportion ?proportioned-vanilla ?kitchen-state-with-vanilla ?kitchen-state-with-salt ?new-container-7 vanilla-extract 4 g)
                                        '(fetch-and-proportion ?proportioned-walnuts ?kitchen-state-with-walnuts ?kitchen-state-with-vanilla ?new-container-8 chopped-walnut 50 g)
                                        '(preheat-oven ?preheated-oven ?kitchen-state-with-preheated-oven ?kitchen-state-with-walnuts 175 degrees-celsius)
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
                                        '(transfer-contents ?output-container-d ?rest-d ?output-kitchen-state-d ?output-kitchen-state-c ?output-container-c ?proportioned-walnuts ?quantity-d ?unit-d)
                                        '(mix ?dough ?kitchen-state-with-dough ?output-kitchen-state-d ?output-container-d ?beating-tool)
                                        '(spread ?pan-with-dough ?kitchen-state-with-dough-in-pan ?kitchen-state-with-dough ?floured-pan ?dough ?scraper)
                                        '(bake ?baked-brownie ?kitchen-state-with-baked-brownie ?kitchen-state-with-dough-in-pan ?pan-with-dough ?preheated-oven 25 minute ?temp-quantity ?temp-unit)
                                        '(bring-to-temperature ?cooled-brownie ?kitchen-state-with-cooled-brownie ?kitchen-state-with-baked-brownie ?baked-brownie 18 degrees-celsius)
                                        '(cut ?cut-brownie ?kitchen-state-with-cut-brownie ?kitchen-state-with-cooled-brownie ?cooled-brownie squares ?knife))
                                  :primary-output-var '?cut-brownie))

(defparameter *chocolate-fudge-cookies-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'chocolate-fudge-cookies
                 :kitchen-state (make-instance 'kitchen-state
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
                                                                                     :contents (list (make-instance 'vanilla-extract :amount
                                                                                                                    (make-instance 'amount
                                                                                                                                   :unit (make-instance 'g)
                                                                                                                                   :quantity (make-instance 'quantity
                                                                                                                                                            :value 100)))))
                                                                      (make-instance 'medium-bowl
                                                                                     :contents (list (make-instance 'chopped-walnut :amount
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
                                                                                     :contents (list (make-instance 'cocoa-powder :amount
                                                                                                                    (make-instance 'amount
                                                                                                                                   :unit (make-instance 'g)
                                                                                                                                   :quantity (make-instance 'quantity
                                                                                                                                                            :value 500)))))
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
                                                                 (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                                                 (make-instance 'spatula) (make-instance 'knife)

                                                                 ;; baking equipment
                                                                 (make-instance 'baking-tray)
                                                                 (make-instance 'pan)
                                                                 (make-instance 'baking-paper)))))
                                  :meaning-network
                                  (list '(get-kitchen ?kitchen-state)
                                        '(fetch-and-proportion ?proportioned-butter ?kitchen-state-with-butter ?kitchen-state ?new-container-1 butter 113 g)
                                        '(melt ?melted-butter ?kitchen-state-with-melted-butter ?kitchen-state-with-butter ?proportioned-butter)
                                        '(fetch-and-proportion ?proportioned-sugar ?kitchen-state-with-sugar ?kitchen-state-with-melted-butter ?new-container-2 white-sugar 201 g)
                                        '(fetch-and-proportion ?proportioned-eggs ?kitchen-state-with-eggs ?kitchen-state-with-sugar ?new-container-3 egg 2 piece)
                                        '(fetch-and-proportion ?proportioned-flour ?kitchen-state-with-flour ?kitchen-state-with-eggs ?new-container-4 all-purpose-flour 68 g)
                                        '(fetch-and-proportion ?proportioned-cocoa ?kitchen-state-with-cocoa ?kitchen-state-with-flour ?new-container-5 cocoa-powder 45 g)
                                        '(fetch-and-proportion ?proportioned-salt ?kitchen-state-with-salt ?kitchen-state-with-cocoa ?new-container-6 salt 1.5 g)
                                        '(fetch-and-proportion ?proportioned-vanilla ?kitchen-state-with-vanilla ?kitchen-state-with-salt ?new-container-7 vanilla-extract 4 g)
                                        '(fetch-and-proportion ?proportioned-walnuts ?kitchen-state-with-walnuts ?kitchen-state-with-vanilla ?new-container-8 chopped-walnut 50 g)
                                        '(preheat-oven ?preheated-oven ?kitchen-state-with-preheated-oven ?kitchen-state-with-walnuts 175 degrees-celsius)
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
                                        '(transfer-contents ?output-container-d ?rest-d ?output-kitchen-state-d ?output-kitchen-state-c ?output-container-c ?proportioned-walnuts ?quantity-d ?unit-d)
                                        '(mix ?dough ?kitchen-state-with-dough ?output-kitchen-state-d ?output-container-d ?beating-tool)
                                        '(spread ?pan-with-dough ?kitchen-state-with-dough-in-pan ?kitchen-state-with-dough ?floured-pan ?dough ?scraper)
                                        '(bake ?baked-brownie ?kitchen-state-with-baked-brownie ?kitchen-state-with-dough-in-pan ?pan-with-dough ?preheated-oven 25 minute ?temp-quantity ?temp-unit)
                                        '(bring-to-temperature ?cooled-brownie ?kitchen-state-with-cooled-brownie ?kitchen-state-with-baked-brownie ?baked-brownie 18 degrees-celsius)
                                        '(cut ?cut-brownie ?kitchen-state-with-cut-brownie ?kitchen-state-with-cooled-brownie ?cooled-brownie squares ?knife))
                                  :primary-output-var '?cut-brownie))

; list of all available simulation environments
(defparameter *simulation-environments*
  (list *almond-crescent-cookies-environment*
        *afghan-biscuits-environment*
        *best-brownies-environment*))