(ql:quickload :muhai-cookingbot)

(in-package :muhai-cookingbot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IRL NODE HELPER FUNCTIONS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-predicate-name ((irl-node irl::irl-program-processor-node))
  "Get the predicate name belonging to this node's primitive under evaluation."
  (first (irl::primitive-under-evaluation irl-node)))

(defmethod get-output-binding ((irl-node irl::irl-program-processor-node))
  "Get the binding belonging to the primary output of this node's primitive under evaluation."
  (let ((target-var (second (irl::primitive-under-evaluation irl-node)))
        (list-of-bindings (irl::bindings irl-node)))
    (find target-var list-of-bindings :key #'var)))  

(defmethod get-output-value ((irl-node irl::irl-program-processor-node))
  "Get the value belonging to the binding for the primary output of this node's primitive under evaluation."
  (let ((target-binding (get-output-binding irl-node)))
    (when target-binding
      (value target-binding))))

(defmethod get-output-kitchen-state ((irl-node irl::irl-program-processor-node))
  "Get the output kitchen state belonging to this node's primitive under evaluation."
  (let ((all-vars (irl::all-variables (list (irl::primitive-under-evaluation irl-node))))
        (list-of-bindings (irl::bindings irl-node)))
    (loop for var in all-vars
          for binding = (find var list-of-bindings :key #'var)
          if (eql (type-of (value binding)) 'kitchen-state)
            do (return (value binding)))))

(defmethod get-full-node-sequence ((irl-node irl::irl-program-processor-node))
  "Get the full sequence of nodes that led to the given IRL node.
   Sequence goes from the starting node with the first executed primitive up to and including the given IRL node."
  (let ((node irl-node)
        (node-seq '()))
    (loop do
         (push node node-seq)
         (setf node (parent node))
       while node)
    (rest node-seq)))

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

; list of all available simulation environments
(defparameter *simulation-environments*
  (list *almond-crescent-cookies-environment*
        *afghan-biscuits-environment*
        *best-brownies-environment*))

;;;;;;;;;;;;;;;;;;;;;;
;; Recipe Solutions ;;
;;;;;;;;;;;;;;;;;;;;;;

(defclass solution ()
  ((recipe-id :type symbol :initarg :recipe-id :accessor recipe-id :initform nil)
   (meaning-network :type list :initarg :meaning-network :accessor meaning-network :initform '())
   (subgoals-ratio :accessor subgoals-ratio :initform '())
   (dish-score :accessor dish-score :initform '())
   (time-ratio :accessor time-ratio :initform '()))
  (:documentation "Class used for storing a recipe solution and its score.")) 

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reading in Solutions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-solutions-file (filepath)
  "Read all recipe solutions from the file at the given filepath. 
   Solutions are represented by a line containing the ID of a recipe, i.e., #recipe_ID, 
   followed by a sequence of lines each containing a primitive operation from the meaning network for the aforementiond recipe."
  (with-open-file (stream filepath)
    (let ((solutions '()))
      (loop for line = (read-line stream nil)
            while line
            do
              (cond ((not (find #\space line :test-not #'eql))
                     (print "Empty line is skipped"))
                    ((char= (char line 0) #\#)
                     ; first check the previous solution
                     (when solutions
                       (multiple-value-bind (error-status messages) (check-recipe-program (meaning-network (first solutions)) nil *irl-primitives*)
                         (unless error-status
                           (error "Invalid IRL program in solution ~S. Error was thrown: ~a" (recipe-id (first solutions)) (format nil "~{~a~}" messages)))))
                       ; we are starting a new solution
                     (push (make-instance 'solution :recipe-id (read-from-string (subseq line 1))) solutions))
                     ; we are adding a new primitive operation to the current solution's meaning network
                    ((char= (char line 0) #\()
                     (let ((current-solution (first solutions)))
                       (unless current-solution
                         (error "The file should start with a Recipe ID (#recipe-id)!"))
                       (setf (meaning-network current-solution) (nconc (meaning-network current-solution) (list (read-from-string line))))))
                    (t
                     (error "A line should either contain a recipe ID (#recipe-id) or a primitive operation (op ?a ?b), but ~S was found" line))))
              (when solutions
                (multiple-value-bind (error-status messages) (check-recipe-program (meaning-network (first solutions)) nil *irl-primitives*)
                  (unless error-status
                    (error "Invalid IRL program in solution ~S. Error was thrown: ~a" (recipe-id (first solutions)) (format nil "~{~a~}" messages)))))
              solutions)))

(defun check-solutions-completeness (solutions &optional (sim-envs *simulation-environments*))
  "Check if the given solutions contain a solution for every recipe in the simulation environment."
  (loop for sim-env in sim-envs
        when (not (find (recipe-id sim-env) solutions :key #'(lambda (sol) (recipe-id sol))))
          do (error "Recipe ~S is missing in the solutions" (recipe-id sim-env)))
  (loop for solution in solutions
        when (not (find (recipe-id solution) sim-envs :key #'(lambda (sim-env) (recipe-id sim-env))))
          do (error "Solution contains recipe ~S which is currently unsupported" (recipe-id solution))
        when (> (count (recipe-id solution) solutions :key #'(lambda (sol) (recipe-id sol))) 1)
          do (error "Duplicate entry found for recipe ~S" (recipe-id solution))))

(defun check-recipe-program (irl-program ontology primitive-inventory)
  "Checks a recipe irl-program for mistakes.
   This function is based on the check-irl-program function from the IRL package,
   with unnecessary checks being removed and additional checks being added."
  (let ((variables (remove-duplicates
                     (find-all-anywhere-if #'variable-p irl-program)))
        (messages '()))
    
    ;; first check, everything should be a non-empty list
    (loop for expr in irl-program
          unless (and (listp expr) expr)
          do (push (format nil "The expression should be a non-empty list, got: ~a." expr) messages))

    ;; then check, the irl-program should contain exactly one get-kitchen    
    (let ((get-kitchen-count (count 'get-kitchen (mapcar #'first irl-program))))
      (unless (= get-kitchen-count 1)
        (push (format nil "The recipe should contain exactly one get-kitchen operation, but ~d were found" get-kitchen-count) messages)))
                                                         
    ;; lastly we check all primitives
    (loop for expr in irl-program
          for variables = (cdr expr)
          unless (= (length variables) (length (remove-duplicates variables)))
            do (push (format nil "In ~a variables appear at least twice as argument." expr) messages)
          ;; primitive must be found
          unless (find-primitive (first expr) primitive-inventory)
            do (push (format nil "Primitive ~a is not defined " (car expr)) messages)
          do
          (let ((primitive (find-primitive (first expr) primitive-inventory)))            
            ;; check that the number of variables matches the
            ;; number of slot-specs:
            (unless (= (length (irl::slot-specs primitive))
                       (length variables))
              (push (format nil "Error while reading primitive expression~%  ~a.~
                             ~%The number of given variables does not match ~
                             the number of slots."
                             expr) messages)
            ;; check that the given parameters are proper variable identifiers:
            (loop for var in variables
                  unless (or (variable-p var) ; regular variable
                             (numberp var) ; quantity
                             (listp var) ; list-of-kitchen-entities
                             (find-class var)) ; concepts
                  do (push (format nil "Error while reading primitive expression ~a.~
                                    ~%Expected a variable identifier, number, list or an ontology class but got ~a."
                                    expr var) messages)))))
    ;; all test succeeded, return t
    (if messages
      (values nil messages)
      (values t messages))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Measuring Solution Correctness ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Helper Functions Related to Kitchen Entity Locations ;;

(defun similar-locations (location-1 location-2)
  "Check whether two location paths are similar, i.e. they contain the same sequence of object classes."
  (loop for place-1 in location-1
        for place-2 in location-2
        always (eql (type-of place-1) (type-of place-2))))

(defun find-location (object place)
  "Get the full path to the given object, starting from the given place."
    (cond ((loop for el in (contents place)
                 if (eql (persistent-id object) (persistent-id el))
                   do (return t))
           (loop for el in (contents place)
                 if (eql (persistent-id object) (persistent-id el))
                   do (return (list place))))
          (t
           (loop for el in (contents place)
               if (subtypep (type-of el) 'container)
               do (let ((location (find-location object el)))
                    (when location
                      (return (append (list place) location))))))))

;; Helper Functions to Check Kitchen Entity Equality ;;

(defun get-slotnames (item &optional ignore)
  "Get the names of all slots that are available on the given item, excluding slot names present in the ignore list."
  (let* ((classlots  (closer-mop:class-slots (class-of item)))
         (slotnames  (mapcar #'closer-mop:slot-definition-name classlots)))
    (remove-if #'(lambda (slotname) (member slotname ignore))
             slotnames)))

(defun similar-entities (entity-1 entity-2 &optional (ignore '(id persistent-id)))
  "Convenience function to check whether two kitchen entities are similar by checking equality without taking any IDs into account."
  (equal-ontology-objects entity-1 entity-2 ignore))

(defgeneric equal-ontology-objects (object-1 object-2 &optional ignore)
  (:documentation "Returns t if object-1 and object-2 are equal when comparing all slots, except the ones contained in the ignore list."))

(defmethod equal-ontology-objects ((object-1 symbol) (object-2 symbol) &optional ignore)
  (eql object-1 object-2))

(defmethod equal-ontology-objects ((object-1 number) (object-2 number) &optional ignore)
  (= object-1 object-2))

(defmethod equal-ontology-objects ((object-1 string) (object-2 string) &optional ignore)
  (equalp object-1 object-2))

(defmethod equal-ontology-objects ((object-1 list) (object-2 list) &optional ignore)
  (and (= (length object-1) (length object-2))
       (loop for el-1 in object-1
             for el-2 in object-2
             always (equal-ontology-objects el-1 el-2 ignore))))

(defmethod equal-ontology-objects (object-1 object-2 &optional ignore)
  (and (equal (class-of object-1) (class-of object-2))
       (let ((o1-slotnames (get-slotnames object-1 ignore))
             (o2-slotnames (get-slotnames object-2 ignore)))
       (loop for o1-slotname in o1-slotnames
             always (equal-ontology-objects
                     (slot-value object-1  o1-slotname)
                     (slot-value object-2  o1-slotname)
                     ignore)))))

;; Subgoal Evaluation (Goal Condition Testing) ;;

(defclass located-entity ()
  ((entity :type kitchen-entity :initarg :entity :accessor entity)
   (location :type list :initarg :location :accessor location))
  (:documentation "Wrapper class that wraps a kitchen entity with its location.")) 

(defmethod get-located-output-entity ((irl-node irl::irl-program-processor-node))
  "Get the value belonging to the binding for the primary output of this node's primitive under evaluation 
   and wrap it together with its location in output kitchen state."
  (let ((output (get-output-value irl-node)) 
        (ks (get-output-kitchen-state irl-node)))
    (make-instance 'located-entity :entity output :location (find-location output ks))))

(defmethod compute-subgoal-success-ratio ((sol-node irl::irl-program-processor-node) (gold-node irl::irl-program-processor-node))
  "Compute the ratio of the subgoals that have been reached to the total number of subgoals that are present.
   The given IRL nodes are expected to be the final subnodes of the IRL evaluation."
  (multiple-value-bind (goals-reached goals-failed) (evaluate-subgoals sol-node gold-node)
    (/ (length goals-reached) (+ (length goals-reached) (length goals-failed)))))

(defmethod evaluate-subgoals ((sol-node irl::irl-program-processor-node) (gold-node irl::irl-program-processor-node))
  "Get a list of the subgoals that have been reached and a list of the subgoals that have failed.
   A subgoal is defined as the primary output of the golden standard node and is reached if a similar primary output is present in a solution node."
  (let* ((gold-nodes (get-full-node-sequence gold-node))
         ; no need to check the initial kitchen-states as these will always be correct
         (filtered-gold-nodes (remove-if #'(lambda (node) (eql (get-predicate-name node) 'get-kitchen)) gold-nodes))
         (sol-nodes (get-full-node-sequence sol-node))
         (filtered-sol-nodes (remove-if #'(lambda (node) (eql (get-predicate-name node) 'get-kitchen)) sol-nodes))
         (gold-entities (mapcar #'get-located-output-entity filtered-gold-nodes))
         (sol-entities  (mapcar #'get-located-output-entity filtered-sol-nodes))
         (goals-reached '())
         (goals-failed '()))   
    (loop for gold-entity in gold-entities
          for sol-entity = (find-if #'(lambda (sol-entity)
                                        ; for a subgoal to be reached both the location and the entity itself should be similar 
                                        (and (similar-entities (entity gold-entity) (entity sol-entity))
                                             (similar-locations (location gold-entity) (location sol-entity))))
                                        sol-entities)
          if sol-entity
            do
              ; each subgoal can only be matched once (in case the same subgoal would be repeated multiple times)
              (setf sol-entities (remove sol-entity sol-entities))
              (push gold-entity goals-reached)
          else do (push gold-entity goals-failed))
    (values goals-reached goals-failed)))

;; Dish Score Computation ;;

; A wrapper class for ingredient that also contains a pointer to the mixture it belongs. This class is used in "dish unfolding".
; This pointer is not added to ingredient directly to prevent infinite loop issues in web visualization.
(defclass hierarchy-ingredient ()
  ((ingredient :type ingredient :initarg :ingredient :accessor ingredient)
   (part-of :initarg :part-of :accessor part-of :initform nil))
  (:documentation "Wrapper class that wraps an ingredient with the hierarchy of mixtures it belongs."))

(defmethod get-mixture-hierarchy ((hierarchy-ingredient hierarchy-ingredient))
  "Extract a list of mixtures that this ingredient is directly or indirectly used in (in the order from first to last mixture creation)."
  (let ((mixture (part-of hierarchy-ingredient))
        (mixtures '()))    
    (loop while mixture
            do
              (setf mixtures (append (list (ingredient mixture)) mixtures))
              (setf mixture (part-of mixture)))
    mixtures))

(defclass similarity-score ()
  ((points :type number :initarg :points :accessor points :initform 0)
   (max-points :type number :initarg :max-points :accessor max-points :initform 0))
  (:documentation "Similarity score of two dishes based on the number of awarded points and the maximum number of possible points."))

(defmethod add-points ((similarity-score similarity-score) (awarded-points number) (possible-points number))
  "Change the similarity score by incrementing points and maximum attainable points by the given numbers"
  (setf (points similarity-score) (+ (points similarity-score) awarded-points))
  (setf (max-points similarity-score) (+ (max-points similarity-score) possible-points)))

(defmethod add-similarity-score-to ((similarity-score similarity-score) (to-similarity-score similarity-score))
  "Increment a similarity score by adding another similarity score to it."
  (setf (points to-similarity-score) (+ (points similarity-score) (points to-similarity-score)))
  (setf (max-points to-similarity-score) (+ (max-points similarity-score) (max-points to-similarity-score))))

(defmethod compute-dish-score ((similarity-score similarity-score))
  "Compute the actual dish-score as the ratio of the awarded points to the maximum number of points that could be reached."
  (/ (points similarity-score) (max-points similarity-score)))

(defmethod unfold-mixture ((mixture-to-unfold hierarchy-ingredient))
  "Unfold the given mixture into a list of all the base ingredients that are contained in it."
  (let* ((inner-mixture (ingredient mixture-to-unfold))
         (comps (components inner-mixture))
         (unfolded-comps '()))
    (loop for comp in comps
          do
          (cond ((subtypep (type-of comp) 'mixture)
                 (let ((unfolded-sub-comps (unfold-mixture (make-instance 'hierarchy-ingredient
                                                                          :ingredient comp
                                                                          :part-of mixture-to-unfold))))
                   (setf unfolded-comps (append unfolded-comps unfolded-sub-comps))))
                ((subtypep (type-of comp) 'ingredient)
                 (setf unfolded-comps (append unfolded-comps (list (make-instance 'hierarchy-ingredient
                                                                                  :ingredient comp
                                                                                  :part-of mixture-to-unfold)))))
                (t (error "unsupported component of class ~a" (type-of comp)))))
    unfolded-comps))

(defmethod unfold-dish ((dish container))
  "Unfold the contents of the given container into a list of all the base ingredients that are contained in it."
  (let* ((dish-copy (copy-object dish))
         (items (contents dish-copy))
         (ref-item (copy-object (first items)))
         (unfolded-contents '()))
    ; TODO RD: misschien beter eerst unfolden en dan zien welke base ingredients hetzelfde lijken en die samennemen? -> Ja lijkt beter op lange termijn   
    ; if all items in the container are the same, then just consider it to be one big item (for easier comparison)
    (when (loop for item in items
                always (similar-entities ref-item item '(id persistent-id amount)))
       (let ((max-points-value (loop for item in items
                                for current-value = (value (quantity (amount (convert-to-g item))))
                                sum current-value)))
         (setf (amount ref-item) (make-instance 'amount
                                                :unit (make-instance 'g)
                                                :quantity (make-instance 'quantity :value max-points-value)))
         (setf (contents dish-copy) (list ref-item))))
    ; unfold every item that is in the dish     
    (loop for item in (contents dish-copy)
          do (cond ((subtypep (type-of item) 'mixture)
                    (setf unfolded-contents (append unfolded-contents (unfold-mixture (make-instance 'hierarchy-ingredient :ingredient item)))))
                   ((subtypep (type-of item) 'ingredient)
                    (setf unfolded-contents (append unfolded-contents (list (make-instance 'hierarchy-ingredient :ingredient item)))))))
    unfolded-contents))

(defmethod compare-hierarchy-ingredient ((sol-ingredient hierarchy-ingredient) (gold-ingredient hierarchy-ingredient))
  "Compute a similarity score for the given ingredients."
  (let* ((sol-ing (ingredient sol-ingredient))
         (sol-dish-slots (get-slotnames sol-ing '(id persistent-id)))
         (sol-mixtures (get-mixture-hierarchy sol-ingredient))
         (gold-ing (ingredient gold-ingredient))
         (gold-dish-slots (get-slotnames gold-ing '(id persistent-id)))
         (gold-mixtures (get-mixture-hierarchy gold-ingredient))
         (ingredient-similarity-score (make-instance 'similarity-score))
         (hierarchy-similarity-score (make-instance 'similarity-score)))
    ; class doesn't have to be checked, since this function will currently only be called for ingredients that are the same     
    ; each slot that is in common is worth a score of 1  
    (loop for slot in gold-dish-slots
          if (and (member slot sol-dish-slots)
                  (similar-entities (slot-value sol-ing slot)
                                    (slot-value gold-ing slot)))
            do (add-points ingredient-similarity-score 1 1)
          else
            do (add-points ingredient-similarity-score 0 1))
    ; check mixture hierarchy composition
    (loop for gold-mixture in gold-mixtures
          for sol-mixture in sol-mixtures
          do
            (let ((mixture-similarity-score (compare-mixture sol-mixture gold-mixture)))
              (setf (points hierarchy-similarity-score) (+ (points hierarchy-similarity-score) (points mixture-similarity-score)))
              (setf (max-points hierarchy-similarity-score) (+ (max-points hierarchy-similarity-score) (max-points mixture-similarity-score)))))
    ; check the ratio of the hierarchy that is correct and adjust the points that were awarded until now accordingly
    ; TODO RD: maybe compute the number of points that were actually mixed and adjust max-points           
    (let* ((difference (abs (- (length gold-mixtures) (length sol-mixtures))))
           (ratio (/ (length gold-mixtures) (+ (length gold-mixtures) difference))))
      (setf (points hierarchy-similarity-score) (* ratio (points hierarchy-similarity-score))))

    ; compute the final similarity-score for this ingredient, with ingredient composition being a bit more important than mixture hierarchy
    (make-instance 'similarity-score
                   :points (+ (* 0.6 (points ingredient-similarity-score))
                               (* 0.4 (points hierarchy-similarity-score)))
                   :max-points (+ (* 0.6 (max-points ingredient-similarity-score))
                             (* 0.4 (max-points hierarchy-similarity-score))))))

(defmethod compare-mixture ((sol-mixture mixture) (gold-mixture mixture))
  "Compute a similarity score for the given mixtures."
  (let ((sol-dish-slots (get-slotnames sol-mixture '(id persistent-id components)))
        (gold-dish-slots (get-slotnames gold-mixture '(id persistent-id components)))
        (mixture-similarity-score (make-instance 'similarity-score)))    
    ; each slot that is in common is worth a score of 1  
    (loop for slot in gold-dish-slots
          if (and (member slot sol-dish-slots)
                  (similar-entities (slot-value sol-mixture slot)
                                    (slot-value gold-mixture slot)))
            do (add-points mixture-similarity-score 1 1)
          else
            do (add-points mixture-similarity-score 0 1))
    ; check if it is the same type of mixture (heterogeneous or homogeneous),
    ; this is very important so it has a big effect on the final score
    (unless (eq (type-of sol-mixture) (type-of gold-mixture))
      (setf (score mixture-similarity-score) (/ (score mixture-similarity-score 2))))
    mixture-similarity-score))

(defmethod compare-node-dishes ((sol-dish irl::irl-program-processor-node) (gold-dish irl::irl-program-processor-node))
  "Compute a similarity score for the final dish that was made when reaching this node."
  (let* ((sol-value (get-output-value sol-dish))
         (sol-location (find-location sol-value (get-output-kitchen-state sol-dish)))
         (sol-dish-slots (get-slotnames sol-value '(persistent-id id contents))) ; all slots except contents
         (gold-value (get-output-value gold-dish))
         (gold-location (find-location gold-value (get-output-kitchen-state gold-dish)))
         (gold-dish-slots (get-slotnames gold-value '(persistent-id id contents))) ; all slots except contents
         (container-score (make-instance 'similarity-score))
         (contents-score (make-instance 'similarity-score)))
    ; check if this node actually return 
    (if (not (and (subtypep (type-of sol-value) 'container)
                    (contents sol-value)
                    (loop for item in (contents sol-value) always (subtypep (type-of item) 'ingredient))))
      (make-instance 'similarity-score
                     :points 0
                     :max-points 1)
      (progn
        ;; container specific scoring
        ; location of dish is worth a score of 1
        (if (similar-locations sol-location gold-location)
          (add-points container-score 1 1)
          (add-points container-score 0 1))
         ; type of container is worth a score of 1
        (if (eq (type-of sol-value) (type-of gold-value))
          (add-points container-score 1 1)
          (add-points container-score 0 1))
        ; each slot that is in common is worth a score of 1  
        (loop for slot in gold-dish-slots
              if (and (member slot sol-dish-slots)
                      (similar-entities (slot-value sol-value slot)
                                        (slot-value gold-value slot)))
                do (add-points container-score 1 1)
              else
                do (add-points container-score 0 1))
        ;; contents specific scoring
        ; number of portions in the dish is worth a score of 1
        ; (only 1 because right now an end dish will always be portions of the same mixture, so just one portioning operation might be missing)
        (if (= (length (contents sol-value)) (length (contents gold-value)))
          (add-points contents-score 1 1)
        (add-points contents-score 0 1))
        ; actual composition of the final contents
        (let ((unfolded-dish-sol (unfold-dish sol-value))
              (unfolded-dish-gold (unfold-dish gold-value))
              (missing-ingredients '()))
          (loop for unfolded-ing-gold in unfolded-dish-gold
                for matching-ings-sol = (find-all (type-of (ingredient unfolded-ing-gold)) unfolded-dish-sol :key #'(lambda (sim-ing) (type-of (ingredient sim-ing))))
                if matching-ings-sol
                  ; same ingredient can occur multiple times in slightly different forms, 
              ; so check with which ingredient maximum similarity is found and use that one for score computation
                  do (let* ((sim-scores (loop for matching-ing-sol in matching-ings-sol
                                              collect (compare-hierarchy-ingredient matching-ing-sol unfolded-ing-gold)))
                            (match-scores (mapcar #'compute-dish-score sim-scores))
                            (max-score (apply #'max match-scores))
                            (max-position (position max-score match-scores))
                            (max-ing (nth max-position matching-ings-sol)))
                       (add-similarity-score-to (nth max-position sim-scores) contents-score)
                       (setf unfolded-dish-sol (remove max-ing unfolded-dish-sol)))
              else
                  do (push unfolded-ing-gold missing-ingredients))
          ; TODO RD: misschien alignen met hoe het gebeurt voor mixture hierarchy?        
          (let ((missing-ratio (/ (- (length unfolded-dish-gold) (length missing-ingredients)) (length unfolded-dish-gold)))
                (extra-ratio (/ (+ (length unfolded-dish-sol) (length unfolded-dish-gold)) (length unfolded-dish-gold))))
            (setf (points contents-score) (* (points contents-score) missing-ratio))
            (setf (points contents-score) (/ (points contents-score) extra-ratio))))

         ; compute the final similarity-score for this dish, contents are much more important than container characteristics
        (make-instance 'similarity-score
                       :points (+ (* 0.95 (points contents-score))
                                  (* 0.05 (points container-score)))
                       :max-points (+ (* 0.95 (max-points contents-score))
                                    (* 0.05 (max-points container-score))))))))

(defmethod find-best-dish-score ((sol-final-node irl::irl-program-processor-node) (gold-output-node irl::irl-program-processor-node))
  "Compute a similarity score for all nodes in the solutions and return the best one."
  (let ((node sol-final-node)
        (scores '()))
    (loop for score = (compute-dish-score (compare-node-dishes node gold-output-node))
          do
            (push score scores)
            (setf node (parent node))
          while (and node (irl::primitive-under-evaluation node) (< score 1)))
    (apply #'max scores)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Solution File Evaluation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO RD: eventueel kan solutions een hash-table gemaakt worden
(defun evaluate (filepath &optional (sim-envs *simulation-environments*))
  (let ((solutions (parse-solutions-file filepath))) ; read in the solutions
    (check-solutions-completeness solutions sim-envs) ; check if the solutions file contains all the needed solutions
    (loop for current-solution in solutions
          for current-id = (recipe-id current-solution)
          for current-sim-env = (find current-id sim-envs :key #'(lambda (sim-env) (recipe-id sim-env)))
          for gold-mn = (meaning-network current-sim-env)
          for final-gold-node = (final-node current-sim-env) 
          for gold-output-node = (output-node current-sim-env)
          do
            (init-kitchen-state current-sim-env)
            (let ((extended-mn (append-meaning-and-irl-bindings (meaning-network current-solution) nil)))
              (multiple-value-bind (sol-bindings sol-nodes) (evaluate-irl-program extended-mn nil)
                ; compute subgoal success ratio
                (setf (subgoals-ratio current-solution) (compute-subgoal-success-ratio (first sol-nodes) final-gold-node))
                ; compute the dish score (if all subgoals are reached, then the dish score will already be maximal so no reason to compute it then)
                (if (= (subgoals-ratio current-solution) 1)
                  (setf (dish-score current-solution) 1)
                  (setf (dish-score current-solution) (find-best-dish-score (first sol-nodes) gold-output-node)))
                ; compute the ratio of needed execution time to the execution time of the golden standard
                (setf (time-ratio current-solution) (/ (irl::available-at (get-output-binding (first sol-nodes)))
                                                       (irl::available-at (get-output-binding final-gold-node)))))))
    solutions))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convenience Functions (Removable) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(defparameter test (evaluate "C:\\Users\\robin\\Projects\\babel\\applications\\muhai-cookingbot\\test.lisp"))
;  (evaluate "C:\\Users\\robin\\Projects\\babel\\applications\\muhai-cookingbot\\test.lisp")

(defun print-results (solutions)
  "Convenience Function that prints the measurement results of the given solutions."
  (loop for solution in solutions
        do (print "SOLUTION:")
           (print (recipe-id solution))
           (print "Percentage of Reached Subgoals:")
           (print (subgoals-ratio solution))
           (print "Dish Score:")
           (print (dish-score solution))
           (print "Time Ratio:")
           (print (time-ratio solution))))

; TODO RD: get node at maximum depth that was reached if no solution could be found?
; TODO RD: final value zou altijd een container met een ingredient in moeten zijn
; TODO RD: currently unused
(defun get-final-value (irl-node)
  (let* ((target-var (second (irl::primitive-under-evaluation irl-node)))
         (list-of-bindings (irl::bindings irl-node))
         (target-binding (find target-var list-of-bindings :key #'var)))
    (when target-binding
      (value target-binding))))

;test

;(defun testos (x) (print (time-ratio x)))
;(testos (first test))

;(print-results test)

(defun toppie (x)
  (print "ok"))

;(toppie test)

;;;;;;;;;;;
;; Tests ;;
;;;;;;;;;;;

(defun test-perfect ()
  "The same network as the simulation environment's solution."
  (let* ((solutions (evaluate "applications\\muhai-cookingbot\\tests\\test-perfect.lisp"))
         (perfection (loop for solution in solutions
                             always (and (= (subgoals-ratio solution) 1)
                                         (= (dish-score solution) 1)
                                         (= (time-ratio solution) 1)))))
    (if perfection
      (print "test-perfect: SUCCESS")
      (error "test-perfect: FAILURE"))))

(test-perfect)
     
(defun test-permuted-perfect ()
  "The same network as the simulation environment's solution, but with some parts executed in a different order."
  (let ((solution (first (evaluate "applications\\muhai-cookingbot\\tests\\test-permuted-perfect.lisp" (list *almond-crescent-cookies-environment*)))))
    (if (and (= (subgoals-ratio solution) 1)
             (= (dish-score solution) 1)
             (= (time-ratio solution) 1))
      (print "test-permuted-perfect: SUCCESS")
      (error "test-permuted-perfect: FAILURE"))))

(defun test-imperfect ()
  "The same network as the simulation environment's solution, but with some instructions missing at the end."
  (let ((solution (first (evaluate "applications\\muhai-cookingbot\\tests\\test-imperfect.lisp"  (list *almond-crescent-cookies-environment*)))))
    (if (and (= (subgoals-ratio solution) (/ 23 24))
             (< (dish-score solution) 1))
      (print "test-imperfect: SUCCESS")
      (error "test-imperfect: FAILURE"))))

(defun test-extra-operations ()
  "The same network as the simulation environment's solution, but with some extra instructions at the end."
  (let ((solution (first (evaluate "applications\\muhai-cookingbot\\tests\\test-extra-operation.lisp"  (list *almond-crescent-cookies-environment*)))))
    (if (and (= (subgoals-ratio solution) 1)
             (= (dish-score solution) 1)
             (> (time-ratio solution) 1))
      (print "test-extra-operations: SUCCESS")
      (error "test-extra-operations: FAILURE"))))

(defun test-empty ()
  "A solution that only contains get-kitchen."
  (let ((solution (first (evaluate "applications\\muhai-cookingbot\\tests\\test-empty.lisp"  (list *almond-crescent-cookies-environment*)))))
    (if (and (= (subgoals-ratio solution) 0)
             (= (dish-score solution) 0))
      (print "test-empty: SUCCESS")
      (error "test-empty: FAILURE"))))

(defun test-multiple-recipes ()
  "A file that contains two solutions, an imperfect one and a perfect one."
  (let* ((solutions (evaluate "applications\\muhai-cookingbot\\tests\\test-multiple-recipes.lisp"  (list *almond-crescent-cookies-environment* *afghan-biscuits-environment*)))
         (solution-perfect (first solutions))
         (solution-imperfect (second solutions)))
    (if (and (= (subgoals-ratio solution-perfect) 1)
             (= (dish-score solution-perfect) 1)
             (= (subgoals-ratio solution-imperfect) (/ 23 24))
             (< (dish-score solution-imperfect) 1))
      (print "test-multiple-recipes: SUCCESS")
      (error "test-multiple-recipes: FAILURE"))))

(defun execute-all-tests ()
  (test-perfect)
  (test-permuted-perfect)
  (test-imperfect)
  (test-extra-operations)
  (test-empty))
