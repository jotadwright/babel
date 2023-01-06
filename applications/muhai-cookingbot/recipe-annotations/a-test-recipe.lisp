(ql:quickload :muhai-cookingbot)
(in-package :muhai-cookingbot)
;; The 'trace-irl' monitor will make sure that
;; the IRL evaluation process is shown on the web
;; interface (which can be found at localhost:8000).
;; We need to activate it:
(activate-monitor trace-irl)

;; ##################################################################
;; Almond Crescent Cookies recipe
;; https: //w ww. simplyrecipes.com/recipes/almond_crescent_cookies/
;; ##################################################################

;; Defining the initial kitchen state
(defparameter *initial-kitchen-state*
  (make-instance
   'kitchen-state
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
                                                                                                                              :value 500)))))))
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
                                                       :contents (list (make-instance 'almond-flour :amount
                                                                                      (make-instance 'amount
                                                                                                     :unit (make-instance 'g)
                                                                                                     :quantity (make-instance 'quantity
                                                                                                                              :value 1000)))))
                                        (make-instance 'medium-bowl
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
                                   (make-instance 'baking-paper))))))


(defparameter *almond-cookies-recipe*

  '(
    (get-kitchen ?kitchen-1)

    ;(set-kitchen ?kitchen ?kitchen-1)

    (fetch-and-proportion ?proportioned-butter ?kitchen-2 ?kitchen-1 ?target-container-1 butter 134 g)

    (fetch-and-proportion ?proportioned-sugar ?kitchen-3 ?kitchen-2 ?target-container-2 sugar 134 g)

    (transfer-contents ?output-1 ?rest-1 ?kitchen-4 ?kitchen-3 ?empty-container-1 ?proportioned-butter ?quantity-1 ?unit-1)

    (transfer-contents ?to-mix ?rest-2 ?kitchen-5 ?kitchen-4 ?output-1 ?proportioned-sugar ?quantity-2 ?unit-2)

    (mix ?container-with-dough ?kitchen-6 ?kitchen-5 ?to-mix ?mixture-tool)

    (fetch ?fetched-baking-tray ?kitchen-7 ?kitchen-6 baking-tray 1)
    (fetch ?fetched-baking-paper ?kitchen-8 ?kitchen-7 baking-paper 1)

    (line ?lined-baking-tray ?kitchen-9 ?kitchen-8 ?fetched-baking-tray ?fetched-baking-paper)

    (shape ?shaped-result ?kitchen-10 ?kitchen-9 ?container-with-dough ?lined-baking-tray crescent-shape)


    ;; Primitive to get intermediate result after shaping to test faster
    ;(get-baked-stuff ?shaped-result ?kitchen-10)

    (bake ?baked-dough ?kitchen-11 ?kitchen-10 ?shaped-result ?oven 15 minute 175 degrees-celsius)

    (sprinkle ?sprinkled-object ?kitchen-12 ?kitchen-11 ?baked-dough ?topping-container)

    ))


;; ======================
;; Append bindings to the recipe
;; ======================

(defparameter *extended-recipe*
  (append-meaning-and-irl-bindings *almond-cookies-recipe* nil))


;; ======================
;; Evaluate the recipe
;; ======================

(evaluate-irl-program *extended-recipe* nil :primitive-inventory *vr-primitives*)

;; ======================
;; Visualise the recipe
;; ======================

;; (draw-recipe *almond-cookies-recipe*)
;; (draw-recipe *extended-recipe*)
