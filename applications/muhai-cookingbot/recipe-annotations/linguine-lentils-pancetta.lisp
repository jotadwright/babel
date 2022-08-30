;(ql:quickload :muhai-cookingbot)
(in-package :muhai-cookingbot)
;;#############################################
;;# Linguine with lentils and pancetta        #
;;#############################################

(setf *linguine-lentils-recipe*

      `((get-kitchen ?input-kitchen-state) ;;retrieve the current kitchen state
        
; Ingredients
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;200g small brown lentils
(fetch-and-proportion ?lentils ?ks-with-lentils ?input-kitchen-state ?emtpy-container-1 brown-lentils 200 g)
;;2 tablespoons extra-virgin olive oil
(fetch-and-proportion ?olive-oil ?ks-with-olive-oil ?ks-with-lentils ?emtpy-container-2 olive-oil 2 tablespoon)
;;125g pancetta, cut into thin slivers
(fetch-and-proportion ?pancetta ?ks-with-pancetta ?ks-with-olive-oil ?emtpy-container-3 pancetta 125 g)
(cut ?cut-pancetta ?ks-with-cut-pancetta ?ks-with-pancetta ?pancetta thin-slivers ?cutting-tool)
;;2 stalks celery, cut into peasized cubes
(fetch-and-proportion ?celery ?ks-with-celery ?ks-with-cut-pancetta ?emtpy-container-4 celery 2 stalk)
(cut ?cut-pancetta ?ks-with-cut-celery ?ks-with-celery ?celery peasized-cubes ?cutting-tool)
;;4 large cloves garlic, chopped
(fetch-and-proportion ?garlic ?ks-with-garlic ?ks-with-cut-celery ?empty-container-5 garlic 4 clove)
(cut ?chopped-garlic ?ks-with-chopped-garlic ?ks-with-garlic ?garlic ?cut-pattern ?cutting-tool)
;;2 teaspoons chopped rosemary leaves
(fetch-and-proportion ?rosemary ?ks-with-rosemary ?ks-with-chopped-garlic ?empty-container-6 fresh-rosemary 2 teaspoon)
(pick ?rosemary-leaves ?rosemary-stalks ?ks-with-rosemary-leaves ?ks-with-rosemary ?rosemary)
(cut ?chopped-rosemary ?ks-with-chopped-rosemary ?ks-with-rosemary-leaves ?rosemary-leaves ?cut-pattern ?cutting-tool)
;;1-2 fresh hot red chillies, slided and seeded, if desired
(fetch-and-proportion ?chilipepper ?ks-with-chilipepper ?ks-with-chopped-rosemary ?empty-container-7 red-chilipepper 1 piece)
;;125ml dry white wine
(fetch-and-proportion ?wine ?ks-with-wine ?ks-with-chilipepper ?empty-container-8 dry-white-wine 125 ml)
;;400g dried linguine
(fetch-and-proportion ?linguine ?ks-with-linguine ?ks-with-wine ?empty-container-9 linguine 400 g)
;;good handful of fresh parsley, stems removed and leaves coarsely chopped
(fetch-and-proportion ?parsley ?ks-with-parsley ?ks-with-linguine ?empty-container-10 fresh-parsley 1 handful)
;(remove-stems ??)
(cut ?chopped-parsley ?ks-with-chopped-parsley ?ks-with-parsley ?parsley ?cut-pattern ?cutting-tool)

; Method
;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Step 1
;;;;;;;;;;

;; Place lentils in a saucepan and cover with about 5cm water.
(fetch ?saucepan ?ks-with-saucepan ?ks-with-chopped-parsley saucepan) 
(transfer-contents ?saucepan-with-lentils ?empty-lentil-container ?ks-with-lentils-in-saucepan ?ks-with-saucepan ?saucepan ?quant ?unit) ;;no quantity and unit given ==> all contents!!
(cover ?saucepan-with-water ?ks-with-lentils-covered-with-water ?ks-with-lentils-in-saucepan ?saucepan-with-lentils water 5 cm)
;; Add salt and cover the pan.
(season ?saucepan-with-salt ?ks-with-lentils-salted ?ks-with-lentils-covered-with-water salt)
(cover ?saucepan-covered ?ks-with-lentils-salted-and-covered ?ks-with-lentils-salted ?saucepan-with-salt ?lid ?quant-2 ?unit-2) ;;no lid, quantity and unit given => use standard saucepan lid
;; Set over low heat and cook until the water comes to a boil.
`(bind stove ?stove ,(stove *initial-kitchen-state*))
(transfer ?saucepan-on-stove ?ks-with-saucepan-on-stove ?ks-with-lentils-salted-and-covered ?saucepan-covered ?stove) ;;last elt = destination (actual stove in the kitchen)
(set-stove ?ks-with-stove-on ?ks-with-saucepan-on-stove low-heat)
(boil ?boiling-lentils ?ks-with-lentils-boiling ?ks-with-stove-on ?saucepan-on-stove ?quant-3 ?unit-3) ;;no boiling time given ==> until contents of container reaches boiling point
;; Set lid on an angle and cook over very low heat for about 30 minutes, or until lentils are tender.
(uncover ?saucepan-with-lid-on-angle ?ks-with-uncovered-lid ?ks-with-lentils-boiling ?boiling-lentils lid-on-angle) ;; lid-on-angle = partial uncovering
(set-stove ?ks-with-lentils-cooking-on-very-low-heat ?ks-with-uncovered-lid very-low-heat)
(cook ?lentils-cooked ?ks-with-cooked-lentils ?ks-with-lentils-cooking-on-very-low-heat ?saucepan-with-lid-on-angle 30 minute)
;; Remove from the heat and set aside lentils in their liquid.
(bind counter-top ?countertop ,(counter-top *initial-kitchen-state*))
(transfer ?cooked-lentils-on-countertop ?ks-with-cooked-lentils-on-countertop ?ks-with-cooked-lentils ?lentils-cooked ?countertop) ;;destination = countertop

;; Step 2
;;;;;;;;;;

;; Combine oil and pancetta in a large, deep frying pan and cook over moderate heat for about 5 minutes, or until pancetta is crisp and golden.
(fetch ?frying-pan ?ks-with-frying-pan ?ks-with-cooked-lentils-on-countertop frying-pan)
(transfer-contents ?frying-pan-with-oil ?empty-oil-bowl ?ks-with-oil-in-pan ?ks-with-frying-pan ?frying-pan ?olive-oil ?all ?ml)
(transfer-contents ?frying-pan-with-oil ?empty-oil-bowl ?ks-with-oil-in-pan ?ks-with-frying-pan ?frying-pan ?pancetta ?all ?g)
)
;; Remove pancetta with a slotted spoon to a plate lined with paper towels.

;; Step 3
;;;;;;;;;;;

;; Add celery to the pan and cook for about 5 minutes, or until it starts to soften.
;; Add garlic, rosemary and chilli and cook for about 2 minutes, or until fragrant.
;; Drain lentils and add to celery mixture; add wine and cook until wine has reduced by half.
;; Check seasoning.

;; Step 4
;;;;;;;;;;;

;; Meanwhile, cook pasta in salted boiling water until al dente.
;; Drain, reserving some of the cooking water.
;; Add pasta to lentil mixture and toss over heat for 1 minutes.
;; Remove from heat and toss in parsley leaves and pancetta.
;; Serve in deep, heated bowls.



(defparameter *initial-kitchen-state* 
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
                                   (make-instance 'baking-paper)))
         (make-instance 'stove
                        :contents nil))))


(defparameter *pdm* (initialise-personal-dynamic-memory
                    *fcg-constructions*
                    `((get-kitchen ,(make-var 'kitchen-state)))))


(append-meaning-and-irl-bindings *linguine-lentils-recipe* nil)

(evaluate-irl-program *linguine-lentils-recipe* nil)

(draw-recipe *linguine-lentils-recipe* )
