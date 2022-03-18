
;;##############################################################################;;
;;                                                                              ;;
;; Web demo 'Understanding and executing recipes expressed in natural language' ;;
;;                                                                              ;;
;;##############################################################################;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading the package and setup ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ql:quickload :muhai-cookingbot)

(in-package :muhai-cookingbot)
(use-package :web-interface)
(activate-monitor trace-fcg)
(activate-monitor trace-irl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Web demo                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun header-page ()
  (clear-page)
  ;; Title
  (add-element '((hr)))
  (add-element '((h1) "Understanding and executing recipes expressed in natural language"))
  (add-element '((h2) "Katrien Beuls and Paul Van Eecke"))  
  (add-element '((hr)))

  (add-element '((h3) "Summary"))
  (add-element '((p) "This web demonstration shows the understanding and execution of a recipe expressed in natural language. Concretely, we start from an "
                 ((a :href "https://www.simplyrecipes.com/recipes/almond_crescent_cookies/") "almond crescent cookie recipe ")
                 ", interpret the instructions in the recipe, and execute them in a simulated kitchen environment."))
  
  (add-element '((hr)))
  
  (add-element '((h3) "Contents"))
  
  (add-element '((p)  ((a :href "#1") ((b) "1. Recipe"))))
  (add-element '((p)  ((a :href "#2") ((b) "2. Demonstration of the understanding process"))))
  (add-element '((h3) ""))

  (add-element '((hr)))
  
    (add-element '((p) "Explanations on how to use this demo are " ((a :href "https://www.fcg-net.org/projects/web-demonstration-guide/" :target "_blank") "here.")))

  (add-element '((p :style "color:darkred") "Warning! A loading delay might occur when expanding parts of the search processes. If the page seems no longer responsive, refreshing might help."))
  (add-element '((h3) ""))
  
  (add-element '((hr))))

;; (header-page)


(defun recipe ()
  (add-element '((a :name "1")))
  (add-element '((h2) "1. Recipe"))
  (add-element '((h4) "Almond crescent cookies"))
  (add-element '((p) "Ingredients: "))
  (add-element '((ul)
                 ((li) "226 grams butter, room temperature")
                 ((li) "116 grams sugar")
                 ((li) "4 grams vanilla extract")
                 ((li) "4 grams almond extract")
                 ((li) "340 grams flour")
                 ((li) "112 grams almond flour")
                 ((li) "29 grams powdered sugar")))
  (add-element '((p) "Instructions:"))
  (add-element '((ul)
                 ((li) "Beat the butter and the sugar together until light and fluffy.")
                 ((li) "Add the vanilla and almond extracts and mix.")
                 ((li) "Add the flour and the almond flour.")
                 ((li) "Mix thoroughly.")
                 ((li) "Take generous tablespoons of the dough and roll it into a small ball, about an inch in diameter, and then shape it into a crescent shape.")
                 ((li) "Place onto a parchment paper lined baking sheet.")
                 ((li) "Bake at 175°C for 15-20 minutes.")
                 ((li) "Dust with powdered sugar.")))
    (add-element '((hr))))


;; (recipe)

(defun understanding ()
  (add-element '((a :name "2")))
  (add-element '((h2) "2. Demonstration of the understanding process"))
  (process-uterances '(;;;; Ingredients
                     "226 grams butter , room temperature"
                     "116 grams sugar"    
                     "4 grams vanilla extract"
                     "4 grams almond extract"
                     "340 grams flour"
                     "112 grams almond flour"
                     "29 grams powdered sugar"
                     
                     ;;;; Instructions
                     "beat the butter and the sugar together until light and fluffy"
                     "add the vanilla and almond extracts and mix"
                     "add the flour and the almond flour"
                     "mix thoroughly"
                     "take generous tablespoons of the dough and roll it into a small ball , about an inch in diameter , and then shape it into a crescent shape"
                     "place onto a parchment paper lined baking sheet"
                     ;"bake at 175 °C for 15 - 20 minutes" ; or until a light golden brown
                     ;"dust with powdered sugar"
                     )
                     (initialise-personal-dynamic-memory
                      *fcg-constructions*
                      `((get-kitchen ,(make-var 'kitchen-state)))))
  (add-element '((hr))))

;; (understanding)

#|
(clear-page)
(load (babel-pathname :directory '("applications" "muhai-cookingbot" "recipes") :name "almond-cookies-grammar" :type "lisp"))
(create-static-html-page "Recipe understanding"
  (header-page)
  (recipe)
  (understanding)
)
|#
