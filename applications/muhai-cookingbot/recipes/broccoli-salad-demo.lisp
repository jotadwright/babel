(ql:quickload :muhai-cookingbot)
(ql:quickload :measuring-understanding)
(in-package :muhai-cookingbot)

;; load the kitchen state
(load (babel-pathname :directory '("applications" "muhai-cookingbot" "recipes") :name "broccoli-salad-kitchen-state" :type "lisp"))

;; load the grammar
(load (babel-pathname :directory '("applications" "muhai-cookingbot" "recipes") :name "broccoli-salad-grammar" :type "lisp"))


(defun run-recipe (&key vr? inn?)
  ;; reset stuff
  (clear-output)
  (reset-id-counters)
  ;; activate monitors
  (activate-monitor trace-fcg)
  (activate-monitor trace-irl)
  ;; activate INN
  (when inn?
    (monitors::activate-monitor measuring-understanding::questions-solved-by-mental-simulation)
    (monitors::activate-monitor measuring-understanding::questions-solved-by-grammar)
    (activate-monitor measuring-understanding::questions-introduced-by-grammar)
    (activate-monitor measuring-understanding::node-names-fcg)
    (activate-monitor measuring-understanding::node-names-irl)
    (activate-vis-js)

    (setf measuring-understanding::*time* 0.1)

    (measuring-understanding::initialize-values)
    (setf measuring-understanding::*visual-dialog-inn*
          (measuring-understanding::make-integrative-network))
    (add-element `((div :id "narrativeNetwork")
                   ,(wi::make-vis-network :element-id "narrativeNetwork"
                                          :nodes nil
                                          :edges nil))))
  (let* ((init-op `((get-kitchen ,(make-var 'kitchen-state))))
         (primitive-inventory (if vr? *vr-primitives* *irl-primitives*))
         (pdm (initialise-personal-dynamic-memory
               *fcg-constructions* init-op :primitive-inventory primitive-inventory)))
    (multiple-value-bind (final-set-of-bindings meaning-network)
      (process-utterances '(;;;; Ingredients
                            "1 head fresh broccoli"
                            "50 grams red onion , chopped"
                            "450 grams cooked bacon"
                            "2.5 tablespoons cider vinegar"
                            "230 grams mayonnaise"
                            "70 grams sugar"
                            "170 grams grated mozzarella cheese"

                            ;;;; Instructions
                            "cut cooked bacon into pieces"
                            "chop up broccoli into bite size pieces"
                            "mix broccoli , onions , bacon and mozzarella in large bowl"
                            "in separate large bowl combine vinegar , sugar and mayo"
                            "pour over broccoli mixture and toss to coat" 
                            "best if made a day ahead and stored in the refrigerator"
                            
                            "end")
                          pdm)
      (declare (ignore final-set-of-bindings))
    (append init-op meaning-network))))



(run-recipe :vr? nil :inn? nil)