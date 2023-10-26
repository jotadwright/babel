(ql:quickload :muhai-cookingbot)
(in-package :muhai-cookingbot)

(defparameter *ks*
  (parse-namestring "/Users/jensnevens/Downloads/dbgWS/bsWS25.log"))
  ;(babel-pathname :directory '("applications" "muhai-cookingbot")
  ;                :name "almondCookiesInitialState" :type "json"))

(defparameter *data*
  (with-open-file (stream *ks* :direction :input)
    (jzon:parse stream))) ;:key-fn #'(lambda (key) (make-kw (camel-case->lisp key))))))

;(defparameter *alist-data* (hash-table->alist-rec *data*))

(defparameter *symbolic-ks* (vr-to-symbolic-kitchen *data*))
(defparameter *vr-ks* (symbolic-to-vr-kitchen *symbolic-ks*))

; (defparameter *hash-data* (alist->hash-table-rec *vr-ks*))

(jzon:stringify *vr-ks* :stream (babel-pathname :directory '(".tmp") :name "test" :type "json"))


(monitors::activate-monitor trace-irl)

(evaluate-irl-program
 (instantiate-non-variables-in-irl-program
  '((get-kitchen ?kitchen)
    (fetch-and-proportion ?proportioned-broccoli ?ks-with-broccoli ?kitchen ?target-container-1 broccoli 1 piece)
    (fetch-and-proportion ?proportioned-onion ?ks-with-onion ?ks-with-broccoli ?target-container-2 red-onion 50 g)
    (peel ?peeled-onion ?peelings ?ks-with-peeled-onion ?ks-with-onion ?proportioned-onion ?knife)
    (cut ?chopped-onion ?ks-with-chopped-onion ?ks-with-peeled-onion ?peeled-onion chopped ?knife ?cutting-board-1)
    ))
 nil :primitive-inventory *vr-primitives*)

(defparameter *broccoli-salad*
  '((get-kitchen ?kitchen)
    ;; fetch an empty bowl and place the broccoli in the bowl
    (fetch-and-proportion ?proportioned-broccoli ?ks-with-broccoli ?kitchen ?target-container-1 broccoli 1 piece)
    ;; fetch an empty bowl and place the red onion in the bowl
    (fetch-and-proportion ?proportioned-onion ?ks-with-onion ?ks-with-broccoli ?target-container-2 red-onion 50 g)
    ;; peel the red onion (stays in the same bowl)
    (peel ?peeled-onion ?peelings ?ks-with-peeled-onion ?ks-with-onion ?proportioned-onion ?knife)
    ;; cut the peeled onion (stays in the same bowl)
    (cut ?chopped-onion ?ks-with-chopped-onion ?ks-with-peeled-onion ?peeled-onion chopped ?knife ?cutting-board-1)
    ;; fetch an empty bowl and place the cooked-bacon in the bowl
    (fetch-and-proportion ?proportioned-bacon ?ks-with-bacon ?ks-with-chopped-onion ?target-container-3 cooked-bacon 450 g)
    ;; fetch an empty bowl and portion 2.5 tablespoons of cider vinegar in the bowl (ciderVinegarBottle)
    (fetch-and-proportion ?proportioned-vinegar ?ks-with-vinegar ?ks-with-bacon ?target-container-4 cider-vinegar 2.5 tablespoon)
    ;; fetch an empty bowl and portion 230g of mayonnaise in the bowl (mayonnaiseJar)
    (fetch-and-proportion ?proportioned-mayo ?ks-with-mayo ?ks-with-vinegar ?target-container-5 mayonnaise 230 g)
    ;; fetch an empty bowl and portion 70g of white sugar in the bowl
    (fetch-and-proportion ?proportioned-sugar ?ks-with-sugar ?ks-with-mayo ?target-container-6 white-sugar 70 g)
    ;; fetch an empty bowl and place/portion mozzarella in the bowl
    (fetch-and-proportion ?proportioned-cheese ?ks-with-grated-cheese ?ks-with-sugar ?target-container-7 grated-mozzarella 170 g)
    ;; cut the bacon (stays in the same bowl)
    (cut ?chopped-bacon ?ks-with-chopped-bacon ?ks-with-grated-cheese ?proportioned-bacon chopped ?knife ?cutting-board-2)
    ;; cut the broccoli (stays in the same bowl)
    (cut ?chopped-broccoli ?ks-with-chopped-broccoli ?ks-with-chopped-bacon ?proportioned-broccoli chopped ?knife ?cutting-board-3)
    ;; fetch a new large bowl
    (fetch ?large-bowl-1 ?ks-with-large-bowl-1 ?ks-with-chopped-broccoli large-bowl 1)
    ;; transfer the cut broccoli into the large bowl
    (transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-large-bowl-1 ?large-bowl-1 ?chopped-broccoli ?quantity-a ?unit-a)
    ;; transfer the chopped onion into the same bowl
    (transfer-contents ?output-container-b ?rest-b ?output-ks-b ?output-ks-a ?output-container-a ?chopped-onion ?quantity-b ?unit-b)
    ;; transfer the cheese into the same bowl
    (transfer-contents ?output-container-c ?rest-c ?output-ks-c ?output-ks-b ?output-container-b ?proportioned-cheese ?quantity-c ?unit-c)
    ;; mingle the contents of the bowl (becomes hetereogeneous mixture)
    (mingle ?broccoli-mixture ?ks-with-broccoli-mixture ?output-ks-c ?output-container-c ?mingling-tool)
    ;; fetch another large bowl
    (fetch ?large-bowl-2 ?ks-with-large-bowl-2 ?ks-with-broccoli-mixture large-bowl 1)
    ;; transfer the vinegar, sugar, and mayo into that bowl and mix (becomes homogenerous mixture)
    (transfer-contents ?output-container-d ?rest-d ?output-ks-d ?ks-with-large-bowl-2 ?large-bowl-2 ?proportioned-vinegar ?quantity-d ?unit-d)
    (transfer-contents ?output-container-e ?rest-e ?output-ks-e ?output-ks-d ?output-container-d ?proportioned-sugar ?quantity-e ?unit-e)
    (transfer-contents ?output-container-f ?rest-f ?output-ks-f ?output-ks-e ?output-container-e ?proportioned-mayo ?quantity-f ?unit-f)
    (mix ?dressing ?ks-with-dressing ?output-ks-f ?output-container-f ?mixing-tool)
    ;; transfer the dressing to the bowl with broccoli, onion, and cheese + mingle (become heterogeneous mixture)
    (transfer-contents ?output-container-g ?rest-g ?output-ks-g ?ks-with-dressing ?broccoli-mixture ?dressing ?quantity-g ?unit-g)
    (mingle ?broccoli-salad ?ks-with-broccoli-salad ?output-ks-g ?output-container-g ?mingling-tool)
    ;; place the bowl in the fridge
    (refrigerate ?cooled-salad ?ks-with-cooled-salad ?ks-with-broccoli-salad ?broccoli-salad ?fridge 24 hour)))