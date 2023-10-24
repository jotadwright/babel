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
    (fetch-and-proportion ?proportioned-broccoli ?ks-with-broccoli ?kitchen ?target-container-1 broccoli 1 piece)))
 nil :primitive-inventory *vr-primitives*)

(defparameter *broccoli-salad*
  '((get-kitchen ?kitchen)
    (fetch-and-proportion ?proportioned-broccoli ?ks-with-broccoli ?kitchen ?target-container-1 broccoli 1 piece)
    (fetch-and-proportion ?proportioned-onion ?ks-with-onion ?ks-with-broccoli ?target-container-2 red-onion 50 g)
    (peel ?peeled-onion ?peelings ?ks-with-peeled-onion ?ks-with-onion ?proportioned-onion ?knife)
    (cut ?chopped-onion ?ks-with-chopped-onion ?ks-with-peeled-onion ?peeled-onion chopped ?knife ?cutting-board-1)
    (fetch-and-proportion ?proportioned-bacon ?ks-with-bacon ?ks-with-chopped-onion ?target-container-3 cooked-bacon 450 g)
    (fetch-and-proportion ?proportioned-vinegar ?ks-with-vinegar ?ks-with-bacon ?target-container-4 cider-vinegar 2.5 tablespoon)
    (fetch-and-proportion ?proportioned-mayo ?ks-with-mayo ?ks-with-vinegar ?target-container-5 mayonnaise 230 g)
    (fetch-and-proportion ?proportioned-sugar ?ks-with-sugar ?ks-with-mayo ?target-container-6 white-sugar 70 g)
    (fetch-and-proportion ?proportioned-cheese ?ks-with-grated-cheese ?ks-with-sugar ?target-container-7 grated-mozzarella 170 g)
    (cut ?chopped-bacon ?ks-with-chopped-bacon ?ks-with-grated-cheese ?proportioned-bacon chopped ?knife ?cutting-board-2)
    (cut ?chopped-broccoli ?ks-with-chopped-broccoli ?ks-with-chopped-bacon ?proportioned-broccoli chopped ?knife ?cutting-board-3)
    (fetch ?large-bowl-1 ?ks-with-large-bowl-1 ?ks-with-chopped-broccoli large-bowl 1)
    (transfer-contents ?output-container-a ?rest-a ?output-ks-a ?ks-with-large-bowl-1 ?large-bowl-1 ?chopped-broccoli ?quantity-a ?unit-a)
    (transfer-contents ?output-container-b ?rest-b ?output-ks-b ?output-ks-a ?output-container-a ?chopped-onion ?quantity-b ?unit-b)
    (transfer-contents ?output-container-c ?rest-c ?output-ks-c ?output-ks-b ?output-container-b ?proportioned-cheese ?quantity-c ?unit-c)
    (mingle ?broccoli-mixture ?ks-with-broccoli-mixture ?output-ks-c ?output-container-c ?mingling-tool)
    (fetch ?large-bowl-2 ?ks-with-large-bowl-2 ?ks-with-broccoli-mixture large-bowl 1)
    (transfer-contents ?output-container-d ?rest-d ?output-ks-d ?ks-with-large-bowl-2 ?large-bowl-2 ?proportioned-vinegar ?quantity-d ?unit-d)
    (transfer-contents ?output-container-e ?rest-e ?output-ks-e ?output-ks-d ?output-container-d ?proportioned-sugar ?quantity-e ?unit-e)
    (transfer-contents ?output-container-f ?rest-f ?output-ks-f ?output-ks-e ?output-container-e ?proportioned-mayo ?quantity-f ?unit-f)
    (mix ?dressing ?ks-with-dressing ?output-ks-f ?output-container-f ?mixing-tool)
    (transfer-contents ?output-container-g ?rest-g ?output-ks-g ?ks-with-dressing ?broccoli-mixture ?dressing ?quantity-g ?unit-g)
    (mingle ?broccoli-salad ?ks-with-broccoli-salad ?output-ks-g ?output-container-g ?mingling-tool)
    (refrigerate ?cooled-salad ?ks-with-cooled-salad ?ks-with-broccoli-salad ?broccoli-salad ?fridge 24 hour)))