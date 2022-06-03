(ql:quickload :grammar-learning)
(in-package :grammar-learning)


(setf *raise-errors* t)
;; full logging except trace-fcg
(progn
  (deactivate-all-monitors)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor summarize-results-after-n-interactions)
  (activate-monitor show-type-hierarchy-after-n-interactions)
  (activate-monitor trace-interactions-in-wi))

;; minimal logging after 100 interactions with type hierarchy
(progn
  (deactivate-all-monitors)
  (activate-monitor display-metrics)
  (activate-monitor summarize-results-after-n-interactions)
  (activate-monitor show-type-hierarchy-after-n-interactions)
  (activate-monitor print-a-dot-for-each-interaction))

;; minimal logging after 100 interactions
(progn
  (deactivate-all-monitors)
  (activate-monitor display-metrics)
  (activate-monitor summarize-results-after-n-interactions)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor export-categorial-network-evolution-to-jsonl)
  (activate-monitor export-type-hierarchy-to-json))


;; full logging except trace-fcg
(progn
  (deactivate-all-monitors)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor summarize-results-after-n-interactions)
  ;(activate-monitor export-categorial-network-evolution-to-jsonl)
  ;(activate-monitor show-type-hierarchy-after-n-interactions)
  ;(activate-monitor trace-interactions-in-wi)
  )

;; full logging
(progn
  (deactivate-all-monitors)
  (activate-monitor display-metrics)
  (activate-monitor trace-fcg)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor summarize-results-after-n-interactions)
  (activate-monitor show-type-hierarchy-after-n-interactions)
  (activate-monitor trace-interactions-in-wi))


(progn
  (wi::reset)
  (notify reset-monitors)
  (defparameter *experiment*
    (eval `(make-instance 'grammar-learning-experiment
                   :entries '((:determine-interacting-agents-mode . :corpus-learner)
                         (:observation-sample-mode . :debug)
                         (:meaning-representation . :irl)
                         (:de-render-mode . :de-render-string-meets-no-punct)
                         (:corpus-files-root . ,(merge-pathnames
                                     (make-pathname :directory '(:relative "clevr-grammar-learning"))
                                     cl-user:*babel-corpora*))
                         (:corpus-data-file . ,(make-pathname :directory '(:relative "train")
                                                   :name "stage-1" :type "jsonl")))))))
                              

;(cl-store:store (grammar (first (agents *experiment*))) (babel-pathname :directory '("experiments" "clevr-grammar-learning" "raw-data") :name "cxn-inventory-train-random" :type "store"))

;(add-element (make-html (categorial-network (grammar (first (agents *experiment*)))) :weights t))
;(add-element (make-html (grammar (first (agents *experiment*)))))

;(defparameter *th* (categorial-network (grammar (first (interacting-agents *experiment*)))))

;;; test single interaction
;(run-interaction *experiment*)

;;; test series of interactions
;(run-series *experiment* (length (question-data *experiment*)))

;(run-series *experiment* 20)

;(run-series *experiment* 2000) ; item-based to holistic fails


#|
ISSUES:

I2: there aren't that many holophrases to learn from, add the repairs that learn from a combination of cxns


 
TODO:
- add new repairs that don't only start from holophrases, but also from minimally differing combinations of cxns

SUBST
example:  what is the color of the cube + cube  +  what is the size of the B => what is the X of the Y
step 1:   apply already existing lex cxns, create temporary item-based cxn what is the color of the X
step 2:   loop through all item-based cxns that you already have, check if there is one diff in form and meaning with the temp item-based cxn, if so generalise it further
result:   what is the X of the Y

DELETION
example:  what is the color of the cube + cube + what is the color of the large Y => what is the color of the X Y
step 1:   apply already existing lex cxns, create temporary item-based cxn what is the color of the X
step 2:   loop through all item-based cxns that you already have, check if there is a continuous diff in form and meaning with the temp item-based cxn and that the observation is a subset of the existing cxn
result:   what is the color of the X Y

ADDITION
example: what is the color of the large cube + cube + what is the color of the X => what is the color of the X Y
step 1:   apply already existing lex cxns, create temporary item-based cxn what is the color of the X
step 2:   loop through all item-based cxns that you already have, check if there is a continuous diff in form and meaning with the temp item-based cxn and that the observation is a superset of the existing cxn
result:   what is the color of the X Y

building blocks:
- holistic to item-based repair: gives the temporary item-based cxn (form and meaning is all we need) AVAILABLE
- function to check for similar cxn with one difference ADAPTABLE from existing SUBST/DEL/ADD functions, but take item-based instead of holophrase

step 1: create test case
step 2: copy holistic to item-based repair






pad naar modulariteit:
what is the color of the cube
what size has the sphere

what ?x + size has the sphere + is the color of the cube

what is the color of the cylinder
what ?x applies

"is the color of the large cylinder" : loop through all repairs again!
1. add cat link (we already have it but it didn't occur in what ?x yet)
2. item-based to holistic: we already have is the color of the ?x, we learn "large cylinder"


"large cylinder": loop through all repairs again!
1. add cat link (we already have it but it didn't occur in "is the color of the ?x" yet)
2. item-based to holistic (we already have large ?X), we learn "cylinder"

"cylinder": 

telkens als je een item-based kan toepassen ga je niet gewoon een holistic cxn maken van de rest, maar ga je opnieuw voor dat deel door alle repairs


|#