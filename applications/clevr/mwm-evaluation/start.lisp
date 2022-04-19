(ql:quickload :mwm-evaluation)
(in-package :mwm-evaluation)


(activate-monitor trace-fcg)
(activate-monitor trace-irl)
(deactivate-all-monitors)

;;------------;;
;; Evaluation ;;
;;------------;;

(defparameter *config-entries*
  '((:dot-interval . 100)
    (:nr-of-scenes . 5)
    (:nr-of-questions . nil)
    (:data-split . "val")
    (:world-type . :extracted)))

;; Evaluate one particular serie
(evaluate-mwm-serie 1 *config-entries*)
(evaluate-mwm-serie 2)
(evaluate-mwm-serie 3)
(evaluate-mwm-serie 4)
(evaluate-mwm-serie 5)
(evaluate-mwm-serie 6)
(evaluate-mwm-serie 7)
(evaluate-mwm-serie 8)
(evaluate-mwm-serie 9)
(evaluate-mwm-serie 10)

;; Evaluate on all series of concepts by loading the different series into the ontology
(evaluate-all-series)

;;--------------;;
;; The ontology ;;
;;--------------;;

(make-mwm-ontology
 (merge-pathnames (make-pathname :directory '(:relative "serie-1"))
                  *simulated-concepts-path*))

;;---------;;
;; Testing ;;
;;---------;;

;; Test sentences (see "Babel/grammars/clevr-grammar/start.lisp" for more examples):
(let ((ontology
       (make-mwm-ontology
        (merge-pathnames (make-pathname :directory '(:relative "serie-1"))
                         *simulated-concepts-path*))))
  (test-utterance-in-first-scene "there is a big gray object that is the same shape as the purple rubber object; what is it made of?"
                                 ontology)
  (test-utterance-in-first-scene "What color is the large sphere?" ontology)
  (test-utterance-in-first-scene "How many things have the same shape as the large red thing?" ontology)
  (test-utterance-in-first-scene "How many things are left of the small gray sphere that is in front of the large sphere that is right of the large blue cube?"
                                 ontology)
  (test-utterance-in-first-scene "How many things are left of the purple sphere that is behind the yellow thing?"
                                 ontology))




;;---------;;
;; Testing ;;
;;---------;;

(defparameter *ontology*
  (make-mwm-ontology
        (merge-pathnames (make-pathname :directory '(:relative "serie-1"))
                         *simulated-concepts-path*)))

(defparameter *pathname-entity*
  (make-instance 'pathname-entity
                 :pathname (parse-namestring "/Users/jensnevens/Babel-Corpora/CLEVR-v1.0/scenes/val/CLEVR_val_000005.json")))

(defparameter *program*
  `((BIND PATHNAME-ENTITY CLEVR-GRAMMAR::?SCENE ,*pathname-entity*) (BIND SPATIAL-RELATION-CATEGORY ?SPATIAL-RELATION-20167 BEHIND) (FILTER ?TARGET-160246 ?TARGET-160240 CLEVR-GRAMMAR::?SCENE ?SIZE-992) (UNIQUE ?OBJECT-109914 ?TARGET-160246) (BIND ATTRIBUTE-CATEGORY ?ATTRIBUTE-33764 MATERIAL) (EQUAL? ?TARGET-160366 ?SRC-30835 ?SRC-30836 ?ATTRIBUTE-33764) (QUERY ?SRC-30835 ?OBJECT-109913 CLEVR-GRAMMAR::?SCENE ?ATTRIBUTE-33764) (QUERY ?SRC-30836 ?OBJECT-109914 CLEVR-GRAMMAR::?SCENE ?ATTRIBUTE-33764) (FILTER ?TARGET-160249 ?TARGET-160242 CLEVR-GRAMMAR::?SCENE ?SIZE-984) (UNIQUE ?OBJECT-109913 ?TARGET-160249) (BIND SIZE-CATEGORY ?SIZE-984 LARGE) (FILTER ?TARGET-160242 ?TARGET-160241 CLEVR-GRAMMAR::?SCENE ?COLOR-1281) (BIND SIZE-CATEGORY ?SIZE-992 LARGE) (FILTER ?TARGET-160240 ?SOURCE-67832 CLEVR-GRAMMAR::?SCENE ?SHAPE-4442) (BIND SHAPE-CATEGORY ?SHAPE-4441 SPHERE) (BIND SHAPE-CATEGORY ?SHAPE-4444 SPHERE) (BIND SIZE-CATEGORY ?SIZE-983 SMALL) (FILTER ?TARGET-160238 ?TARGET-160237 CLEVR-GRAMMAR::?SCENE ?COLOR-1280) (BIND MATERIAL-CATEGORY ?MATERIAL-1250 RUBBER) (FILTER ?TARGET-160236 ?SOURCE-67835 CLEVR-GRAMMAR::?SCENE ?SHAPE-4452) (BIND SHAPE-CATEGORY ?SHAPE-4452 CUBE) (FILTER ?TARGET-160237 ?TARGET-160236 CLEVR-GRAMMAR::?SCENE ?MATERIAL-1250) (BIND COLOR-CATEGORY ?COLOR-1280 YELLOW) (BIND SHAPE-CATEGORY ?SHAPE-4442 THING) (FILTER ?TARGET-160241 ?SOURCE-67833 CLEVR-GRAMMAR::?SCENE ?SHAPE-4444) (BIND COLOR-CATEGORY ?COLOR-1281 YELLOW) (FILTER ?TARGET-160243 ?SOURCE-67835 CLEVR-GRAMMAR::?SCENE ?SHAPE-4441) (BIND MATERIAL-CATEGORY ?MATERIAL-1254 METAL) (FILTER ?TARGET-160245 ?TARGET-160243 CLEVR-GRAMMAR::?SCENE ?MATERIAL-1254) (BIND COLOR-CATEGORY ?COLOR-1282 GRAY) (FILTER ?TARGET-160247 ?TARGET-160245 CLEVR-GRAMMAR::?SCENE ?COLOR-1282) (BIND SIZE-CATEGORY ?SIZE-986 LARGE) (UNIQUE ?TARGET-OBJECT-22492 ?TARGET-160250) (FILTER ?TARGET-160250 ?TARGET-160247 CLEVR-GRAMMAR::?SCENE ?SIZE-986) (UNIQUE ?TARGET-OBJECT-22500 ?TARGET-160239) (FILTER ?TARGET-160239 ?TARGET-160238 CLEVR-GRAMMAR::?SCENE ?SIZE-983) (BIND SPATIAL-RELATION-CATEGORY ?SPATIAL-RELATION-20169 FRONT) (CLEVR-GRAMMAR::SEGMENT-SCENE ?SOURCE-67835 CLEVR-GRAMMAR::?SCENE) (RELATE ?SOURCE-67832 ?TARGET-OBJECT-22492 ?SOURCE-67835 CLEVR-GRAMMAR::?SCENE ?SPATIAL-RELATION-20169) (RELATE ?SOURCE-67833 ?TARGET-OBJECT-22500 ?SOURCE-67835 CLEVR-GRAMMAR::?SCENE ?SPATIAL-RELATION-20167)))
(setf *program* (substitute-categories *program*))


(loop repeat 20
      do (time
          (evaluate-irl-program *program* *ontology*
                                :n 1 :primitive-inventory *mwm-primitives*)




