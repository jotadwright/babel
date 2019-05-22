;(asdf:operate 'asdf:load-op :fcg)

(in-package :fcg)

(load (babel-pathname :directory '("grammars" "Russian") :name "motion-verbs" :type "lisp"))

(activate-monitor trace-fcg)

(defun header ()
  (clear-page)
  (add-element '((hr)))
  (add-element '((h1) "Russian Verbs of Motion and their Aspectual Partners in Fluid Construction Grammar"))
  (add-element '((p) "This web demo accompanies the paper:"))
  (add-element '((p) "Beuls, K., Knight, Y., and M. Spranger. (2017). "((a :href "" :target "_blank") "Russian Verbs of Motion and their Aspectual Partners in Fluid Construction Grammar")".  "((i) "Constructions and Frames") "."))
  (add-element '((p) "Explanations on how to use this demo are " ((a :href "https://www.fcg-net.org/projects/web-demonstration-guide/" :target "_blank") "here.")))
  (add-element '((hr)))
  (add-element '((p) ((a :name "start") "Menu:")))
  (add-element '((p)  ((a :href "#WD-1") "WD-1. Motion verbs in Russian")))
  (add-element '((p)  ((a :href "#WD-2") "WD-2. Spatial and temporal prefixes.")))
  ;(add-element '((p)  ((a :href "#WD-3") "WD-3. Stem changes.")))
  ;(add-element '((p)  ((a :href "#WD-4") "WD-4. Irregular verb conjugation")))
  (add-element '((hr))))


(defun WD-1 ()
  (add-element '((a :name "WD-1")))
  (add-element '((h2) "WD-1. Motion verbs in Russian"))
  (add-element '((p) "Motion verbs in Russian always have two aspectual realizations, depending on the directionality that is intended by the speaker. A walking event can be expressed by <i> xodit </i> or <i> idti </i> respectively. The two morphological constructions that are responsible for expressing or interpreting this distinction look as follows (click on the &#8853; sign to fully expand a construction), the only difference being the value of the direction feature (in the sem-cat):"))
  (add-element (make-html (find-cxn 'idti-morph *russian-motion-grammar*) :expand-initially t 
             :wrap-in-paragraph nil))
  (add-element (make-html (find-cxn 'xodit-morph *russian-motion-grammar*) :expand-initially t 
             :wrap-in-paragraph nil))

  (add-element '((p) "The interesting part lies in the interaction of such morphological constructions with lexical and grammatical constructions that also form part of the construction inventory. To express the conceptualized meaning representation that `masha is walking up and down for a while', eight constructions are at work:"))
  (formulate '((person masha o-1) 
               (motion walk ev-1) (walk-agent ev-1 o-1)
               (event-directionality ev-1 multidirectional)
               (event-aktionsart ev-1 durative)
               (event-bounds ev-1 ongoing)) :cxn-inventory *russian-motion-grammar*)
  (add-element '((p) "The event-directionality, event-aktionsart and event-bounds meaning predicates are translated by three corresponding grammatical constructions that add specific sem-cat or syn-cat features to the verb stem unit in the transient structure. These features are then required by the production locks of the morphological constructions (xodit-morph and unmarked-prefix). You can click on every step in the construction application process to inspect how a certain affected the transient structure. The resulting utterance is shown under the visualization of the final transient structure.")) 
  (add-element '((p) ((a :href "#start") "Back to menu")))
  (add-element '((hr))))
;(clear-page)
;(WD-1)

(defun WD-2 ()
  (add-element '((a :name "WD-2")))
  (add-element '((h2) "WD-2. Spatial and temporal prefixes."))
  (add-element '((p) "The Russian grammar fragment contains seven different prefixes, which can form all types of prefixed forms: Natural Perfectives, Specialised Perfectives, Complex Act Perfectives, Single Act Perfectives or Imperfectives. For example, <i> po- </i> added to the unidirectional stem <i> idti </i> can produce an NP, <i> po- </i> added to the multidirectional <i> xodit </i>, a CAP. Stem directionality thus needs to be taken into account to differentiate between the above types."))
  (add-element '((p) "We chose for a grammar design with a single po-prefix construction for both types. The use of variable equalities makes it possible to allow for two different instantiations of the construction schema. If you open the construction here below and click on the ?multi or ?uni variables you will see that the same variable occurs in three locations of the feature structure: as the value of the direction feature, the aktionsart feature and the cluster model (in the contributing part). We can read this as follows: if the verb has a multidirectional usage, its aktionsart is delimitative and it is a Complex Act Perfective."))
  (add-element (make-html (find-cxn 'po-temporal-prefix *russian-motion-grammar*) :expand-initially t 
             :wrap-in-paragraph nil))
  (add-element '((p) "The comprehension process of <i> po- idti </i> results in the following application process and resulting meaning:"))
  (comprehend "po- idti" :cxn-inventory *russian-motion-grammar*)
  (add-element '((hr)))
  (add-element '((p) "A further distinction can be made within the categories as they can differ in terms of Aktionsart. Aktionsart can introduce a temporal dimension to an event by conveying which part of the event is in focus: ingressive for the beginning, delimitative for duration, terminative for the endpoint. To express the meaning that Masha has started to walk (<i> masha za- xodit </i>), the za-temporal-prefix would be called upon. An example of such a formulation process is included here below."))
  (formulate '((person masha o-1) (motion walk ev-1) (walk-agent ev-1 o-1)
               (event-directionality ev-1 multidirectional)
               (event-aktionsart ev-1 ingressive)
               (event-bounds ev-1 complete)) :cxn-inventory *russian-motion-grammar*)
  (add-element '((hr)))
  (add-element '((p) "The <i> za- </i> prefix can also be used to express spatial trajectories, along with the multi or unidirectional nature of the verb stem. If we ask the FCG engine for all solutions of the utterance <i> masha za- xodit </i>, we get a temporal (see above) as well as a spatial meaning back. The temporal interpretation requires eight constructions to apply (the ingressive adds the aktionsart meaning), whereas the spatial branch consists of seven constructions. The trajectory meaning is directly added by the za-spatial prefix construction, since it is specific to this particular prefix: (event-trajectory ?ev in-out). Paraphrasing the utterance, we get two resulting interpretations: (i) Masha pops in and (ii) Masha starts to walk in multiple directions."))
  (comprehend-all "masha za- xodit" :cxn-inventory *russian-motion-grammar*)
  
  (add-element '((p) ((a :href "#start") "Back to menu")))
  (add-element '((hr))))

;(WD-2)


(defun make-demo ()
(create-static-html-page "Russian motion verbs"
  (header)
  (wd-1)
  (wd-2)
;  (wd-3)
  ))
(make-demo)