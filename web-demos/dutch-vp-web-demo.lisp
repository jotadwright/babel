
;;###########################################################################;;
;;                                                                           ;;
;; Web demo file for the Dutch VP paper (Constructions and Frames)           ;;
;;                                                                           ;;
;;###########################################################################;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading the package and setup ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (asdf:operate 'asdf:load-op :dutch-vp)

(in-package :dutch-vp)
(activate-monitor trace-fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Web demo                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun header-page ()
  (clear-page)
  ;; Title
  (add-element '((hr)))
  (add-element '((h1) "Robust Processing of the Dutch VP"))
  (add-element '((h3) "Paul Van Eecke"))
  (add-element '((p) "This web demo accompanies the paper:"))
  (add-element '((p) "Van Eecke, P. (2017). "((a :href "" :target "_blank") "Robust Processing of the Dutch Verb Phrase.")" "((i) " Constructions and Frames") "."))
  (add-element '((p) "Explanations on how to use this demo are " ((a :href "https://www.fcg-net.org/projects/web-demonstration-guide/" :target "_blank") "here.")))
  (add-element '((hr)))
  (add-element '((p) ((a :name "start") "Menu:")))
  (add-element '((p)  ((a :href "#WD-1") "1. Introduction")))
  (add-element '((p)  ((a :href "#WD-2") "2. The Construction Inventory")))
  (add-element '((p)  ((a :href "#WD-3") "3. An Example (Comprehension)")))
  (add-element '((p)  ((a :href "#WD-4") "4. An Example (Formulation)")))
  (add-element '((p)  ((a :href "#WD-5") "5. An Example (Robust Comprehension)")))
  (add-element '((hr))))

;; (header-page)

(defun introduction ()
  (add-element '((p) ((a :name "WD-1") "")))
  (add-element '((h2) "1. Introduction"))
  (add-element '((p) "This web demonstration supports our paper on the Dutch VP. It presents the grammar in full formal detail using the examples discussed in the paper. For more information about the FCG formalism, notation and processing mechanisms, we refer the reader to the paper <i>Basics of Fluid Construction Grammar</i> (Steels, same issue)."))
  (add-element '((hr))))

;; (introduction)

(defun grammar ()
  (add-element '((p) ((a :name "WD-2") "")))
  (add-element '((h2) "2. The Construction Inventory"))
  (add-element '((p) "The construction inventory is shown below. You can click on the blue boxes and then on the encircled + sign to expand the constructions. The construction inventory contains the morphological and lexical constructions for 2 main verbs ('zingen' (to sing) and 'gaan' (to go)), the morphological and lexical constructions for the perfect and modal auxiliaries, the verb phrase construction, the modality constructions, the perfect-constructions and the tense constructions. Morphological and lexical constructions for other main verbs are not shown here."))
  (add-element (make-html *fcg-constructions* :expand-initially t))
  (add-element '((hr))))

;; (grammar)

(defun comprehension-example ()
  (add-element '((p) ((a :name "WD-3") "")))
  (add-element '((h2) "3. An Example (Comprehension)"))
  (add-element '((p) "The example for comprehension that is discussed in the paper is shown here in full detail. It consists of comprehending the utterance 'zou moeten hebben gezongen' (should have sung). Click on the green boxes (twice for more detail) to zoom in on an individual step in the construction application process."))
  (comprehend  "zou moeten hebben gezongen")
  (add-element '((hr))))

;; (comprehension-example)

(defun formulation-example ()
  (add-element '((p) ((a :name "WD-4") "")))
  (add-element '((h2) "4. An Example (Formulation)"))
  (add-element '((p) "We will now illustrate the same example in formulation. We take the meaning representation that was the output of the comprehension process of 'zou moeten hebben gezongen' (should have sung) and use the grammar to formulate this meaning representation. We can see that the formulated utterance is indeed as expected 'zou moeten hebben gezongen' (should have sung). Moreover, exactly the same constructions have applied."))
  (formulate '((event sing ev) (aspect perfect ev ev-2)
               (focus action - ev-2) (modality obligation ev-2 ev-3)
               (modality hypothesis ev-3 ev-4)
               (time-relation before ev-4 origo) (time-point deictic origo)))
  (add-element '((hr))))

;; (formulation-example)

(defun robust-parsing ()
  (add-element '((p) ((a :name "WD-5") "")))
  (add-element '((h2) "5. An example (Robust Comprehension)"))
(add-element '((p) "The following example illustrates the robust comprehension method as discussed in the section 4.2. of the paper. The input utterance that is comprehended is 'heeft zingen kunnen' (has sing can). First, the sentence is comprehended using the grammar. This yields a partial meaning presentation. Then, the variables in this meaning representation are uniquely renamed (in order to avoid any false bindings from the partial comprehension process). Then, the predicates for tense are added to the meaning representation. The meaning representation is then used in formulation (maximum 3 solutions). The resulting utterances are then ranked according to the minimal word-level edit distance with respect to the input utterance. Finally, the highest-ranked utterance is comprehended."))
  (parse-robust '("heeft" "zingen" "kunnen") :search-beam 3)
  (add-element '((hr))))

;; (robust-parsing)

(create-static-html-page "Robust Processing of the Dutch VP"
(progn
  (header-page)
  (introduction)
  (grammar)
  (comprehension-example)
  (formulation-example)
  (robust-parsing))
)
