;;############################################
;;Web demo: Propbank grammar
;;############################################

;; (ql:quickload :propbank-english)
(in-package :propbank-english)

(defun header ()
  (clear-page)
  (add-element '((hr)))
  (add-element '((h1) "Operationalising large-scale, usage-based construction grammar"))
  (add-element '((h3) "Paul Van Eecke &amp; Katrien Beuls"))
  ;(add-element '((p) "This web demo accompanies the paper:" ((br)) 
  ;               "Van Eecke, Paul &amp; Beuls, Katrien. (2017). " ((a :href "https://ehai.ai.vub.ac.be/assets/pdfs/fcg-creativity.pdf" :target "_blank") "Exploring the creative potential of computional construction grammar. ") ((i) "Zeitschrift f&#252;r Anglistik und Amerikanistik ") "66(3): 341-355. "))
  (add-element '((p) "Explanations on how to use this demo are " ((a :href "https://www.fcg-net.org/projects/web-demonstration-guide/" :target "_blank") "here.")))
;  (add-element '((hr)))
 ; (add-element '((p) ((a :name "start") "Menu:")))
 ; (add-element '((p)  ((a :href "#WD-1") "1.  Introduction")))
;  (add-element '((p)  ((a :href "#WD-1") "1.  Introduction")))
  (add-element '((hr))))

(defun aim ()
  (add-element '((a :name "WD-1")))
  (add-element '((h2) "1.  Aim"))
  (add-element '((p) "Our main objective is to investigate how constructionist approaches to language can be operationalised on a large scale. We do this through the development of a large construction grammar for English, which should fulfill the following requirements: "))
  (add-element '((ul)
                 ((li) "Constructionist: The grammar should consist of a dynamically expandable set of learned form-meaning pairings.")
                 ((li) "Usage-based: The grammar should be learned from observed language use.")
                 ((li) "Large-scale: The grammar should have a broad (exhaustive) coverage of the language.")
                 ((li) "Computational: The grammar should have a computational implementation, so that it can be automatically evaluated and to support its use for applications such as semantic role labelling.")))
  (add-element '((hr))))

(defun methodology ()
  (add-element '((a :name "WD-2")))
  (add-element '((h2) "2.  Method"))
  (add-element '((p) "In order to build up an inventory of form-meaning mappings in a usage-based fashion, we first need data from which we can learn. This data should inform us both about the form and the meaning of utterances. We chose to use" ((b) " PropBank-annotated corpora") " for two main reasons: (i) large, exhaustively annotated corpora are available, and (ii) PropBank annotations focus on semantic frames and argument structure relations, an important aspect of language that is central to construction grammar."))

  (add-element '((p) "This data is annotated as follows. Take for example the utterance " ((i) "I believe you") ". The corresponding meaning annotation would look as follows: "))

  (add-element (make-html (make-instance 'frame :frame-name 'believe.01
                                         :frame-evoking-element (make-instance 'frame-evoking-element
                                                                               :fel-string "believe")
                                         :frame-elements (list (make-instance 'frame-element
                                                                              :fe-name 'arg0
                                                                               :fe-string "I")
                                                               (make-instance 'frame-element
                                                                              :fe-name 'arg1
                                                                              :fe-string "you")))
                          :expand-initially t))
  (add-element '((p) "In the annotation we can see that the sentence evokes a single frame, namely a believe.01 frame. This frame has three arguments, namely the frame-evoking element itself (FEE), an arg0 and an arg1. believe.01 refers to a particular sense of the word believe. This sense is associated to a roleset, which describes the actual sense of the arg0 and arg1 arguments. For the believe.01 roleset, arg0 refers to the believer, whereas arg1 refers to the believed. The rolesets and their possible roles are all described in the PropBank documentation. "))

  (add-element '((p) "We will represent constructions as "))
  (add-element '((hr))))



(defun make-demo ()
  (create-static-html-page "Operationalising large-scale, usage-based construction grammar"
    (header)
    (aim)
    (methodology))
  )


;(make-demo)