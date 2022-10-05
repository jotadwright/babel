(ql:quickload :cooking-bot-new)
(in-package :cooking-bot-new)

(progn 
  (monitors::activate-monitor fcg::trace-fcg)
  (monitors::activate-monitor trace-irl)
  (activate-monitor questions-introduced-by-grammar)
  (activate-monitor questions-solved-by-grammar)
  (activate-monitor questions-introduced-by-mental-simulation)
  (activate-monitor questions-solved-by-mental-simulation)
  (activate-monitor questions-solved-by-discourse)
  (activate-monitor questions-introduced-by-discourse)
  (activate-monitor questions-solved-by-ontology)
  (activate-monitor questions-introduced-by-ontology)
  (activate-monitor node-names-fcg)
  (activate-monitor node-names-irl)
  )

(setf *small-almond-cookies-recipe* '(;;;; Ingredients
                                "226 grams butter , room temperature"
                                "116 grams sugar"    
                                ;;;; Instructions
                                "beat the butter and the sugar together until light and fluffy"
                                "end"))

(measure-and-understand  *small-almond-cookies-recipe*)
(measure-and-understand  *almond-cookies-recipe*)

(setf *almond-cookies-recipe* '(;;;; Ingredients
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
                                "bake at 175 C for 15 - 20 minutes" ; or until a light golden brown
                                "dust with powdered sugar"
                                "end"
                                ))

(write-measures-to-file 'node-names-fcg 'node-names-irl
                        (list 'questions-introduced-by-grammar
                              'questions-solved-by-grammar
                              'questions-solved-by-mental-simulation
                              'questions-solved-by-discourse
                              'questions-introduced-by-mental-simulation
                              'questions-introduced-by-discourse
                              'questions-introduced-by-ontology
                              'questions-solved-by-ontology))

(format t "~%~%questions: grammar ~a, ontology ~a,simulation ~a, discourse ~a ~% answers: grammar ~a, ontology ~a, simulation ~a, discourse ~a~%~%"
        *total-number-of-questions-grammar*
        *total-number-of-questions-ontology*
        *total-number-of-questions-simulation*
        *total-number-of-questions-discourse*
        *total-number-of-answers-grammar*
        *total-number-of-answers-ontology*
        *total-number-of-answers-simulation*
        *total-number-of-answers-discourse*)


(defun total-questions-grammar ()
  (loop for el in  (slot-value (monitors::get-monitor 'questions-introduced-by-grammar) 'values)
        for vals = (sum (flatten (first el)))
        sum vals))

(defun total-answers-grammar ()
  (loop for el in  (slot-value (monitors::get-monitor 'questions-solved-by-grammar) 'values)
        for vals = (sum (flatten (first el)))
      sum vals))

(defun total-questions-ontology ()
  (loop for el in  (slot-value (monitors::get-monitor 'questions-introduced-by-ontology) 'values)
        for vals = (sum (flatten (first el)))
      sum vals))

(defun total-answers-ontology ()
  (loop for el in  (slot-value (monitors::get-monitor 'questions-solved-by-ontology) 'values)
        for vals = (sum (flatten (first el)))
      sum vals))

(defun total-questions-simulation ()
  (loop for el in  (slot-value (monitors::get-monitor 'questions-introduced-by-mental-simulation) 'values)
        for vals = (sum (flatten (first el)))
      sum vals))

(defun total-answers-simulation ()
  (loop for el in  (slot-value (monitors::get-monitor 'questions-solved-by-mental-simulation) 'values)
        for vals = (sum (flatten (first el)))
      sum vals))

(total-answers-grammar) ;14
(total-questions-grammar) ;41 
(total-answers-ontology) ;134
(total-questions-ontology) ;140
(total-questions-simulation) ;4
(total-answers-simulation) ;26





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Demo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun header ()
  (clear-page)
  (deactivate-all-monitors) ; just to be sure that there is no other stuff 
  (activate-monitor trace-fcg)
  (activate-monitor trace-irl)
  (activate-monitor questions-introduced-by-grammar)
  (activate-monitor questions-solved-by-grammar)
  (activate-monitor questions-solved-by-mental-simulation)
  (activate-monitor questions-solved-by-discourse)
  (activate-monitor questions-introduced-by-discourse)
  (activate-monitor questions-solved-by-ontology)
  (activate-monitor questions-introduced-by-ontology)
  (activate-monitor node-names-fcg)
  (activate-monitor node-names-irl)
  (add-element '((hr)))
  (add-element
   '((h1) "Measuring Understanding Recipe Execution"))
  (measure-and-understand  *small-almond-cookies-recipe*)
  )

(defun full-demo ()
  (header)
  ;(section-1)
  )

;(full-demo)


;;;; Static web page
; (web-interface:create-static-html-page "Measures" (full-demo))


