(ql:quickload :predicate-networks)
(in-package :pn)

;; ############################################################################
;; Test Cases
;; This file contains a number of examples of the anti-unification over
;; predicate networks. Each example consists of two input networks
;; (network-1 and network-2), their corresponding form in natural language,
;; and the result of the anti-unification process. The latter consists of
;; the generalisation, two bindings lists, and two deltas.
;; ----------------------------------------------------------------------------

;; Substitution example (same type in bind statement)
;; ==================================================

(defparameter *test-case-1*
  (let (;; are there red cubes?
        (network-1
         '((get-context ?context)
           (filter ?set-1 ?context ?cube)
           (bind shape-category ?cube cube)
           (filter ?set-2 ?set-1 ?color-1)
           (bind color-category ?color-1 red)
           (exist ?target ?set-2)))

        ;; are there red spheres?
        (network-2
         '((get-context ?c)
           (filter ?s1 ?c ?b1)
           (bind shape-category ?b1 sphere)
           (filter ?s2 ?s1 ?b2)
           (bind color-category ?b2 red)
           (exist ?t ?s2)))

        ;; are there red X?
        (msg 
         '((get-context ?context)
           (filter ?set-1 ?context ?X)
           (bind shape-category ?X ?Y)
           (filter ?set-2 ?set-1 ?color-1)
           (bind color-category ?color-1 red)
           (exist ?target ?set-2)))

        (bindings-1
         '((?cube . ?X) (cube . ?Y)))

        (bindings-2
         '((?b1 . ?X) (sphere . ?Y)))

        (delta-1 nil)
        (delta-2 nil))
    (draw-anti-unification-test-case network-1 network-2 msg bindings-1 bindings-2 delta-1 delta-2)))

;; ############################################################################

;; Substitution example (different type in bind statement)
;; =======================================================

(defparameter *test-case-2*
  (let (;; are there red cubes?
        (network-1
         '((get-context ?context)
           (filter ?set-1 ?context ?shape-1)
           (bind shape-category ?shape-1 cube)
           (filter ?set-2 ?set-1 ?color-1)
           (bind color-category ?color-1 red)
           (exist ?target ?set-2)))

        ;; are there small cubes?
        (network-2
         '((get-context ?c)
           (filter ?s1 ?c ?b1)
           (bind shape-category ?b1 cube)
           (filter ?s2 ?s1 ?b2)
           (bind size-category ?b2 small)
           (exist ?t ?s2)))
        
        ;; are there X cubes?
        (msg
         '((get-context ?context)
           (filter ?set-1 ?context ?shape-1)
           (bind shape-category ?shape-1 cube)
           (filter ?set-2 ?set-1 ?X)
           (bind ?Y ?X ?Z)
           (exist ?target ?set-2)))

        (bindings-1
         '((?color-1 . ?X) (color-category . ?Y) (red . ?Z)))
        
        (bindings-2
         '((?b2 . ?X) (size-category . ?Y) (small . ?Z)))
        (delta-1 nil)
        (delta-2 nil))
    (draw-anti-unification-test-case network-1 network-2 msg bindings-1 bindings-2 delta-1 delta-2)))

;; ############################################################################

;; Substitution example (different final predicate)
;; ================================================

(defparameter *test-case-3*
  (let (;; are there red cubes?
        (network-1
         '((get-context ?context)
           (filter ?set-1 ?context ?shape-1)
           (bind shape-category ?shape-1 cube)
           (filter ?set-2 ?set-1 ?color-1)
           (bind color-category ?color-1 red)
           (exist ?target ?set-2)))

        ;; how many red cubes?
        (network-2
         '((get-context ?c)
           (filter ?s1 ?c ?b1)
           (bind shape-category ?b1 cube)
           (filter ?s2 ?s1 ?b2)
           (bind color-category ?b2 red)
           (count ?t ?s2)))

        ;; X red cubes?
        (msg
         '((get-context ?context)
           (filter ?set-1 ?context ?shape-1)
           (bind shape-category ?shape-1 cube)
           (filter ?Y ?set-1 ?color-1)
           (bind color-category ?color-1 red)))

        (bindings-1
         '((?set-2 . ?Y)))

        (bindings-2
         '((?s2 . ?Y)))

        (delta-1
         '((exist ?target ?set-2)))

        (delta-2
         '((count ?t ?s2))))
    (draw-anti-unification-test-case network-1 network-2 msg bindings-1 bindings-2 delta-1 delta-2)))

;; ############################################################################

;; Substitution example (network with cycle)
;; =========================================

;; how many cubes or spheres
(defparameter *network-1*  
  '((get-context ?context)
    (filter ?cube-set ?context ?cube)
    (bind shape-category ?cube cube)
    (filter ?sphere-set ?context ?sphere)
    (bind shape-category ?sphere sphere)
    (union ?set ?cube-set ?sphere-set)
    (count ?target ?set)))

;; how many cubes or cylinders
(defparameter *network-2* 
  '((get-context ?c)
    (filter ?s1 ?c ?b1)
    (bind shape-category ?b1 cube)
    (filter ?s2 ?c ?b2)
    (bind shape-category ?b2 cylinder)
    (union ?s3 ?s1 ?s2)
    (count ?t ?s3)))

;; MSG
;; how many cubes or X
'((get-context ?context)
  (filter ?cube-set ?context ?cube)
  (bind shape-category ?cube cube)
  (filter ?sphere-set ?context ?Y)
  (bind shape-category ?Y ?X)
  (union ?set ?cube-set ?sphere-set)
  (count ?target ?set))
;; bindings-1
'((?sphere . ?Y) (sphere . ?X))
;; bindings-2
'((?b2 . ?Y) (cylinder . ?X))
;; delta-1
nil
;; delta-2
nil

;; ############################################################################

;; Addition example
;; ================

;; how many red cubes?
(defparameter *network-1* 
  '((get-context ?context)
    (filter ?set-1 ?context ?shape-1)
    (bind shape-category ?shape-1 cube)
    (filter ?set-2 ?set-1 ?color-1)
    (bind color-category ?color-1 red)
    (count ?target ?set-2)))

;; how many large red cubes?
(defparameter *network-2* 
  '((get-context ?c)
    (filter ?s1 ?c ?b1)
    (bind shape-category ?b1 cube)
    (filter ?s2 ?s1 ?b2)
    (bind color-category ?b2 red)
    (filter ?s3 ?s2 ?b3)
    (bind size-category ?b3 large)
    (count ?t ?s3)))

;; MSG
;; how many X red cubes?

;; bindings-1

;; bindings-2

;; delta-1

;; delta-2

;; ############################################################################

;; Deletion example
;; ================

;; how many large red cubes?
(defparameter *network-1* 
  '((get-context ?c)
    (filter ?s1 ?c ?b1)
    (bind shape-category ?b1 cube)
    (filter ?s2 ?s1 ?b2)
    (bind color-category ?b2 red)
    (filter ?s3 ?s2 ?b3)
    (bind size-category ?b3 large)
    (count ?t ?s3)))

;; how many red cubes?
(defparameter *network-2* 
  '((get-context ?context)
    (filter ?set-1 ?context ?shape-1)
    (bind shape-category ?shape-1 cube)
    (filter ?set-2 ?set-1 ?color-1)
    (bind color-category ?color-1 red)
    (count ?target ?set-2)))

;; MSG
;; how many X red cubes?

;; bindings-1

;; bindings-2

;; delta-1

;; delta-2

;; ############################################################################

;; Substitution example (generalising over previous generalisation)
;; ================================================================

;; how many X cubes?
(defparameter *network-1* 
  '((get-context ?context)
    (filter ?set-1 ?context ?shape-1)
    (bind shape-category ?shape-1 cube)
    (filter ?set-2 ?set-1 ?Y)
    (?X ?Y)
    (count ?target ?set-3)))

;; how many X spheres?
(defparameter *network-2* 
  '((get-context ?c)
    (filter ?s1 ?c ?b1)
    (bind shape-category ?b1 sphere)
    (filter ?s2 ?s1 ?Y)
    (?X ?Y)
    (count ?t ?s2)))

;; MSG
;; how many X Y?

;; bindings-1

;; bindings-2

;; delta-1

;; delta-2


;; ############################################################################

;; Substitution example (same operators in different order)
;; ========================================================

;; how many large red cubes?
(defparameter *network-1* 
  '((get-context ?c)
    (filter ?s1 ?c ?b1)
    (bind shape-category ?b1 cube)
    (filter ?s2 ?s1 ?b2)
    (bind color-category ?b2 red)
    (filter ?s3 ?s2 ?b3)
    (bind size-category ?b3 large)
    (count ?t ?s3)))
(add-element (predicate-network->html *pattern-network*))

;; how many red large cubes?
(defparameter *network-2* 
  '((get-context ?context)
    (filter ?set-1 ?context ?shape-1)
    (bind shape-category ?shape-1 cube)
    (filter ?set-2 ?set-1 ?size-1)
    (bind size-category ?size-1 large)
    (filter ?set-3 ?set-2 ?color-1)
    (bind color-category ?color-1 red)
    (count ?target ?set-3)))
(add-element (predicate-network->html *source-network*))

;; MSG

;; bindings-1

;; bindings-2

;; delta-1

;; delta-2

;; ############################################################################

;; Substitution example (not minimal difference)
;; =============================================

;; what color is the large cube?
(defparameter *network-1*
  )

;; what color is the small blue sphere?
(defparameter *network-2*
  )

;; MSG

;; bindings-1

;; bindings-2

;; delta-1

;; delta-2

;; ############################################################################

;; Substitution example (not minimal difference, with cycle)
;; =========================================================

;; how many cubes or large spheres?
(defparameter *network-1*
  )

;; how many small spheres or cylinders?
(defparameter *network-2*
  )







