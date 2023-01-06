(ql:quickload :fcg)
(in-package :fcg)


;; Substitution (same category in bind statement)
(print-anti-unification-results
 (anti-unify-predicate-network
  ;; are there any red cubes?
  '((get-context ?context)
    (filter ?set-1 ?context ?cube)
    (bind shape-category ?cube cube)
    (filter ?set-2 ?set-1 ?color-1)
    (bind color-category ?color-1 red)
    (exist ?target ?set-2))
  ;; are there any red spheres?
  '((get-context ?c)
    (filter ?s1 ?c ?b1)
    (bind shape-category ?b1 sphere)
    (filter ?s2 ?s1 ?b2)
    (bind color-category ?b2 red)
    (exist ?t ?s2))))

(print-anti-unification-results
 (anti-unify-predicate-network
  (form-constraints-with-meets (split "are there any red cubes" #\space) :variables t)
  (form-constraints-with-meets (split "are there any red spheres" #\space) :variables t)))

;; Substitution (different category in bind statement)
(print-anti-unification-results
 (anti-unify-predicate-network
  ;; are there any red cubes?
  '((get-context ?context)
    (filter ?set-1 ?context ?shape-1)
    (bind shape-category ?shape-1 cube)
    (filter ?set-2 ?set-1 ?color-1)
    (bind color-category ?color-1 red)
    (exist ?target ?set-2))
  ;; are there any small cybes?
  '((get-context ?c)
    (filter ?s1 ?c ?b1)
    (bind shape-category ?b1 cube)
    (filter ?s2 ?s1 ?b2)
    (bind size-category ?b2 small)
    (exist ?t ?s2))))

(print-anti-unification-results
 (anti-unify-predicate-network
  (form-constraints-with-meets (split "are there any red cubes" #\space) :variables t)
  (form-constraints-with-meets (split "are there any small cylinders" #\space) :variables t)))

;; Substitution (different final predicate)
(print-anti-unification-results
 (anti-unify-predicate-network
  ;; are there any red cubes?
  '((get-context ?context)
    (filter ?set-1 ?context ?shape-1)
    (bind shape-category ?shape-1 cube)
    (filter ?set-2 ?set-1 ?color-1)
    (bind color-category ?color-1 red)
    (exist ?target ?set-2))
  ;; how many red cubes are there?
  '((get-context ?c)
    (filter ?s1 ?c ?b1)
    (bind shape-category ?b1 cube)
    (filter ?s2 ?s1 ?b2)
    (bind color-category ?b2 red)
    (count ?t ?s2))))

(print-anti-unification-results
 (anti-unify-predicate-network
  (form-constraints-with-meets (split "are there any red cubes" #\space) :variables t)
  (form-constraints-with-meets (split "how many red cubes are there" #\space) :variables t)))

;; Substitution (network with cycle)
(print-anti-unification-results
 (anti-unify-predicate-network
  ;; how many cubes or spheres?
  '((get-context ?context)
    (filter ?cube-set ?context ?cube)
    (bind shape-category ?cube cube)
    (filter ?sphere-set ?context ?sphere)
    (bind shape-category ?sphere sphere)
    (union ?set ?cube-set ?sphere-set)
    (count ?target ?set))
  ;; how many cubes or cylinders?
  '((get-context ?c)
    (filter ?s1 ?c ?b1)
    (bind shape-category ?b1 cube)
    (filter ?s2 ?c ?b2)
    (bind shape-category ?b2 cylinder)
    (union ?s3 ?s1 ?s2)
    (count ?t ?s3))))

(print-anti-unification-results
 (anti-unify-predicate-network
  (form-constraints-with-meets (split "how many cubes or spheres are there" #\space) :variables t)
  (form-constraints-with-meets (split "how many cubes or cylinders are there" #\space) :variables t)))

;; Different order
(print-anti-unification-results
 (anti-unify-predicate-network
  ;; how many red large cubes?
  '((get-context ?context)
    (filter ?set-1 ?context ?shape-1)
    (bind shape-category ?shape-1 cube)
    (filter ?set-2 ?set-1 ?size-1)
    (bind size-category ?size-1 large)
    (filter ?set-3 ?set-2 ?color-1)
    (bind color-category ?color-1 red)
    (count ?target ?set-3))
  ;; how many large red cubes?
  '((get-context ?c)
    (filter ?s1 ?c ?b1)
    (bind shape-category ?b1 cube)
    (filter ?s2 ?s1 ?b2)
    (bind color-category ?b2 red)
    (filter ?s3 ?s2 ?b3)
    (bind size-category ?b3 large)
    (count ?t ?s3))))

(print-anti-unification-results
 (anti-unify-predicate-network
  (form-constraints-with-meets (split "how many red large cubes are there" #\space) :variables t)
  (form-constraints-with-meets (split "how many large red cubes are there" #\space) :variables t)))

;; Addition
(print-anti-unification-results
 (anti-unify-predicate-network
  ;; how many red cubes?
  '((get-context ?context)
    (filter ?set-1 ?context ?shape-1)
    (bind shape-category ?shape-1 cube)
    (filter ?set-2 ?set-1 ?color-1)
    (bind color-category ?color-1 red)
    (count ?target ?set-2))
  ;; how many large red cubes?
  '((get-context ?c)
    (filter ?s1 ?c ?b1)
    (bind shape-category ?b1 cube)
    (filter ?s2 ?s1 ?b2)
    (bind color-category ?b2 red)
    (filter ?s3 ?s2 ?b3)
    (bind size-category ?b3 large)
    (count ?t ?s3))))

(print-anti-unification-results
 (anti-unify-predicate-network
  (form-constraints-with-meets (split "how many red cubes are there" #\space) :variables t)
  (form-constraints-with-meets (split "how many large red cubes are there" #\space) :variables t)))

;; Deletion
(print-anti-unification-results
 (anti-unify-predicate-network
  ;; how many large red cubes?
  '((get-context ?c)
    (filter ?s1 ?c ?b1)
    (bind shape-category ?b1 cube)
    (filter ?s2 ?s1 ?b2)
    (bind color-category ?b2 red)
    (filter ?s3 ?s2 ?b3)
    (bind size-category ?b3 large)
    (count ?t ?s3))
  ;; how many red cubes?
  '((get-context ?context)
    (filter ?set-1 ?context ?shape-1)
    (bind shape-category ?shape-1 cube)
    (filter ?set-2 ?set-1 ?color-1)
    (bind color-category ?color-1 red)
    (count ?target ?set-2))))

(print-anti-unification-results
 (anti-unify-predicate-network
  (form-constraints-with-meets (split "how many large red cubes are there" #\space) :variables t)
  (form-constraints-with-meets (split "how many red cubes are there" #\space) :variables t)))

;; Generalise over previous generalisation
(print-anti-unification-results
 (anti-unify-predicate-network
  ;; how many X cubes?
  '((get-context ?context)
    (filter ?set-1 ?context ?shape-1)
    (bind shape-category ?shape-1 cube)
    (filter ?set-2 ?set-1 ?X)
    (count ?target ?set-3))
  ;; how many X spheres?
  '((get-context ?c)
    (filter ?s1 ?c ?b1)
    (bind shape-category ?b1 sphere)
    (filter ?s2 ?s1 ?Y)
    (count ?t ?s2))))

(print-anti-unification-results
 (anti-unify-predicate-network
  '((string ?how "how")
    (string ?many "many")
    (string ?cubes "cubes")
    (string ?are "are")
    (string ?there "there")
    (meets ?how ?many)
    (meets ?many ?large)
    (meets ?large ?cubes)
    (meets ?cubes ?are)
    (meets ?are ?there))
  '((string ?how "how")
    (string ?many "many")
    (string ?spheres "spheres")
    (string ?are "are")
    (string ?there "there")
    (meets ?how ?many)
    (meets ?many ?red)
    (meets ?red ?spheres)
    (meets ?spheres ?are)
    (meets ?are ?there))))



;; not minimal difference
(print-anti-unification-results
 (anti-unify-predicate-network
  ;; what color is the large cube?
  '((get-context ?context)
    (filter ?set-1 ?context ?shape-1)
    (bind shape-category ?shape-1 cube)
    (filter ?set-2 ?set-1 ?size-1)
    (bind size-category ?size-1 large)
    (unique ?object-1 ?set-2)
    (query ?target ?object-1 ?attribute-1)
    (bind attribute-category ?attribute-1 color))
  ;; what color is the small blue sphere?
  '((get-context ?c)
    (filter ?s1 ?c ?b1)
    (bind shape-category ?b1 sphere)
    (filter ?s2 ?s1 ?b2)
    (bind color-category ?b2 blue)
    (filter ?s3 ?s2 ?b3)
    (bind size-category ?b3 small)
    (unique ?o1 ?s3)
    (query ?t ?o1 ?b4)
    (bind attribute-category ?b4 color))))

(print-anti-unification-results
 (anti-unify-predicate-network
  (form-constraints-with-meets (split "what color is the large cube" #\space) :variables t)
  (form-constraints-with-meets (split "what color is the small blue sphere" #\space) :variables t)))











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
         '((get-context ?V1)
           (filter ?V2 ?V1 ?V3)
           (bind shape-category ?V3 ?V4)
           (filter ?v5 ?V2 ?V6)
           (bind color-category ?V6 red)
           (exist ?V7 ?V5)))

        (bindings-1
         '((?context . ?V1) (?set-1 . ?V2) (?cube . ?V3) (cube . ?V4)
           (?set-2 . ?V5) (?color-1 . ?V6) (?target . ?V7)))

        (bindings-2
         '((?c . ?V1) (?s1 . ?V2) (?b1 . ?V3) (sphere . ?V4)
           (?s2 . ?V5) (?b2 . ?V6) (?t . ?V7)))

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
         '((get-context ?V1)
           (filter ?V2 ?V1 ?V3)
           (bind shape-category ?V3 cube)
           (filter ?V4 ?V2 ?V5)
           (bind ?V6 ?V5 ?V7)
           (exist ?V8 ?V4)))

        (bindings-1
         '((?context . ?V1) (?set-1 . ?V2) (?shape-1 . ?V3)
           (?set-2 . ?V4) (?color-1 . ?V5) (color-category . ?V6)
           (red . ?V7) (?target . ?V8)))
        
        (bindings-2
         '((?c . ?V1) (?s1 . ?V2) (?b1 . ?V3)
           (?s2 . ?V4) (.b2 . ?V5) (size-category . ?V6)
           (small . ?V7) (?t . ?V8)))
        
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
         '((get-context ?V1)
           (filter ?V2 ?V1 ?V3)
           (bind shaoe-category ?V3 cube)
           (filter ?V4 ?V2 ?V5)
           (bind color-category ?V5 red)))

        (bindings-1
         '((?context . ?V1) (?set-1 . ?V2) (?shape-1 . ?V3)
           (?set-2 . ?V4) (?color-1 . ?V5)))

        (bindings-2
         '((?c . ?V1) (?s1 . ?V2) (?b1 . ?V3)
           (?s2 . ?V4) (?b2 . ?V5)))

        (delta-1
         '((exist ?target ?set-2)))

        (delta-2
         '((count ?t ?s2))))
    (draw-anti-unification-test-case network-1 network-2 msg bindings-1 bindings-2 delta-1 delta-2)))

;; ############################################################################

;; Substitution example (network with cycle)
;; =========================================

(defparameter *test-case-4*
  (let (;; how many cubes or spheres
        (network-1
         '((get-context ?context)
           (filter ?cube-set ?context ?cube)
           (bind shape-category ?cube cube)
           (filter ?sphere-set ?context ?sphere)
           (bind shape-category ?sphere sphere)
           (union ?set ?cube-set ?sphere-set)
           (count ?target ?set)))

        ;; how many cubes or cylinders
        (network-2
         '((get-context ?c)
           (filter ?s1 ?c ?b1)
           (bind shape-category ?b1 cube)
           (filter ?s2 ?c ?b2)
           (bind shape-category ?b2 cylinder)
           (union ?s3 ?s1 ?s2)
           (count ?t ?s3)))

        ;; how many cubes or X
        (msg
         '((get-context ?V1)
           (filter ?V2 ?V1 ?V3)
           (bind shape-category ?V3 cube)
           (filter ?V4 ?V1 ?V5)
           (bind shape-category ?V5 ?V6)
           (union ?V7 ?V2 ?V4)
           (count ?V8 ?V7)))

        (bindings-1
         '((?context . ?V1) (?cube-set . ?V2) (?cube . ?V3)
           (?sphere-set . ?V4) (?sphere . ?V5) (sphere . ?V6)
           (?set . ?V7) (?target . ?V8)))

        (bindings-2
         '((?c . ?V1) (?s1 . ?V2) (?b1 . ?V3)
           (?s2 . ?V4) (?b2 . ?V5) (cylinder . ?V6)
           (?s3 . ?V7) (?t . ?V8)))

        (delta-1 nil)
        (delta-2 nil))
    (draw-anti-unification-test-case network-1 network-2 msg bindings-1 bindings-2 delta-1 delta-2)))

;; ############################################################################

;; Substitution example (same operators in different order)
;; ========================================================

(defparameter *test-case-8*
  (let (;; how many red large cubes?
        (network-1
         '((get-context ?context)
           (filter ?set-1 ?context ?shape-1)
           (bind shape-category ?shape-1 cube)
           (filter ?set-2 ?set-1 ?size-1)
           (bind size-category ?size-1 large)
           (filter ?set-3 ?set-2 ?color-1)
           (bind color-category ?color-1 red)
           (count ?target ?set-3)))

        ;; how many large red cubes?
        (network-2
         '((get-context ?c)
           (filter ?s1 ?c ?b1)
           (bind shape-category ?b1 cube)
           (filter ?s2 ?s1 ?b2)
           (bind color-category ?b2 red)
           (filter ?s3 ?s2 ?b3)
           (bind size-category ?b3 large)
           (count ?t ?s3)))

        ;; how many X Y cubes?
        (msg
         '((get-context ?V1)
           (filter ?V2 ?V1 ?V3)
           (bind shape-category ?V3 cube)
           (filter ?V4 ?V2 ?V5)
           (filter ?V6 ?V4 ?V7)
           (count ?V8 ?V6)))
        
        (bindings-1
         '((?context . ?V1) (?set-1 . ?V2) (?shape-1 . ?V3)
           (?set-2 . ?V4) (?size-1 . ?V5) (?set-3 . ?V6)
           (?color-1 . ?V7) (?target . ?V8)))
        
        (bindings-2
         '((?c . ?V1) (?s1 . ?V2) (?b1 . ?V3)
           (?s2 . ?V4) (?b2 . ?V5) (?s3 . ?V6)
           (?b3 . ?V7) (?t . ?V8)))
        
        (delta-1
         '((bind size-category ?size-1 large)
           (bind color-category ?color-1 red)))
        
        (delta-2
         '((bind color-category ?b2 red)
           (bind size-category ?b3 large))))
    (draw-anti-unification-test-case network-1 network-2 msg bindings-1 bindings-2 delta-1 delta-2)))

;; ############################################################################

;; Addition example
;; ================

(defparameter *test-case-5*
  (let (;; how many red cubes?
        (network-1
         '((get-context ?context)
           (filter ?set-1 ?context ?shape-1)
           (bind shape-category ?shape-1 cube)
           (filter ?set-2 ?set-1 ?color-1)
           (bind color-category ?color-1 red)
           (count ?target ?set-2)))

        ;; how many large red cubes?
        (network-2 
         '((get-context ?c)
           (filter ?s1 ?c ?b1)
           (bind shape-category ?b1 cube)
           (filter ?s2 ?s1 ?b2)
           (bind color-category ?b2 red)
           (filter ?s3 ?s2 ?b3)
           (bind size-category ?b3 large)
           (count ?t ?s3)))

        ;; how many X red cubes?
        (msg nil)
        (bindings-1 nil)
        (bindings-2 nil)
        (delta-1 nil)
        (delta-2 nil))
    (draw-anti-unification-test-case network-1 network-2 msg bindings-1 bindings-2 delta-1 delta-2)))

;; ############################################################################

;; Deletion example
;; ================

(defparameter *test-case-6*
  (let (;; how many large red cubes?
        (network-1
         '((get-context ?c)
           (filter ?s1 ?c ?b1)
           (bind shape-category ?b1 cube)
           (filter ?s2 ?s1 ?b2)
           (bind color-category ?b2 red)
           (filter ?s3 ?s2 ?b3)
           (bind size-category ?b3 large)
           (count ?t ?s3)))
        
        ;; how many red cubes?
        (network-2
         '((get-context ?context)
           (filter ?set-1 ?context ?shape-1)
           (bind shape-category ?shape-1 cube)
           (filter ?set-2 ?set-1 ?color-1)
           (bind color-category ?color-1 red)
           (count ?target ?set-2)))

        (msg nil)
        (bindings-1 nil)
        (bindings-2 nil)
        (delta-1 nil)
        (delta-2 nil))
    (draw-anti-unification-test-case network-1 network-2 msg bindings-1 bindings-2 delta-1 delta-2)))


;; ############################################################################

;; Substitution example (generalising over previous generalisation)
;; ================================================================

(defparameter *test-case-7*
  (let (;; how many X cubes?
        (network-1
         '((get-context ?context)
           (filter ?set-1 ?context ?shape-1)
           (bind shape-category ?shape-1 cube)
           (filter ?set-2 ?set-1 ?X)
           (count ?target ?set-3)))

        ;; how many X spheres?
        (network-2
         '((get-context ?c)
           (filter ?s1 ?c ?b1)
           (bind shape-category ?b1 sphere)
           (filter ?s2 ?s1 ?Y)
           (count ?t ?s2)))

        ;; how many X Y?
        (msg nil)
        (bindings-1 nil)
        (bindings-2 nil)
        (delta-1 nil)
        (delta-2 nil))
    (draw-anti-unification-test-case network-1 network-2 msg bindings-1 bindings-2 delta-1 delta-2)))


;; ############################################################################

;; Substitution example (addition + not minimal difference)
;; ========================================================

(defparameter *test-case-8*
  (let (;; what color is the large cube?
        (network-1
         '((get-context ?context)
           (filter ?set-1 ?context ?shape-1)
           (bind shape-category ?shape-1 cube)
           (filter ?set-2 ?set-1 ?size-1)
           (bind size-category ?size-1 large)
           (unique ?object-1 ?set-2)
           (query ?target ?object-1 ?attribute-1)
           (bind attribute-category ?attribute-1 color)))

        ;; what color is the small blue sphere?
        (network-2
         '((get-context ?c)
           (filter ?s1 ?c ?b1)
           (bind shape-category ?b1 sphere)
           (filter ?s2 ?s1 ?b2)
           (bind color-category ?b2 blue)
           (filter ?s3 ?s2 ?b3)
           (bind size-category ?b3 small)
           (unique ?o1 ?s3)
           (query ?t ?o1 ?b4)
           (bind attribute-category ?b4 color)))

        ;; what color is the X?
        (msg nil)
        (bindings-1 nil)
        (bindings-2 nil)
        (delta-1 nil)
        (delta-2 nil))
    (draw-anti-unification-test-case network-1 network-2 msg bindings-1 bindings-2 delta-1 delta-2)))


;; ############################################################################

;; Substitution example (not minimal difference, with cycle)
;; =========================================================

(defparameter *test-case-9*
  (let (;; how many cubes or large spheres?
        (network-1
         nil)

        ;; how many small spheres or cylinders?
        (network-2
         nil)

        ;; how many X or Y?
        (msg nil)
        (bindings-1 nil)
        (bindings-2 nil)
        (delta-1 nil)
        (delta-2 nil))
    (draw-anti-unification-test-case network-1 network-2 msg bindings-1 bindings-2 delta-1 delta-2)))







