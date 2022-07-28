(ql:quickload :predicate-networks)
(in-package :pn)

;; ############################################################################
;; Test Cases
;; ----------------------------------------------------------------------------

;; Substitution example (same type in bind statement)
(defparameter *pattern-network* ;; are there red cubes?
  '((get-context ?context)
    (filter ?set-1 ?context ?cube)
    (bind shape-category ?cube cube)
    (filter ?set-2 ?set-1 ?color-1)
    (bind color-category ?color-1 red)
    (exist ?target ?set-2)))

(defparameter *source-network* ;; are there red spheres?
  '((get-context ?c)
    (filter ?s1 ?c ?b1)
    (bind shape-category ?b1 sphere)
    (filter ?s2 ?s1 ?b2)
    (bind color-category ?b2 red)
    (exist ?t ?s2)))

(reorder-source-network
 (shuffle *pattern-network*)
 (shuffle *source-network*)
 nil)
;; => 1 solutions
;; 1. every predicate from pattern gets assigned to the correct
;;    predicate from source AND the non-matching bind statements
;;    (i.e. cube and sphere) get assigned to each other

(anti-unify
 (shuffle *pattern-network*)
 (shuffle *source-network*))
;; ==> see web interface

;; LGG (is this what we want?)
;; are there red X?
'((get-context ?context)
  (filter ?set-1 ?context ?Y)
  (?X ?Y)
  (filter ?set-2 ?set-1 ?color-1)
  (bind color-category ?color-1 red)
  (exist ?target ?set-2))
'(((bind shape-category ?Y cube) . ?X) (?cube . ?Y))
'(((bind shape-category ?Y sphere) . ?X) (?b1 . ?Y))

;; ############################################################################

;; Substitution example (different type in bind statement)
(defparameter *pattern-network* ;; are there red cubes?
  '((get-context ?context)
    (filter ?set-1 ?context ?shape-1)
    (bind shape-category ?shape-1 cube)
    (filter ?set-2 ?set-1 ?color-1)
    (bind color-category ?color-1 red)
    (exist ?target ?set-2)))

(defparameter *source-network* ;; are there small cubes?
  '((get-context ?c)
    (filter ?s1 ?c ?b1)
    (bind shape-category ?b1 cube)
    (filter ?s2 ?s1 ?b2)
    (bind size-category ?b2 small)
    (exist ?t ?s2)))

(reorder-source-network
 (shuffle *pattern-network*)
 (shuffle *source-network*)
 nil)
;; => 1 solutions
;; 1. every predicate from pattern gets assigned to the correct
;;    predicate from source AND the non-matching bind statements
;;    (i.e. red and small) get assigned to each other

(anti-unify
 (shuffle *pattern-network*)
 (shuffle *source-network*))
;; ==> see web interface

;; LGG (is this what we want?)
;; are there X cubes?
'((get-context ?context)
  (filter ?set-1 ?context ?shape-1)
  (bind shape-category ?shape-1 cube)
  (filter ?set-2 ?set-1 ?X)
  ?Y
  (exist ?target ?set-2))
'(((bind color-category ?X red) . ?Y)  (?color-1 . ?X))
'(((bind size-category ?X small) . ?Y) (?b2 . ?X))

;; ############################################################################

;; Substitution example (different final predicate)
(defparameter *pattern-network* ;; are there red cubes?
  '((get-context ?context)
    (filter ?set-1 ?context ?shape-1)
    (bind shape-category ?shape-1 cube)
    (filter ?set-2 ?set-1 ?color-1)
    (bind color-category ?color-1 red)
    (exist ?target ?set-2)))

(defparameter *source-network* ;; how many red cubes?
  '((get-context ?c)
    (filter ?s1 ?c ?b1)
    (bind shape-category ?b1 cube)
    (filter ?s2 ?s1 ?b2)
    (bind color-category ?b2 red)
    (count ?t ?s2)))

(reorder-source-network
 (shuffle *pattern-network*)
 (shuffle *source-network*)
 nil)
;; => 1 solutions
;; 1. every predicate from pattern gets assigned to the correct
;;    predicate from source AND the non-matching predicates
;;    (i.e. exist and count) get assigned to each other

(anti-unify
 (shuffle *pattern-network*)
 (shuffle *source-network*))
;; ==> see web interface

;; LGG (is this what we want?)
;; X red cubes?
'((get-context ?context)
  (filter ?set-1 ?context ?shape-1)
  (bind shape-category ?shape-1 cube)
  (filter ?set-2 ?set-1 ?color-1)
  (bind color-category ?color-1 red)
  ?X)
'(((exist ?target ?Y) . ?X) (?set-2 . Y))
'(((count ?target ?Y) . ?X) (?s2 . Y))

;; ############################################################################

;; Substitution example
(defparameter *pattern-network*  ;; how many cubes or spheres
  '((get-context ?context)
    (filter ?cube-set ?context ?cube)
    (bind shape-category ?cube cube)
    (filter ?sphere-set ?context ?sphere)
    (bind shape-category ?sphere sphere)
    (union ?set ?cube-set ?sphere-set)
    (count ?target ?set)))

(defparameter *source-network* ;; how many cubes or cylinders
  '((get-context ?c)
    (filter ?s1 ?c ?b1)
    (bind shape-category ?b1 cube)
    (filter ?s2 ?c ?b2)
    (bind shape-category ?b2 cylinder)
    (union ?s3 ?s1 ?s2)
    (count ?t ?s3)))

(reorder-source-network
 (shuffle *pattern-network*)
 (shuffle *source-network*)
 nil)
;; => 1 solutions
;; 1. every predicate from pattern gets assigned to the correct
;;    predicate from source AND the non-matching bind statements
;;    (i.e. sphere and cylinder) get assigned to each other

(anti-unify
 (shuffle *pattern-network*)
 (shuffle *source-network*))
;; ==> see web interface

;; LGG (is this what we want?)
'((get-context ?context)
  (filter ?cube-set ?context ?cube)
  (bind shape-category ?cube cube)
  (filter ?sphere-set ?context ?Y)
  ?X
  (union ?set ?cube-set ?sphere-set)
  (count ?target ?set))
'(((bind shape-category ?Y sphere) . ?X) (?sphere . ?Y))
'(((bind shape-category ?Y cylinder) . ?X) (?b2 . ?Y))

;; ############################################################################

;; Addition example
(defparameter *pattern-network* ;; how many red cubes?
  '((get-context ?context)
    (filter ?set-1 ?context ?shape-1)
    (bind shape-category ?shape-1 cube)
    (filter ?set-2 ?set-1 ?color-1)
    (bind color-category ?color-1 red)
    (count ?target ?set-2)))

(defparameter *source-network* ;; how many large red cubes?
  '((get-context ?c)
    (filter ?s1 ?c ?b1)
    (bind shape-category ?b1 cube)
    (filter ?s2 ?s1 ?b2)
    (bind color-category ?b2 red)
    (filter ?s3 ?s2 ?b3)
    (bind size-category ?b3 large)
    (count ?t ?s3)))

(reorder-source-network
 *pattern-network*
  *source-network*
 nil)
;; => 2 solutions

;; LGG (is this what we want?)
;; How many X red cubes?
'((get-context ?context)
  (filter ?set-1 ?context ?shape-1)
  (bind shape-category ?shape-1 cube)
  (filter ?Y ?set-1 ?color-1)
  (bind color-category ?color-1 red)
  (?A ?X ?Y ?Z)
  (?B ?Z)
  (count ?target ?X))
'((0 . ?A) (0 . ?B) (0 . ?Z) (?set-2 . ?Y) (?set-2 . ?X))
'(((filter ?X ?Y ?Z) . ?A) ((bind size-category ?Z large) . ?B) (?s2 . ?Y) (?s3 . ?X) (?b3 . ?Z))

;; ############################################################################

;; Deletion example
(defparameter *pattern-network* ;; how many large red cubes?
  '((get-context ?c)
    (filter ?s1 ?c ?b1)
    (bind shape-category ?b1 cube)
    (filter ?s2 ?s1 ?b2)
    (bind color-category ?b2 red)
    (filter ?s3 ?s2 ?b3)
    (bind size-category ?b3 large)
    (count ?t ?s3)))

(defparameter *source-network* ;; how many red cubes?
  '((get-context ?context)
    (filter ?set-1 ?context ?shape-1)
    (bind shape-category ?shape-1 cube)
    (filter ?set-2 ?set-1 ?color-1)
    (bind color-category ?color-1 red)
    (count ?target ?set-2)))

(reorder-source-network
 (shuffle *pattern-network*)
 (shuffle *source-network*)
 nil)
;; => NIL :-(

;; LGG (is this what we want?)
;; How many X red cubes?
'((get-context ?context)
  (filter ?set-1 ?context ?shape-1)
  (bind shape-category ?shape-1 cube)
  (filter ?Y ?set-1 ?color-1)
  (bind color-category ?color-1 red)
  (?A ?X ?Y ?Z)
  (?B ?Z)
  (count ?target ?X))
'((0 . ?A) (0 . ?B) (0 . ?Z) (?set-2 . ?Y) (?set-2 . ?X))
'(((filter ?X ?Y ?Z) . ?A) ((bind size-category ?Z large) . ?B) (?s2 . ?Y) (?s3 . ?X) (?b3 . ?Z))

;; ############################################################################

;; Substitution example with variables
(defparameter *pattern-network* ;; how many X cubes?
  '((get-context ?c)
    (filter ?s1 ?c ?b1)
    (bind shape-category ?b1 cube)
    (filter ?s2 ?s1 ?Y)
    (?X ?Y)
    (count ?t ?s2)))

(defparameter *pattern-network* ;; how many X spheres?
  '((get-context ?c)
    (filter ?s1 ?c ?b1)
    (bind shape-category ?b1 sphere)
    (filter ?s2 ?s1 ?Y)
    (?X ?Y)
    (count ?t ?s2)))

;; LGG (is this what we want?)
;; how many X Y?
'((get-context ?x)
  (filter ?s1 ?c ?A)
  (?Z ?A)
  (filter ?s1 ?s1 ?b2)
  ?X
  (count ?t ?s2))
'(((bind shape-category ?b1 cube) . ?Z) (?b1 . ?A))
'(((bind shape-category ?b1 sphere) . ?Z) (?b1 . ?A))







