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

;; Deletion
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

;; Addition
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