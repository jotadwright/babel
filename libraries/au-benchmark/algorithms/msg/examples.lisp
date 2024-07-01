
(ql:quickload :au-benchmark)
(in-package :au-benchmark.msg.kswap-omega)


#|
(defparameter *G1* '((add ?x ?y ?z) (even ?x) (odd ?z) (p ?z)))
(defparameter *G2* '((add ?a ?b ?c) (add ?c ?b ?a) (even ?c) (odd ?a) (p ?c)))

(k-swap-generalise *G1* *G2* :k 2 :W 2)
(anti-unify-predicate-networks *G1* *G2* :k 2 :W 3)
                                       

;; when counting shared variables
;; the Delta_I of the final pairing is 10

;; when counting shared bindings
;; the Delta_I of the final pairing is 8

(length
 (delta-intern
  '(((P ?Z) (P ?C) ((?Z . ?C)))
    ((EVEN ?X) (EVEN ?C) ((?X . ?C)))
    ((ODD ?Z) (ODD ?A) ((?Z . ?A)))
    ((ADD ?X ?Y ?Z) (ADD ?C ?B ?A) ((?Z . ?A) (?Y . ?B) (?X . ?C))))
  :shared-bindings))

(anti-unify-predicate-sequence
 '((odd ?z) (even ?x) (p ?z) (add ?x ?y ?z))
 '((odd ?a) (even ?c) (p ?c) (add ?c ?b ?a)))
;; => ((ODD #:?A-1) (EVEN #:?B-1) (P #:?C-1) (ADD #:?B-1 #:?D-1 #:?A-1))
;; => 2 links!

;; exhaustive solution
(print-anti-unification-results (au-benchmark.msg.exhaustive:anti-unify-predicate-networks *G1* *G2*))
;; => ((add ?j-1 ?k-1 ?l-1) (even ?j-1) (odd ?l-1) (p ?m-1))
;; => 2 links
|#



#|
(defparameter *class-1-G1*
  '((F ?E-3)
    (G ?G-3 ?F-3) (G ?C-3 ?B-3) (G ?G-3 ?E-3)
    (H ?D-3 ?G-3 ?C-3) (H ?E-3 ?G-3 ?A-3) (H ?A-3 ?C-3 ?E-3) (H ?B-3 ?F-3 ?G-3)))

(defparameter *class-1-G2*
  '((F ?ETA-3) (F ?BETA-3) (F ?EPSILON-3)
    (G ?BETA-3 ?ALPHA-3) (G ?ALPHA-3 ?ALPHA-3) (G ?DELTA-3 ?GAMMA-3) (G ?BETA-3 ?GAMMA-3) (G ?EPSILON-3 ?GAMMA-3) (G ?ZETA-3 ?EPSILON-3) (G ?GAMMA-3 ?ALPHA-3)
    (H ?ALPHA-3 ?GAMMA-3 ?BETA-3)))

(k-swap-generalise (shuffle *class-1-G1*) (shuffle *class-1-G2*) :mode :shared-bindings)


;;;; counting shared variables
(((F ?E-3)           (F ?BETA-3)                   (?E-3 ?BETA-3))
 ((G ?C-3 ?B-3)      (G ?BETA-3 ?ALPHA-3)          (?C-3 ?B-3 ?BETA-3 ?ALPHA-3))
 ((G ?G-3 ?E-3)      (G ?GAMMA-3 ?ALPHA-3)         (?G-3 ?E-3 ?GAMMA-3 ?ALPHA-3))
 ((G ?G-3 ?F-3)      (G ?ALPHA-3 ?ALPHA-3)         (?G-3 ?F-3 ?ALPHA-3 ?ALPHA-3))
 ((H ?B-3 ?F-3 ?G-3) (H ?ALPHA-3 ?GAMMA-3 ?BETA-3) (?B-3 ?F-3 ?G-3 ?ALPHA-3 ?GAMMA-3 ?BETA-3)))
;; => Delta_I = 19 

(anti-unify-predicate-sequence
 '((f ?e) (g ?c ?b) (g ?g ?e) (g ?g ?f) (h ?b ?f ?g))
 '((f ?beta) (g ?beta ?alpha) (g ?gamma ?alpha) (g ?alpha ?alpha) (h ?alpha ?gamma ?beta)))
=> ((F #:?H-14) (G #:?I-14 #:?J-14) (G #:?K-14 #:?L-14) (G #:?M-14 #:?N-14) (H #:?J-14 #:?O-14 #:?P-14))
=> with Delta_I = 19, we obtain only 1 link in the final generalisation :-(

;;;; counting shared bindings
                                                                           
(delta-intern 
 '(((F ?E-3)           (F ?EPSILON-3)                ((?E-3 . ?EPSILON-3)))
   ((G ?C-3 ?B-3)      (G ?ALPHA-3 ?ALPHA-3)         ((?B-3 . ?ALPHA-3) (?C-3 . ?ALPHA-3)))
   ((G ?G-3 ?E-3)      (G ?BETA-3 ?ALPHA-3)          ((?E-3 . ?ALPHA-3) (?G-3 . ?BETA-3)))
   ((G ?G-3 ?F-3)      (G ?BETA-3 ?GAMMA-3)          ((?F-3 . ?GAMMA-3) (?G-3 . ?BETA-3)))
   ((H ?B-3 ?F-3 ?G-3) (H ?ALPHA-3 ?GAMMA-3 ?BETA-3) ((?G-3 . ?BETA-3) (?F-3 . ?GAMMA-3) (?B-3 . ?ALPHA-3))))
 :shared-bindings)
;; => Delta_I = 8/10/12/14
;; => ((?B-3 . ?ALPHA-3) (?G-3 . ?BETA-3) (?F-3 . ?GAMMA-3) (?G-3 . ?BETA-3) (?G-3 . ?BETA-3) (?F-3 . ?GAMMA-3) (?B-3 . ?ALPHA-3)
;;     (?ALPHA-3 . ?B-3) (?BETA-3 . ?G-3) (?GAMMA-3 . ?F-3) (?BETA-3 . ?G-3) (?BETA-3 . ?G-3) (?GAMMA-3 . ?F-3) (?ALPHA-3 . ?B-3))
;; => shared bindings are sparser than shared-variables.. this seems to have as consequence that
;;    there are multiple 1-swap stable generalisations?

;; => can we compute the number of links in the final generalisation from these bindings?
;; => Sum n * (n - 1) / 2 for every binding count, and divide by 2

(anti-unify-predicate-sequence
 '((f ?e) (g ?c ?b) (g ?g ?e) (g ?g ?f) (h ?b ?f ?g))
 '((f ?epsilon) (g ?alpha ?alpha) (g ?beta ?alpha) (g ?beta ?gamma) (h ?alpha ?gamma ?beta)))
=> ((F #:?Q-14)
    (G #:?R-14 #:?S-14)
    (G #:?T-14 #:?U-14)
    (G #:?T-14 #:?V-14)
    (H #:?S-14 #:?V-14 #:?T-14))
=> with Delta_I = 14, we obtain 5 links! the optimal solution!!

;; exhaustive solution
(print-anti-unification-results (au-benchmark.msg.exhaustive:anti-unify-predicate-networks *class-1-G1* *class-1-G2*))
=> ((f ?r-749)
    (g ?u-749 ?v-749)
    (g ?s-749 ?w-749)
    (g ?s-749 ?t-749)
    (h ?v-749 ?t-749 ?s-749))
=> 5 links
|#

(defun number-of-links (network)
  (let ((all-args (mapcar #'mkstr (mappend #'rest network))))
    (loop for (arg . rest) on all-args
          sum (count arg rest :test #'string=))))


#|
(defparameter *class-1-G1*
  '((F ?D-2) (F ?F-2) (F ?E-2) (F ?G-2) (G ?B-2 ?C-2) (G ?F-2 ?B-2) (G ?A-2 ?H-2) (H ?F-2 ?C-2 ?E-2) (H ?A-2 ?B-2 ?B-2) (H ?G-2 ?F-2 ?C-2) (H ?H-2 ?H-2 ?A-2)))

(defparameter *class-1-G2*
  '((F ?EPSILON-2) (F ?ETA-2) (F ?DELTA-2) (F ?THETA-2) (G ?ZETA-2 ?ETA-2) (G ?THETA-2 ?DELTA-2) (G ?GAMMA-2 ?EPSILON-2) (G ?BETA-2 ?GAMMA-2)
    (H ?BETA-2 ?THETA-2 ?EPSILON-2) (H ?EPSILON-2 ?DELTA-2 ?THETA-2) (H ?GAMMA-2 ?ZETA-2 ?ETA-2) (H ?THETA-2 ?DELTA-2 ?BETA-2) (H ?THETA-2 ?DELTA-2 ?DELTA-2)))

(k-swap-generalise (shuffle *class-1-G1*) (shuffle *class-1-G2*) :mode :shared-bindings)

;; counting shared variables
;; => Delta_I = 42 (always)
(((F ?D-2)           (F ?ETA-2)                       (?D-2 ?ETA-2))
 ((F ?E-2)           (F ?EPSILON-2)                   (?E-2 ?EPSILON-2))
 ((F ?G-2)           (F ?DELTA-2)                     (?G-2 ?DELTA-2))
 ((G ?A-2 ?H-2)      (G ?BETA-2 ?GAMMA-2)             (?A-2 ?H-2 ?BETA-2 ?GAMMA-2))
 ((F ?F-2)           (F ?THETA-2)                     (?F-2 ?THETA-2))
 ((G ?B-2 ?C-2)      (G ?GAMMA-2 ?EPSILON-2)          (?B-2 ?C-2 ?GAMMA-2 ?EPSILON-2))
 ((H ?F-2 ?C-2 ?E-2) (H ?BETA-2 ?THETA-2 ?EPSILON-2)  (?F-2 ?C-2 ?E-2 ?BETA-2 ?THETA-2 ?EPSILON-2))
 ((G ?F-2 ?B-2)      (G ?THETA-2 ?DELTA-2)            (?F-2 ?B-2 ?THETA-2 ?DELTA-2))
 ((H ?G-2 ?F-2 ?C-2) (H ?EPSILON-2 ?DELTA-2 ?THETA-2) (?G-2 ?F-2 ?C-2 ?EPSILON-2 ?DELTA-2 ?THETA-2))
 ((H ?A-2 ?B-2 ?B-2) (H ?THETA-2 ?DELTA-2 ?BETA-2)    (?A-2 ?B-2 ?B-2 ?THETA-2 ?DELTA-2 ?BETA-2))
 ((H ?H-2 ?H-2 ?A-2) (H ?THETA-2 ?DELTA-2 ?DELTA-2)   (?H-2 ?H-2 ?A-2 ?THETA-2 ?DELTA-2 ?DELTA-2)))

(anti-unify-predicate-sequence
 '((f ?d) (f ?e) (f ?g) (g ?a ?h) (f ?f) (g ?b ?c) (h ?f ?c ?e) (g ?f ?b) (h ?g ?f ?c) (h ?a ?b ?b) (h ?h ?h ?a))
 '((f ?eta) (f ?epsilon) (f ?delta) (g ?beta ?gamma) (f ?theta) (g ?gamma ?epsilon) (h ?beta ?theta ?epsilon)
   (g ?theta ?delta) (h ?epsilon ?delta ?theta) (h ?theta ?delta ?beta) (h ?theta ?delta ?delta)))
;; => (number-of-links '((F #:?M-5) (F #:?N-5) (F #:?O-5) (G #:?P-5 #:?Q-5) (F #:?R-5) (G #:?S-5 #:?T-5) (H #:?U-5 #:?V-5 #:?N-5) (G #:?R-5 #:?W-5) (H #:?X-5 #:?Y-5 #:?V-5) (H #:?Z-5 #:?W-5 #:?A-6) (H #:?B-6 #:?C-6 #:?D-6)))
;; => 4 links!

;; counting shared bindings
;; => Delta_I = 22/26/28/30/34 (depending on shuffle)
(((F ?D-2)           (F ?ETA-2)                       ((?D-2 . ?ETA-2)))
 ((G ?B-2 ?C-2)      (G ?GAMMA-2 ?EPSILON-2)          ((?C-2 . ?EPSILON-2) (?B-2 . ?GAMMA-2)))
 ((F ?E-2)           (F ?EPSILON-2)                   ((?E-2 . ?EPSILON-2)))
 ((F ?G-2)           (F ?THETA-2)                     ((?G-2 . ?THETA-2)))
 ((F ?F-2)           (F ?DELTA-2)                     ((?F-2 . ?DELTA-2)))
 ((G ?F-2 ?B-2)      (G ?BETA-2 ?GAMMA-2)             ((?B-2 . ?GAMMA-2) (?F-2 . ?BETA-2)))
 ((H ?F-2 ?C-2 ?E-2) (H ?BETA-2 ?THETA-2 ?EPSILON-2)  ((?E-2 . ?EPSILON-2) (?C-2 . ?THETA-2) (?F-2 . ?BETA-2)))
 ((H ?G-2 ?F-2 ?C-2) (H ?THETA-2 ?DELTA-2 ?BETA-2)    ((?C-2 . ?BETA-2) (?F-2 . ?DELTA-2) (?G-2 . ?THETA-2)))
 ((G ?A-2 ?H-2)      (G ?THETA-2 ?DELTA-2)            ((?H-2 . ?DELTA-2) (?A-2 . ?THETA-2)))
 ((H ?H-2 ?H-2 ?A-2) (H ?EPSILON-2 ?DELTA-2 ?THETA-2) ((?A-2 . ?THETA-2) (?H-2 . ?DELTA-2) (?H-2 . ?EPSILON-2)))
 ((H ?A-2 ?B-2 ?B-2) (H ?THETA-2 ?DELTA-2 ?DELTA-2)   ((?B-2 . ?DELTA-2) (?B-2 . ?DELTA-2) (?A-2 . ?THETA-2))))

(anti-unify-predicate-sequence
 '((f ?d) (g ?b ?c) (f ?e) (f ?g) (f ?f) (g ?f ?b) (h ?f ?c ?e) (h ?g ?f ?c) (g ?a ?h) (h ?h ?h ?a) (h ?a ?b ?b))
 '((f ?eta) (g ?gamma ?epsilon) (f ?epsilon) (f ?theta) (f ?delta) (g ?beta ?gamma) (h ?beta ?theta ?epsilon)
   (h ?theta ?delta ?beta) (g ?theta ?delta) (h ?epsilon ?delta ?theta) (h ?theta ?delta ?delta)))
;; => (number-of-links '((F #:?E-6) (G #:?F-6 #:?G-6) (F #:?H-6) (F #:?I-6) (F #:?J-6) (G #:?K-6 #:?F-6) (H #:?K-6 #:?L-6 #:?H-6) (H #:?I-6 #:?J-6 #:?M-6) (G #:?N-6 #:?O-6) (H #:?P-6 #:?O-6 #:?N-6) (H #:?N-6 #:?Q-6 #:?Q-6)))
;; => 10 links!


;; exhaustive solution
(print-anti-unification-results (au-benchmark.msg.exhaustive:anti-unify-predicate-networks *class-1-G1* *class-1-G2*))
(number-of-links
 '((f ?epsilon-2)
   (f ?theta-2)
   (f ?eta-2)
   (f ?delta-2)
   (g ?zeta-2 ?eta-2)
   (g ?theta-2 ?delta-2)
   (g ?gamma-2 ?epsilon-2)
   (h ?theta-2 ?delta-2 ?beta-2)
   (h ?theta-2 ?delta-2 ?delta-2)
   (h ?beta-2 ?theta-2 ?epsilon-2)
   (h ?epsilon-2 ?delta-2 ?theta-2)
   (g ?beta-2 ?gamma-2)
   (h ?gamma-2 ?zeta-2 ?eta-2)))
;; => 46 links!
|#

#|
;; exhaustive anti-unification ran for 12 minutes
;; this version of kswap omega ran for 0.059s
;; shared-variables gave a cost of 32
;; shared-bindings gave a cost of 8
;; the generalisations were equivalent to the exhaustive result!! :-)
  
(print-anti-unification-results
 (anti-unify-predicate-networks
  '((RIGHT-HAND-ARTICULATION ?PALM-UP-17 PALM-UP)
    (RIGHT-HAND-ARTICULATION ?RIVIERE.K-33 RIVIERE.K)
    (RIGHT-HAND-ARTICULATION ?PT-17 PT)
    (LOCATION ?PT-17 MIDDLE)
    (RIGHT-HAND-ARTICULATION ?FS\:MISSOURI-1 FS\:MISSOURI)
    (RIGHT-HAND-ARTICULATION ?DS-33 DS)
    (LOCATION ?DS-33 MIDDLE)
    (HANDSHAPE ?DS-33 1)
    (ORIENTATION ?DS-33 DOWN-OUT)
    (MOVEMENT ?DS-33 IN-OUT)
    (RIGHT-HAND-ARTICULATION ?COMBIEN-17 COMBIEN)
    (RIGHT-HAND-ARTICULATION ?KILOMETRE-17 KILOMETRE)
    (LEFT-HAND-ARTICULATION ?RIVIERE.K-34 RIVIERE.K)
    (LEFT-HAND-ARTICULATION ?DS-34 DS)
    (LOCATION ?DS-34 MIDDLE)
    (HANDSHAPE ?DS-34 1)
    (ORIENTATION ?DS-34 RIGHT-OUT)
    (MEETS ?PALM-UP-17 ?RIVIERE.K-33)
    (MEETS ?RIVIERE.K-33 ?PT-17)
    (MEETS ?PT-17 ?FS\:MISSOURI-1)
    (MEETS ?FS\:MISSOURI-1 ?DS-33)
    (MEETS ?DS-33 ?COMBIEN-17)
    (MEETS ?COMBIEN-17 ?KILOMETRE-17)
    (MEETS ?PALM-UP-17 ?RIVIERE.K-34)
    (COINCIDES-RELATION ?RIVIERE.K-33 ?RIVIERE.K-34 EQUALS)
    (MEETS ?FS\:MISSOURI-1 ?DS-34)
    (COINCIDES-RELATION ?DS-33 ?DS-34 EQUALS))
  '((RIGHT-HAND-ARTICULATION ?PALM-UP-16 PALM-UP)
    (RIGHT-HAND-ARTICULATION ?RIVIERE.K-31 RIVIERE.K)
    (RIGHT-HAND-ARTICULATION ?PT-16 PT)
    (LOCATION ?PT-16 MIDDLE)
    (RIGHT-HAND-ARTICULATION ?FS\:DELAWARE-16 FS\:DELAWARE)
    (RIGHT-HAND-ARTICULATION ?DS-31 DS)
    (LOCATION ?DS-31 MIDDLE)
    (HANDSHAPE ?DS-31 1)
    (ORIENTATION ?DS-31 DOWN-OUT)
    (MOVEMENT ?DS-31 IN-OUT)
    (RIGHT-HAND-ARTICULATION ?COMBIEN-16 COMBIEN)
    (RIGHT-HAND-ARTICULATION ?KILOMETRE-16 KILOMETRE)
    (LEFT-HAND-ARTICULATION ?RIVIERE.K-32 RIVIERE.K)
    (LEFT-HAND-ARTICULATION ?DS-32 DS)
    (LOCATION ?DS-32 MIDDLE)
    (HANDSHAPE ?DS-32 1)
    (ORIENTATION ?DS-32 RIGHT-OUT)
    (MEETS ?PALM-UP-16 ?RIVIERE.K-31)
    (MEETS ?RIVIERE.K-31 ?PT-16)
    (MEETS ?PT-16 ?FS\:DELAWARE-16)
    (MEETS ?FS\:DELAWARE-16 ?DS-31)
    (MEETS ?DS-31 ?COMBIEN-16)
    (MEETS ?COMBIEN-16 ?KILOMETRE-16)
    (MEETS ?PALM-UP-16 ?RIVIERE.K-32)
    (COINCIDES-RELATION ?RIVIERE.K-31 ?RIVIERE.K-32 EQUALS)
    (MEETS ?FS\:DELAWARE-16 ?DS-32)
    (COINCIDES-RELATION ?DS-31 ?DS-32 EQUALS))
  :mode :shared-bindings))

|#
