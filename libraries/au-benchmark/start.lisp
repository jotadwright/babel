(ql:quickload :au-benchmark)
(in-package :au-benchmark)

(defparameter *G1* '((add ?x ?y ?z) (even ?x) (odd ?z) (p ?z)))
(defparameter *G2* '((add ?a ?b ?c) (add ?c ?b ?a) (even ?c) (odd ?a) (p ?c)))

(au-benchmark.msg.kswap-omega:anti-unify-predicate-networks *G1* *G2*)

(defparameter *class-1-G1*
  '((F ?E-3)
    (G ?G-3 ?F-3) (G ?C-3 ?B-3) (G ?G-3 ?E-3)
    (H ?D-3 ?G-3 ?C-3) (H ?E-3 ?G-3 ?A-3) (H ?A-3 ?C-3 ?E-3) (H ?B-3 ?F-3 ?G-3)))

(defparameter *class-1-G2*
  '((F ?ETA-3) (F ?BETA-3) (F ?EPSILON-3)
    (G ?BETA-3 ?ALPHA-3) (G ?ALPHA-3 ?ALPHA-3) (G ?DELTA-3 ?GAMMA-3) (G ?BETA-3 ?GAMMA-3) (G ?EPSILON-3 ?GAMMA-3) (G ?ZETA-3 ?EPSILON-3) (G ?GAMMA-3 ?ALPHA-3)
    (H ?ALPHA-3 ?GAMMA-3 ?BETA-3)))

(defparameter *class-6-G1*
  '((F ?F-69) (F ?C-69) (F ?H-50) (F ?D-69) (F ?B-69) (G ?H-50 ?E-69) (G ?G-59 ?A-69) (G ?C-69 ?D-69) (G ?G-59 ?E-69) (G ?H-50 ?C-69) (G ?D-69 ?G-59) (G ?E-69 ?A-69) (H ?E-69 ?I-32 ?G-59)))

(defparameter *class-6-G2*
  '((F ?EPSILON-69) (F ?ALPHA-69) (F ?DELTA-69) (F ?ZETA-69) (F ?THETA-50) (G ?ZETA-69 ?DELTA-69) (G ?THETA-50 ?BETA-69) (G ?BETA-69 ?ZETA-69) (G ?THETA-50 ?ALPHA-69) (H ?GAMMA-69 ?GAMMA-69 ?IOTA-32) (H ?BETA-69 ?ALPHA-69 ?GAMMA-69)))

;; msg kswap
(time
 (au-benchmark.msg.kswap:anti-unify-predicate-networks
  *class-6-G1* *class-6-G2*
  :k nil :W nil :V nil
  :omega-scope :global))

;; lcg exhaustive
(au-benchmark.lcg.exhaustive:anti-unify-predicate-networks
 *class-6-G1* *class-6-G2*)

;; lcg kswap
(time
(au-benchmark.lcg.kswap:anti-unify-predicate-networks
 *class-6-G1* *class-6-G2*
 :k nil :W nil :V nil
 :omega-scope :global))
;; k=nil -> 1.970
;; k=10 ->  1.981
;; k=8 -> 1.984
;; k=6 -> 0.297
;; k=4 -> 0.026
;; k=2 -> 0.007
;; k=0 -> 0.003

;; lcg exhaustive
(au-benchmark.lcg.exhaustive:anti-unify-predicate-networks
 *class-6-G1* *class-6-G2*)

(print-anti-unification-results
 (exhaustive:anti-unify-predicate-network-exhaustive
  *class-1-G1* *class-1-G2*))

(print-anti-unification-results
 (k-swap:anti-unify-predicate-network-k-swap
  *class-1-G1*  *class-1-G2*
  :k 11 :W nil :V nil))

(print-anti-unification-results
 (exhaustive-injective:anti-unify-predicate-network-exhaustive
  *class-1-G1* *class-1-G2*))

(print-anti-unification-results
 (k-swap-variable-decoupling:anti-unify-predicate-network-k-swap
  *class-1-G1*  *class-1-G2*
  :k 11 :W 1 :V 2
  :omega-scope :global))

(print-anti-unification-results
 (exhaustive:anti-unify-predicate-network-exhaustive
  *class-1-G1*  *class-1-G2*))






;; Why is k-swap with variable decoupling with W > 1 so slow?
(time
 (k-swap-variable-decoupling:anti-unify-predicate-network-k-swap
  (shuffle *class-1-G1*)
  (shuffle *class-1-G2*)
  :k 11 :W 2 :V 1))


0.032
42.523
=> k=11 W=3 V=3 -> 14s (cost 39)
=> k=11 W=1 V=inf -> 0.887s (cost 37)
=> k=11 W=2 V=inf -> 2.3s (cost 39)
=> k=11 W=3 V=inf -> 18s (cost 39)


;; k-swap with variable decoupling and search
(mapcar #'fcg::cost
        (k-swap-variable-decoupling-search:anti-unify-predicate-network-k-swap
         (shuffle *class-1-G1*)
         (shuffle *class-1-G2*)
         :k 11 :W 1 :V 1 :n 10))
;; => takes very long






;;;; TEST CASES

  (loop repeat 10
        do (fcg::print-anti-unification-results
            (anti-unify-predicate-network-k-swap
             (shuffle *G1*)
             (shuffle *G2*)
             :k 5 :W nil)))

  (loop repeat 10
        do (print-anti-unification-results
            (anti-unify-predicate-network-k-swap
             '((p ?a ?a))
             '((p ?c ?d))
             :k 1)))

  (loop repeat 10
        do (fcg::print-anti-unification-results
            (k-swap-variable-decoupling::anti-unify-predicate-network-k-swap
             ;; are there any red cubes?
             (shuffle
              '((get-context ?context)
                (filter ?set-1 ?context ?cube)
                (bind shape-category ?cube cube)
                (filter ?set-2 ?set-1 ?color-1)
                (bind color-category ?color-1 red)
                (exist ?target ?set-2)))
             ;; are there any red spheres?
             (shuffle
              '((get-context ?c)
                (filter ?s1 ?c ?b1)
                (bind shape-category ?b1 sphere)
                (filter ?s2 ?s1 ?b2)
                (bind color-category ?b2 red)
                (exist ?t ?s2)))
             :k 6 :W 1 :V 1)))
  => best is cost 4


  (loop repeat 10
          do (print-anti-unification-results
              (anti-unify-predicate-network-k-swap
               ;; are there any red cubes?
               (shuffle
                '((get-context ?context)
                  (filter ?set-1 ?context ?shape-1)
                  (bind shape-category ?shape-1 cube)
                  (filter ?set-2 ?set-1 ?color-1)
                  (bind color-category ?color-1 red)
                  (exist ?target ?set-2)))
               ;; are there any small cubes?
               (shuffle
                '((get-context ?c)
                  (filter ?s1 ?c ?b1)
                  (bind shape-category ?b1 cube)
                  (filter ?s2 ?s1 ?b2)
                  (bind size-category ?b2 small)
                  (exist ?t ?s2)))
               :k 6 :W 1)))

  ;; Deletion/Addition
  (loop repeat 20
          do (print-anti-unification-results
              (anti-unify-predicate-network-k-swap
               ;; how many cubes are there?
               (shuffle
                '((get-context ?context)
                  (filter ?set-1 ?context ?shape-1)
                  (bind shape-category ?shape-1 cube)
                  (count ?target ?set-1)))
               ;; how many large cubes are there?
               (shuffle
                '((get-context ?c)
                  (filter ?s1 ?c ?b1)
                  (bind shape-category ?b1 cube)
                  (filter ?s2 ?s1 ?b2)
                  (bind size-category ?b2 large)
                  (count ?t ?s2)))
               :k 4 :W 1)))

  ;; Substitution (different final predicate)
  (loop repeat 10
          do (print-anti-unification-results
              (anti-unify-predicate-network-k-swap
               ;; are there any red cubes?
               (shuffle
                '((get-context ?context)
                 (filter ?set-1 ?context ?shape-1)
                 (bind shape-category ?shape-1 cube)
                 (filter ?set-2 ?set-1 ?color-1)
                 (bind color-category ?color-1 red)
                 (exist ?target ?set-2)))
               ;; how many red cubes are there?
               (shuffle
                '((get-context ?c)
                 (filter ?s1 ?c ?b1)
                 (bind shape-category ?b1 cube)
                 (filter ?s2 ?s1 ?b2)
                 (bind color-category ?b2 red)
                 (count ?t ?s2)))
               :k 6 :W 1)))

  ;; Substitution (network with cycle)
  (loop repeat 10
          do (print-anti-unification-results
              (anti-unify-predicate-network-k-swap
               ;; how many cubes or spheres?
               (shuffle
                '((get-context ?context)
                  (filter ?cube-set ?context ?cube)
                  (bind shape-category ?cube cube)
                  (filter ?sphere-set ?context ?sphere)
                  (bind shape-category ?sphere sphere)
                  (union ?set ?cube-set ?sphere-set)
                  (count ?target ?set)))
               ;; how many cubes or cylinders?
               (shuffle
                '((get-context ?c)
                  (filter ?s1 ?c ?b1)
                  (bind shape-category ?b1 cube)
                  (filter ?s2 ?c ?b2)
                  (bind shape-category ?b2 cylinder)
                  (union ?s3 ?s1 ?s2)
                  (count ?t ?s3)))
               :k 7 :W 1)))

  ;; Different order
  (loop repeat 20
        do (fcg::print-anti-unification-results
            (anti-unify-predicate-network-k-swap
             ;; how many red large cubes?
             (shuffle
              '((get-context ?context)
                (filter ?set-1 ?context ?shape-1)
                (bind shape-category ?shape-1 cube)
                (filter ?set-2 ?set-1 ?size-1)
                (bind size-category ?size-1 large)
                (filter ?set-3 ?set-2 ?color-1)
                (bind color-category ?color-1 red)
                (count ?target ?set-3)))
             ;; how many large red cubes?
             (shuffle
              '((get-context ?c)
                (filter ?s1 ?c ?b1)
                (bind shape-category ?b1 cube)
                (filter ?s2 ?s1 ?b2)
                (bind color-category ?b2 red)
                (filter ?s3 ?s2 ?b3)
                (bind size-category ?b3 large)
                (count ?t ?s3)))
             :k 8 :W nil)))

  ;; Generalise over previous generalisation
  (loop repeat 10
        do (print-anti-unification-results
            (anti-unify-predicate-network-k-swap
             ;; how many X cubes?
             (shuffle
              '((get-context ?context)
               (filter ?set-1 ?context ?shape-1)
               (bind shape-category ?shape-1 cube)
               (filter ?set-2 ?set-1 ?X)
               (count ?target ?set-3)))
             ;; how many X spheres?
             (shuffle
              '((get-context ?c)
               (filter ?s1 ?c ?b1)
               (bind shape-category ?b1 sphere)
               (filter ?s2 ?s1 ?Y)
               (count ?t ?s2)))
             :k 5 :W 1)))

  ;; not minimal difference
  (loop repeat 10
        do (print-anti-unification-results
            (anti-unify-predicate-network-k-swap
             ;; what color is the large cube?
             (shuffle
              '((get-context ?context)
                (filter ?set-1 ?context ?shape-1)
                (bind shape-category ?shape-1 cube)
                (filter ?set-2 ?set-1 ?size-1)
                (bind size-category ?size-1 large)
                (unique ?object-1 ?set-2)
                (query ?target ?object-1 ?attribute-1)
                (bind attribute-category ?attribute-1 color)))
             ;; what color is the small blue sphere?
             (shuffle
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
             :k 8 :W 1)))

  (time
   (fcg::print-anti-unification-results
    (k-swap-variable-decoupling::anti-unify-predicate-network-k-swap
     (shuffle (form-constraints-with-meets (split "are there any red cubes" #\space) :variables t))
     (shuffle (form-constraints-with-meets (split "are there any blue cubes" #\space) :variables t))
     :k 9 :W nil :V nil)))
  => cost 6 in 0.004s with W=1 and V=1
  => cost 6 in 0.035s with W=inf and V=inf

 (time
   (fcg::print-anti-unification-results
    (anti-unify-predicate-network
     (shuffle (form-constraints-with-meets (split "are there any red cubes" #\space) :variables t))
     (shuffle (form-constraints-with-meets (split "are there any blue cubes" #\space) :variables t)))))
  
  (loop repeat 20
        do (print-anti-unification-results
            (anti-unify-predicate-network-k-swap
             (shuffle (form-constraints-with-meets (split "are there any red cubes" #\space) :variables t))
             (shuffle (form-constraints-with-meets (split "are there any blue cubes" #\space) :variables t))
             :k 9 :W 1)))

  (time
   (k-swap-variable-decoupling::anti-unify-predicate-network-k-swap
    (shuffle (form-constraints-with-meets (split "What is the material of the big purple object?" #\space) :variables t))
    (shuffle (form-constraints-with-meets (split "What is the color of the large shiny sphere?" #\space) :variables t))
    :k 17 :W 2 :V 2))
  => 0.316 with W=1 and V=1
  => 58s with W=1 and V=inf

  (time
   (fcg::print-anti-unification-results
    (anti-unify-predicate-network
     (shuffle (form-constraints-with-meets (split "What is the material of the big purple object?" #\space) :variables t))
     (shuffle (form-constraints-with-meets (split "What is the color of the large shiny sphere?" #\space) :variables t)))))

  (defparameter *G1-class-2*
    '((f ?A) (f ?C) (f ?F) (g ?C ?G) (g ?I ?E) (g ?I ?F) (h ?A ?A ?C) (h ?B ?F ?D)
      (h ?C ?A ?A) (h ?D ?E? ?C) (h ?F ?A ?C) (h ?F ?E ?H) (h ?G ?G ?B) (h ?G ?G ?I)))
  (defparameter *G2-class-2*
    '((f ?J) (f ?K) (f ?P) (g ?N ?L) (g ?N ?N) (g ?O ?J) (h ?K ?M ?J) (h ?K ?P ?M)))

  (time
   (anti-unify-predicate-network
    (shuffle *G1-class-2*)
    (shuffle *G2-class-2*)))
  
  (time
   (anti-unify-predicate-network-k-swap
    (shuffle *G1-class-2*)
    (shuffle *G2-class-2*)
    :k 8 :W 1))
