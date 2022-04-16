;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fcg referent grammar ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; key insights: production is top down, comprehension is bottom up
;; argument structure cxns go first in production
;; information structure depends the topic of the frame


;;(ql:quickload :fcg)

(in-package :fcg)

;(configure-grammar)
(def-fcg-constructions referent-grammar
  :hashed nil
  :feature-types ((subunits set)
                  (args set)
                  (footprints set)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
                  (boundaries default))
  :hierarchy-features (subunits)
  :diagnostics ()
  :repairs ()
  :fcg-configurations ((:parse-goal-tests :no-applicable-cxns)
                       (:production-goal-tests :no-applicable-cxns)
                       (:node-tests :check-duplicate :restrict-nr-of-nodes :restrict-search-depth)
                       (:max-number-of-nodes . 500)
                       (:max-search-depth . 50)
                       (:parse-order cxn)
                       (:production-order cxn)
                       (:render-mode . :generate-and-test)
                       (:de-render-mode . :de-render-string-meets)
                       (:use-meta-layer . nil)
                       (:consolidate-repairs . nil)
                       (:shuffle-cxns-before-application . t)
                       (:draw-meaning-as-network . t)
                       (:construction-inventory-processor-mode . :heuristic-search)
                       (:node-expansion-mode . :full-expansion)
                       (:search-algorithm . :best-first)
                       (:heuristic-value-mode . :sum-heuristics-and-parent)
                       (:cxn-supplier-mode . :all-cxns)
                       (:heuristics :nr-of-applied-cxns))
  :visualization-configurations ((:with-search-debug-data . t)
                                 (:remove-empty-units . nil)
                                 (:show-constructional-dependencies . t)
                                 (:labeled-paths . nil)
                                 (:colored-paths . nil)
                                 (:hierarchy-features subunits)
                                 (:selected-hierarchy . subunits)
                                 (:hide-features footprints)
                                 (:select-subfeatures . nil)))

(def-fcg-cxn active-transitive-cxn
    ((?clause-unit
      (referent ?frame-var)
      (subunits (?agent-unit ?event-unit ?patient-unit)))
     (?agent-unit
      (referent ?agent-var)
      (syn-cat (syn-role subject))
      (sem-cat (sem-role agent)
               (sem-class referring-expression)))
     (?event-unit
      (referent ?event-var)
      (syn-cat (lex-class verb))
      (sem-cat (sem-class event)
               (event-type transitive)))
     (?patient-unit
      (referent ?patient-var)
      (syn-cat (syn-role direct-object))
      (sem-cat (sem-role patient)
               (sem-class referring-expression)))
     <-
     (?agent-unit
      (HASH meaning ((?agent-predicate ?agent-var)))
      --
      (syn-cat (number ?nb)
               (person ?p))
      (boundaries (leftmost-unit ?leftmost-agent-unit)
                  (rightmost-unit ?rightmost-agent-unit)))
     (?event-unit
      (HASH meaning ((?event-predicate ?event-var)))
      --
      (syn-cat (number ?nb)
               (person ?p))
      (boundaries (leftmost-unit ?leftmost-event-unit)
                  (rightmost-unit ?rightmost-event-unit)))
     (?patient-unit
      (HASH meaning ((?patient-predicate ?patient-var)))
      --
      (boundaries (leftmost-unit ?leftmost-patient-unit)
                  (rightmost-unit ?rightmost-patient-unit)))
     (?clause-unit
      (HASH meaning ((frame ?frame-var ?event-var)
                     (referent ?frame-var)
                     (arg0 ?event-var ?agent-var)
                     (arg1 ?event-var ?patient-var)))
      --
      (HASH form ((meets ?rightmost-agent-unit ?leftmost-event-unit)
                  (meets ?rightmost-event-unit ?leftmost-patient-unit))))))

(def-fcg-cxn bakt-cxn
    (<-
     (?bakt-unit
      (meaning ((bakken-01 ?b)))
      (referent ?b)
      (syn-cat (lex-class verb)
               (number sg)
               (person 3))
      (boundaries (leftmost-unit ?bakt-unit)
                  (rightmost-unit ?bakt-unit))
      --
      (HASH form ((string ?bakt-unit "bakt"))))))

(def-fcg-cxn brood-cxn
    ((?brood-unit
      (syn-cat (number sg)
               (person 3)))
     <-
     (?brood-unit
      (meaning ((brood ?b)))
      (referent ?b)
      (boundaries (leftmost-unit ?brood-unit)
                  (rightmost-unit ?brood-unit))
      --
      (HASH form ((string ?brood-unit "brood"))))))

(def-fcg-cxn bart-cxn
    ((?bart-unit
      (syn-cat (number sg)
               (person 3)))
     <-
     (?bart-unit
      (meaning ((bart ?b)))
      (referent ?b)
      (boundaries (leftmost-unit ?bart-unit)
                  (rightmost-unit ?bart-unit))
      --
      (HASH form ((string ?bart-unit "bart"))))))

     

#|'((frame f b1)
  (bakken-01 b1)
  (arg0 b1 b2)
  (arg1 b1 b3)
  (bart b2)
  (brood b3)
  (referent f))|#

;(activate-monitor trace-fcg)
(formulate '((frame f b1)
             (bakken-01 b1)
             (arg0 b1 b2)
             (arg1 b1 b3)
             (bart b2)
             (brood b3)
             (referent f)))

(comprehend "bart bakt brood")

;; (referent f)    => Bart bakt brood, het brood is door Bart gebakken, de gebeurtenis waarbij Bart een brood bakt
;; (referent b1)   => het bakken van brood door Bart
;; (referent b2)   => Bart die brood bakt
;; (referent b3)   => brood door Bart gebakken, brood dat door Bart gebakken werd, door Bart gebakken brood


;; cxn matches on frame, arg0, arg1 and verb sense

'((frame f b1)
  (bakken-01 b1)
  (arg0 f b2)
  (arg1 f b3)
  (bart b2)
  (brood b3)
  (referent f))

C => NPsubj Vtrans Ndo

;; Bart bakt een volkorenbrood

'((frame f b1)
  (bakken-01 b1)
  (arg0 f b2)
  (arg1 f b3)
  (bart b2)
  (brood b3)
  (mod b3 w)
  (whole-grain w)
  (referent f))

C => NPsubj Vtrans Ndo
Ndo => Adj N

;; Ann zag dat Bart een brood bakte

'((frame f1 z)
  (zien-01 z)
  (arg0 f1 a)
  (arg1 f1 f2)
  (ann a)
  (frame f2 b1)
  (bakken-01 b1)
  (arg0 f2 b2)
  (arg1 f2 b3)
  (bart b2)
  (brood b3)
  (referent f1 f1)
  (referent f2 f2))

;; Ann zag Bart die een brood bakte

'((frame f1 z)
  (zien-01 z)
  (arg0 f1 a)
  (arg1 f1 f2)
  (ann a)
  (frame f2 b1)
  (bakken-01 b1)
  (arg0 f2 b2)
  (arg1 f2 b3)
  (bart b2)
  (brood b3)
  (referent f1 f1)
  (referent f2 b2))

;; Ann die Bart die een brood bakt ziet

'((frame f1 z)
  (zien-01 z)
  (arg0 f1 a)
  (arg1 f1 f2)
  (ann a)
  (frame f2 b1)
  (bakken-01 b1)
  (arg0 f2 b2)
  (arg1 f2 b3)
  (bart b2)
  (brood b3)
  (referent f1 a)
  (referent f2 b2))

;; Broodbakker Bart die door Ann gezien wordt

'((frame f1 z)
  (zien-01 z)
  (arg0 f1 a)
  (arg1 f1 f2)
  (ann a)
  (frame f2 b1)
  (bakken-01 b1)
  (arg0 f2 b2)
  (arg1 f2 b3)
  (bart b2)
  (brood b3)
  (ref f2 b2) ;; broodbakker Bart
  (ref f1 f2))

;; De gebeurtenis waarbij Bart een brood bakt, gezien door Ann [was een unicum]

'((frame f1 z)
  (zien-01 z)
  (arg0 f1 a)
  (arg1 f1 f2)
  (ann a)
  (frame f2 b1)
  (bakken-01 b1)
  (arg0 f2 b2)
  (arg1 f2 b3)
  (bart b2)
  (brood b3)
  (ref f2 f2) ;; bart bakt een brood
  (ref f1 f2)) ;; de gebeurtenis waarbij Bart een brood bakt, gezien door Ann [was een unicum]
