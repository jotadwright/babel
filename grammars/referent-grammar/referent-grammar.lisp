;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fcg referent grammar ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; key insights: production is top down, comprehension is bottom up
;; argument structure cxns go first in production
;; information structure depends the topic of the frame


'((frame f b1)
  (bakken-01 b1)
  (arg0 b1 b2)
  (arg1 b1 b3)
  (bart b2)
  (brood b3)
  (referent f))

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