(ql:quickload :amr)
(in-package :amr)

(defparameter *my-sentences* (list
                            '(j / Joe)
                            '(c / child)
                            '(c / child :arg-0-of (f / foolish.01))
                            '(b / book)
                            '(a / air)
                            '(b / book)
                            '(c / child :arg0-of (f / foolish :domain (I / I)))
                            '(c / child :arg0-of (f / foolish :domain (I / I)))
                            '(have-org-role-91 :arg0 (I / I) :arg1 (c / committee) :arg2 (c2 / chairman))
                            '(have-org-role-91 :arg0 (I / I) :arg1 (c / club) :arg2 (c2 / president))
                            '(r / remove.01 :arg0 (i / I) :arg1 (b / bottle) :arg2 (b2 /book))
                            '(c / cousin :horizontal-degree (s / second) :relation (r / removed) :vertical-degree (o / once))
                            '(g / give.01 :arg0 (s / she) :arg1 (b / book) :arg2 (j / Joe))
                            ;;':condition (s / she :arg1-of (b / be-located-at-91 :arg2 (h / here)
                            '(l / lie.08 :arg0 (I / I) :polarity (a /amr-unknown))
                            '(s / she :arg1-of (b / be-located-at-91 :arg2 (h / here) :polarity (a / amr-unknown)))
                            '(l / lie.08 :arg0 (I / I) :polarity (a / amr-unknown))
                            '(l / lie.08 :arg0 (I / I) :location (a / amr-unknown))
                            '(s / she :arg1-of (b / be-located-at-91 :arg2 (h / here) :cause (a / amr-unknown)))
                            '(c / contribute.01 :arg0 (s / she) :arg2 (b / book))
                            '(w / worth.02 :arg1 (s / she) :arg2 (k / know.01))
                            '(d / do.02 :arg0 (s / she) :arg1 (b / best :poss s))
                            '(c / correlate-91 :arg1 (m / more :arg3-of (h / have-degree-91 :arg1 (l / learn.01 :arg0 (y / you) :arg1 (p / pronounce.01 :arg3 (n / name :poss (s / she)) :manner (a / amr-unknown))) :arg2 (s / soon.01 :arg1 m))) :arg2 (m3 / more :arg3-of (h2 / have-degree-91 :arg1 (l / likely.01 :arg1 (g / go-out.34 :arg0 (s / she) :arg1 (y / you))))))))

(loop for sentence in *my-sentences*
   do
     (setf *object-in* (penman->object sentence))
     (setf *predicates* (object->predicates *object-in*))
     (print *predicates*))

(penman->predicates '(r / resign.01 :arg0 (h / have-org-role-91 :arg1 (c / club) :arg2 (p /president))))
;; ((RESIGN.01 R) (HAVE-ORG-ROLE-91 H) (CLUB C) (NIL P) (:ARG0 R H) (:ARG1 H C) (:ARG2 H P))

(print (penman->predicates '(h / have-org-role-91 :arg0 (s / she) :arg1 (c / club) :arg2 (m / member))))
;;((REMOVE.01 R) (I I) (BOTTLE B) (BOOK B2) (:ARG0 R I) (:ARG1 R B) (:ARG2 R B2))

(print (penman->predicates '(v / vote.01 :arg0 (s / she) :condition (s :arg1-of (b / be-located-at-91 :arg2 (h / here))))))
