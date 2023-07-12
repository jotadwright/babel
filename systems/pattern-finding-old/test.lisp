(ql:quickload :pattern-finding-old)
(in-package :pattern-finding-old)

(progn
  (deactivate-all-monitors)
  (activate-monitor trace-fcg)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor trace-interactions-in-wi))

(defun remove-cxns-learned-at (experiment at)
  (let ((learned-at-cxns
         (find-all-if #'(lambda (cxn)
                          (string= (format nil "@~a" at)
                                   (attr-val cxn :learned-at)))
                      (constructions (grammar (learner experiment))))))
    (loop with grammar = (grammar (learner experiment))
          for cxn in learned-at-cxns
          for alter-ego-cxn = (alter-ego-cxn cxn grammar)
          do (delete-cxn (name cxn) grammar :key #'name)
             (delete-cxn (name alter-ego-cxn) grammar :key #'name))))

(defun setup-test-case ()
  (wi::reset)
  (notify reset-monitors)
  (reset-id-counters)
  (let* ((*experiment*
          (make-instance 'pattern-finding-experiment
                         :entries '((:mode . :testing))))
         (*cxn-inventory* (grammar (first (agents *experiment*)))))
    (values *experiment* *cxn-inventory*)))


(defun test-anti-unification-with-item-based-cxn ()
  (multiple-value-bind (*experiment* *cxn-inventory*) (setup-test-case)
    ;;;; Demonstrate anti-unification with an item-based cxn
    ;;;; When anti-unifying with an item-based cxn X,
    ;;;; the pattern delta also becomes an item-based cxn
    ;;;; with as many slots as the original cxn X and
    ;;;; categorial links are added such that these slots
    ;;;; take the same fillers

    ;;;; What-color-is-the-X-cxn + small-rubber-ball-cxn + large-metal-cube-cxn
    ;;;; => anti-unify 'what size is the tiny matte cylinder' with this item-based cxn
    ;;;; => what-X-is-the-Y-cxn + size-tiny-matte-cylinder-cxn + color-X-cxn
    ;;;;    and color-X-cxn takes small-rubber-ball-cxn and large-metal-cube-cxn
    (setf (corpus *experiment*)
          `(("What color is the large metal cube?"
             ,@(fresh-variables
                '((get-context ?context)
                  (filter ?set-1 ?context ?shape-1)
                  (bind shape ?shape-1 cube)
                  (filter ?set-2 ?set-1 ?material-1)
                  (bind material ?material-1 metal)
                  (filter ?set-3 ?set-2 ?size-1)
                  (bind size ?size-1 large)
                  (unique ?obj-1 ?set-3)
                  (query ?tgt ?obj-1 ?attr-1)
                  (bind attribute ?attr-1 color))))
            ("What color is the small rubber ball?"
             ,@(fresh-variables
                '((get-context ?context)
                  (filter ?set-1 ?context ?shape-1)
                  (bind shape ?shape-1 ball)
                  (filter ?set-2 ?set-1 ?material-1)
                  (bind material ?material-1 rubber)
                  (filter ?set-3 ?set-2 ?size-1)
                  (bind size ?size-1 small)
                  (unique ?obj-1 ?set-3)
                  (query ?tgt ?obj-1 ?attr-1)
                  (bind attribute ?attr-1 color))))
            ("What size is the tiny matte cylinder?"
             ,@(fresh-variables
                '((get-context ?context)
                  (filter ?set-1 ?context ?shape-1)
                  (bind shape ?shape-1 cylinder)
                  (filter ?set-2 ?set-1 ?material-1)
                  (bind material ?material-1 rubber)
                  (filter ?set-3 ?set-2 ?size-1)
                  (bind size ?size-1 small)
                  (unique ?obj-1 ?set-3)
                  (query ?tgt ?obj-1 ?attr-1)
                  (bind attribute ?attr-1 size))))))
    (run-interaction *experiment*)
    (run-interaction *experiment*)
    (remove-cxns-learned-at *experiment* 1)
    (run-interaction *experiment*)
    (comprehend-all "What color is the small rubber ball?"
                    :cxn-inventory *cxn-inventory*
                    :gold-standard-meaning
                    (fresh-variables
                     '((get-context ?context)
                       (filter ?set-1 ?context ?shape-1)
                       (bind shape ?shape-1 ball)
                       (filter ?set-2 ?set-1 ?material-1)
                       (bind material ?material-1 rubber)
                       (filter ?set-3 ?set-2 ?size-1)
                       (bind size ?size-1 small)
                       (unique ?obj-1 ?set-3)
                       (query ?tgt ?obj-1 ?attr-1)
                       (bind attribute ?attr-1 color))))))
;(test-anti-unification-with-item-based-cxn)

(defun test-anti-unification-with-item-based-cxn-with-multiple-slots ()
  (multiple-value-bind (*experiment* *cxn-inventory*) (setup-test-case)
    ;;;; Same idea as previous test case, but now anti-unifying with
    ;;;; an item-based cxn that has 3 slots!
    (def-fcg-cxn metal-cxn
                 ((?holistic-unit
                   (form-args (?metal))
                   (meaning-args (?material-1))
                   (category metal-cat-1))
                  <-
                  (?holistic-unit
                   (HASH meaning ((bind material ?material-1 metal)))
                   --
                   (HASH form ((string ?metal "metal")))))
                 :attributes (:label fcg::routine
                              :cxn-type holistic
                              :string "metal"
                              :meaning metal)
                 :score 0.5 
                 :cxn-inventory *cxn-inventory*)
    
    (def-fcg-cxn large-cxn
                 ((?holistic-unit
                   (form-args (?large))
                   (meaning-args (?size-1))
                   (category large-cat-1))
                  <-
                  (?holistic-unit
                   (HASH meaning ((bind size ?size-1 large)))
                   --
                   (HASH form ((string ?large "large")))))
                 :attributes (:label fcg::routine
                              :cxn-type holistic
                              :string "large"
                              :meaning large)
                 :score 0.5 
                 :cxn-inventory *cxn-inventory*)
    
    (def-fcg-cxn cube-cxn
                 ((?holistic-unit
                   (form-args (?cube-1))
                   (meaning-args (?shape-1))
                   (category cube-cat-1))
                  <-
                  (?holistic-unit
                   (HASH meaning ((bind shape ?shape-1 cube)))
                   --
                   (HASH form ((string ?cube-1 "cube")))))
                 :attributes (:label fcg::routine
                              :cxn-type holistic
                              :string "cube"
                              :meaning cube)
                 :score 0.5 
                 :cxn-inventory *cxn-inventory*)
    
    (def-fcg-cxn what-color-is-the-x-y-z-cxn
                 ((?item-based-unit
                   (category what-color-is-the-cat-1)
                   (meaning-args (?target))
                   (form-args nil)
                   (subunits (?x-slot ?y-slot ?z-slot)))
                  <-
                  (?item-based-unit
                   (HASH meaning ((get-context ?context)
                                  (filter ?set-1 ?context ?z-marg)
                                  (filter ?set-2 ?set-1 ?y-marg)
                                  (filter ?set-3 ?set-2 ?x-marg)
                                  (unique ?obj-1 ?set-3)
                                  (query ?target ?obj-1 ?attr-1)
                                  (bind attribute ?attr-1 color)))
                   --
                   (HASH form ((string ?what "what")
                               (string ?color "color")
                               (string ?is "is")
                               (string ?the "the")
                               (meets ?what ?color)
                               (meets ?color ?is)
                               (meets ?is ?the)
                               (meets ?the ?x-farg)
                               (meets ?x-farg ?y-farg)
                               (meets ?y-farg ?z-farg))))
                  (?x-slot
                   (meaning-args (?x-marg))
                   (category what-color-is-the-slot-cat-1)
                   --
                   (form-args (?x-farg))
                   (category what-color-is-the-slot-cat-1))
                  (?y-slot
                   (meaning-args (?y-marg))
                   (category what-color-is-the-slot-cat-2)
                   --
                   (form-args (?y-farg))
                   (category what-color-is-the-slot-cat-2))
                  (?z-slot
                   (meaning-args (?z-marg))
                   (category what-color-is-the-slot-cat-3)
                   --
                   (form-args (?z-farg))
                   (category what-color-is-the-slot-cat-3)))
                 :attributes (:label fcg::routine
                              :cxn-type item-based
                              :string "what"
                              :meaning query)
                 :score 0.5 
                 :cxn-inventory *cxn-inventory*)
    (add-categories '(what-color-is-the-cat-1
                      what-color-is-the-slot-cat-1
                      what-color-is-the-slot-cat-2
                      what-color-is-the-slot-cat-3
                      large-cat-1 metal-cat-1 cube-cat-1)
                    (categorial-network *cxn-inventory*))
    (add-link 'what-color-is-the-slot-cat-1 'large-cat-1 (categorial-network *cxn-inventory*))
    (add-link 'what-color-is-the-slot-cat-2 'metal-cat-1 (categorial-network *cxn-inventory*))
    (add-link 'what-color-is-the-slot-cat-3 'cube-cat-1 (categorial-network *cxn-inventory*))
    (add-element (make-html *cxn-inventory*))
    (add-element (make-html (categorial-network *cxn-inventory*)))
    (setf (corpus *experiment*)
          `(("What size is the tiny matte cylinder?"
             ,@(fresh-variables
                '((get-context ?context)
                  (filter ?set-1 ?context ?shape-1)
                  (bind shape ?shape-1 cylinder)
                  (filter ?set-2 ?set-1 ?material-1)
                  (bind material ?material-1 rubber)
                  (filter ?set-3 ?set-2 ?size-1)
                  (bind size ?size-1 small)
                  (unique ?obj-1 ?set-3)
                  (query ?tgt ?obj-1 ?attr-1)
                  (bind attribute ?attr-1 size))))))
    (run-interaction *experiment*)
    (comprehend-all "What color is the large metal cube?"
                    :cxn-inventory *cxn-inventory*
                    :gold-standard-meaning
                    (fresh-variables
                     '((get-context ?context)
                       (filter ?set-1 ?context ?shape-1)
                       (bind shape ?shape-1 cube)
                       (filter ?set-2 ?set-1 ?material-1)
                       (bind material ?material-1 metal)
                       (filter ?set-3 ?set-2 ?size-1)
                       (bind size ?size-1 large)
                       (unique ?obj-1 ?set-3)
                       (query ?tgt ?obj-1 ?attr-1)
                       (bind attribute ?attr-1 color))))))
;(test-anti-unification-with-item-based-cxn-with-multiple-slots)


(defun test-holistic-partial-analysis ()
  (multiple-value-bind (*experiment* *cxn-inventory*) (setup-test-case)
    ;;;; Manually add the color-cxn and the large-cxn
    ;;;; and observe "what color is the large cube?"
    ;;;; should learn what-Y-is-the-X-cube-cxn (2 slots)
    (def-fcg-cxn color-cxn
                 ((?holistic-unit
                   (form-args (?color))
                   (meaning-args (?attr-1))
                   (category color-cat-1))
                  <-
                  (?holistic-unit
                   (HASH meaning ((bind attribute ?attr-1 color)))
                   --
                   (HASH form ((string ?color "color")))))
                 :attributes (:label fcg::routine
                              :cxn-type holistic
                              :string "color"
                              :meaning color)
                 :score 0.5 
                 :cxn-inventory *cxn-inventory*)
    
    (def-fcg-cxn large-cxn
                 ((?holistic-unit
                   (form-args (?large))
                   (meaning-args (?size-1))
                   (category large-cat-1))
                  <-
                  (?holistic-unit
                   (HASH meaning ((bind size ?size-1 large)))
                   --
                   (HASH form ((string ?large "large")))))
                 :attributes (:label fcg::routine
                              :cxn-type holistic
                              :string "large"
                              :meaning large)
                 :score 0.5 
                 :cxn-inventory *cxn-inventory*)
    
    (add-categories '(color-cat-1 large-cat-1) (categorial-network *cxn-inventory*))
    (add-element (make-html *cxn-inventory*))
    (add-element (make-html (categorial-network *cxn-inventory*)))
    (setf (corpus *experiment*)
          `(("What color is the large cube?"
             ,@(fresh-variables
                '((get-context ?context)
                  (filter ?set-1 ?context ?shape-1)
                  (bind shape ?shape-1 cube)
                  (filter ?set-2 ?set-1 ?size-1)
                  (bind size ?size-1 large)
                  (unique ?obj-1 ?set-2)
                  (query ?tgt ?obj-1 ?attr-1)
                  (bind attribute ?attr-1 color))))))
    (run-interaction *experiment*)
    
    (comprehend-all "What color is the large cube?"
                    :cxn-inventory *cxn-inventory*
                    :gold-standard-meaning
                    (fresh-variables
                     '((get-context ?context)
                       (filter ?set-1 ?context ?shape-1)
                       (bind shape ?shape-1 cube)
                       (filter ?set-2 ?set-1 ?size-1)
                       (bind size ?size-1 large)
                       (unique ?obj-1 ?set-2)
                       (query ?tgt ?obj-1 ?attr-1)
                       (bind attribute ?attr-1 color))))))
;(test-holistic-partial-analysis)


(defun test-holistic-partial-analysis-2 ()
  (multiple-value-bind (*experiment* *cxn-inventory*) (setup-test-case)
    ;;;; Manually add the large-X-cxn and cube-cxn
    ;;;; and observe "what color is the large cube?"
    ;;;; should learn what-color-is-the-X-cxn (1 slot)
    (def-fcg-cxn cube-cxn
                 ((?holistic-unit
                   (form-args (?cube-1))
                   (meaning-args (?shape-1))
                   (category cube-cat-1))
                  <-
                  (?holistic-unit
                   (HASH meaning ((bind shape ?shape-1 cube)))
                   --
                   (HASH form ((string ?cube-1 "cube")))))
                 :attributes (:label fcg::routine
                              :cxn-type holistic
                              :string "cube"
                              :meaning cube)
                 :score 0.5 
                 :cxn-inventory *cxn-inventory*)
    
    (def-fcg-cxn large-x-cxn
                 ((?item-based-unit
                   (category large-x-1-cat-1)
                   (meaning-args (?shape-1 ?size-1))
                   (form-args (?cube-1 ?large-1))
                   (subunits (?slot-unit)))
                  (?slot-unit
                   (footprints (used-as-slot-filler)))
                  <-
                  (?item-based-unit
                   (HASH meaning ((bind size ?size-1 large)))
                   --
                   (HASH form ((string ?large-1 "large"))))
                  (?slot-unit
                   (meaning-args (?shape-1))
                   (category large-x-1-slot-cat-1)
                   (footprints (NOT used-as-slot-filler))
                   --
                   (footprints (NOT used-as-slot-filler))
                   (category large-x-1-slot-cat-1)
                   (form-args (?cube-1))))
                 :attributes (:label fcg::routine
                              :cxn-type item-based
                              :string "large"
                              :meaning large)
                 :score 0.5 
                 :cxn-inventory *cxn-inventory*)
    
    (add-categories '(cube-cat-1 large-x-1-slot-cat-1 large-x-1-cat-1)
                    (categorial-network *cxn-inventory*))
    (add-link 'cube-cat-1 'large-x-1-slot-cat-1
              (categorial-network *cxn-inventory*))
    (add-element (make-html *cxn-inventory*))
    (add-element (make-html (categorial-network *cxn-inventory*)))
    (setf (corpus *experiment*)
          `(("What color is the large cube?"
             ,@(fresh-variables
                '((get-context ?context)
                  (filter ?set-1 ?context ?shape-1)
                  (bind shape ?shape-1 cube)
                  (filter ?set-2 ?set-1 ?size-1)
                  (bind size ?size-1 large)
                  (unique ?obj-1 ?set-2)
                  (query ?tgt ?obj-1 ?attr-1)
                  (bind attribute ?attr-1 color))))))
    (run-interaction *experiment*)
    (comprehend-all "What color is the large cube?"
                    :cxn-inventory *cxn-inventory*
                    :gold-standard-meaning
                    (fresh-variables
                     '((get-context ?context)
                       (filter ?set-1 ?context ?shape-1)
                       (bind shape ?shape-1 cube)
                       (filter ?set-2 ?set-1 ?size-1)
                       (bind size ?size-1 large)
                       (unique ?obj-1 ?set-2)
                       (query ?tgt ?obj-1 ?attr-1)
                       (bind attribute ?attr-1 color))))))
;(test-holistic-partial-analysis-2)


(defun test-item-based-partial-analysis ()
  (multiple-value-bind (*experiment* *cxn-inventory*) (setup-test-case)
    ;;;; Manually add the what-X-is-the-large-cube-cxn
    ;;;; and observe "what color is the large cube?"
    ;;;; should learn the color-cxn (holistic)
    (def-fcg-cxn what-x-is-the-large-cube-cxn-apply-last
                 ((?item-based-unit
                   (category what-is-the-large-cube-1-cat-1)
                   (meaning-args (?target))
                   (form-args ())
                   (subunits (?slot-unit)))
                  (?slot-unit
                   (footprints (used-as-slot-filler)))
                  <-
                  (?item-based-unit
                   (HASH meaning ((get-context ?context)
                                  (filter ?set-1 ?context ?shape-1)
                                  (bind shape ?shape-1 cube)
                                  (filter ?set-2 ?set-1 ?size-1)
                                  (bind size ?size-1 large)
                                  (unique ?obj-1 ?set-2)
                                  (query ?target ?obj-1 ?attr-1)))
                   --
                   (HASH form ((string ?what "what") (string ?is "is")
                               (string ?the "the") (string ?large "large")
                               (string ?cube "cube") (meets ?what ?color)
                               (meets ?color ?is) (meets ?is ?the)
                               (meets ?the ?large) (meets ?large ?cube))))
                  (?slot-unit
                   (meaning-args (?attr-1))
                   (category what-is-the-large-cube-1-slot-cat-1)
                   (footprints (NOT used-as-slot-filler))
                   --
                   (footprints (NOT used-as-slot-filler))
                   (category what-is-the-large-cube-1-slot-cat-1)
                   (form-args (?color))))
                 :attributes (:label fcg::routine
                              :cxn-type item-based
                              :bare-cxn-name what-x-is-the-large-cube-cxn
                              :string "cube"
                              :meaning cube)
                 :score 0.5 
                 :cxn-inventory *cxn-inventory*)
    
    (def-fcg-cxn what-x-is-the-large-cube-cxn-apply-first
                 ((?item-based-unit
                   (category what-is-the-large-cube-1-cat-1)
                   (meaning-args (?target))
                   (form-args ())
                   (subunits (?slot-unit)))
                  (?slot-unit
                   (meaning-args (?attr-1))
                   (form-args (?color))
                   (category what-is-the-large-cube-1-slot-cat-1)
                   (footprints (used-as-slot-filler)))
                  <-
                  (?item-based-unit
                   (HASH meaning ((get-context ?context)
                                  (filter ?set-1 ?context ?shape-1)
                                  (bind shape ?shape-1 cube)
                                  (filter ?set-2 ?set-1 ?size-1)
                                  (bind size ?size-1 large)
                                  (unique ?obj-1 ?set-2)
                                  (query ?target ?obj-1 ?attr-1)))
                   --
                   (HASH form ((string ?what "what") (string ?is "is")
                               (string ?the "the") (string ?large "large")
                               (string ?cube "cube") (meets ?what ?color)
                               (meets ?color ?is) (meets ?is ?the)
                               (meets ?the ?large) (meets ?large ?cube)))))
                 :attributes (:label fcg::meta-only
                              :cxn-type item-based
                              :bare-cxn-name what-x-is-the-large-cube-cxn
                              :string "cube"
                              :meaning cube)
                 :score 0.5 
                 :cxn-inventory *cxn-inventory*)
    
    (add-categories '(what-is-the-large-cube-1-cat-1 what-is-the-large-cube-1-slot-cat-1)
                    (categorial-network *cxn-inventory*))
    (add-element (make-html *cxn-inventory*))
    (add-element (make-html (categorial-network *cxn-inventory*)))
    (setf (corpus *experiment*)
          `(("What color is the large cube?"
             ,@(fresh-variables
                '((get-context ?context)
                  (filter ?set-1 ?context ?shape-1)
                  (bind shape ?shape-1 cube)
                  (filter ?set-2 ?set-1 ?size-1)
                  (bind size ?size-1 large)
                  (unique ?obj-1 ?set-2)
                  (query ?tgt ?obj-1 ?attr-1)
                  (bind attribute ?attr-1 color))))))
    (run-interaction *experiment*)
    (comprehend-all "What color is the large cube?"
                    :cxn-inventory *cxn-inventory*
                    :gold-standard-meaning
                    (fresh-variables
                     '((get-context ?context)
                       (filter ?set-1 ?context ?shape-1)
                       (bind shape ?shape-1 cube)
                       (filter ?set-2 ?set-1 ?size-1)
                       (bind size ?size-1 large)
                       (unique ?obj-1 ?set-2)
                       (query ?tgt ?obj-1 ?attr-1)
                       (bind attribute ?attr-1 color))))))
;(test-item-based-partial-analysis)


(defun test-item-based-partial-analysis-2 ()
  (multiple-value-bind (*experiment* *cxn-inventory*) (setup-test-case)
    ;;;; Manually add the what-X-is-the-Y-cube-cxn
    ;;;; and observe "what color is the large cube?"
    ;;;; should learn the color-cxn and cube-cxn!
    (def-fcg-cxn what-x-is-the-y-cube-cxn-apply-last
                 ((?item-based-unit
                   (category what-is-the-cube-1-cat-1)
                   (meaning-args (?target))
                   (form-args ())
                   (subunits (?slot-unit-1 ?slot-unit-2)))
                  (?slot-unit-1
                   (footprints (used-as-slot-filler)))
                  (?slot-unit-2
                   (footprints (used-as-slot-filler)))
                  <-
                  (?item-based-unit
                   (HASH meaning ((get-context ?context)
                                  (filter ?set-1 ?context ?shape-1)
                                  (bind shape ?shape-1 cube)
                                  (filter ?set-2 ?set-1 ?size-1)
                                  (unique ?obj-1 ?set-2)
                                  (query ?target ?obj-1 ?attr-1)))
                   --
                   (HASH form ((string ?what "what") (string ?is "is")
                               (string ?the "the") (string ?cube "cube") (meets ?what ?color)
                               (meets ?color ?is) (meets ?is ?the)
                               (meets ?the ?large) (meets ?large ?cube))))
                  (?slot-unit-1
                   (meaning-args (?attr-1))
                   (category what-is-the-cube-1-slot-cat-1)
                   (footprints (NOT used-as-slot-filler))
                   --
                   (footprints (NOT used-as-slot-filler))
                   (category what-is-the-cube-1-slot-cat-1)
                   (form-args (?color)))
                  (?slot-unit-2
                   (meaning-args (?size-1))
                   (category what-is-the-cube-1-slot-cat-2)
                   (footprints (NOT used-as-slot-filler))
                   --
                   (footprints (NOT used-as-slot-filler))
                   (category what-is-the-cube-1-slot-cat-2)
                   (form-args (?large))))
                 :attributes (:label fcg::routine
                              :cxn-type item-based
                              :bare-cxn-name what-x-is-the-y-cube-cxn
                              :string "cube"
                              :meaning cube)
                 :score 0.5 
                 :cxn-inventory *cxn-inventory*)
    
    (def-fcg-cxn what-x-is-the-y-cube-cxn-apply-first
                 ((?item-based-unit
                   (category what-is-the-cube-1-cat-1)
                   (meaning-args (?target))
                   (form-args ())
                   (subunits (?slot-unit-1 ?slot-unit-2)))
                  (?slot-unit-1
                   (meaning-args (?attr-1))
                   (form-args (?color))
                   (category what-is-the-cube-1-slot-cat-1)
                   (footprints (used-as-slot-filler)))
                  (?slot-unit-2
                   (meaning-args (?size-1))
                   (form-args (?large))
                   (category what-is-the-cube-1-slot-cat-2)
                   (footprints (used-as-slot-filler)))
                  <-
                  (?item-based-unit
                   (HASH meaning ((get-context ?context)
                                  (filter ?set-1 ?context ?shape-1)
                                  (bind shape ?shape-1 cube)
                                  (filter ?set-2 ?set-1 ?size-1)
                                  (unique ?obj-1 ?set-2)
                                  (query ?target ?obj-1 ?attr-1)))
                   --
                   (HASH form ((string ?what "what") (string ?is "is")
                               (string ?the "the")
                               (string ?cube "cube") (meets ?what ?color)
                               (meets ?color ?is) (meets ?is ?the)
                               (meets ?the ?large) (meets ?large ?cube)))))
                 :attributes (:label fcg::meta-only
                              :cxn-type item-based
                              :bare-cxn-name what-x-is-the-y-cube-cxn
                              :string "cube"
                              :meaning cube)
                 :score 0.5 
                 :cxn-inventory *cxn-inventory*)
    
    (add-categories '(what-is-the-cube-1-cat-1 what-is-the-cube-1-slot-cat-1 what-is-the-cube-1-slot-cat-2)
                    (categorial-network *cxn-inventory*))
    (add-element (make-html *cxn-inventory*))
    (add-element (make-html (categorial-network *cxn-inventory*)))
    (setf (corpus *experiment*)
          `(("What color is the large cube?"
             ,@(fresh-variables
                '((get-context ?context)
                  (filter ?set-1 ?context ?shape-1)
                  (bind shape ?shape-1 cube)
                  (filter ?set-2 ?set-1 ?size-1)
                  (bind size ?size-1 large)
                  (unique ?obj-1 ?set-2)
                  (query ?tgt ?obj-1 ?attr-1)
                  (bind attribute ?attr-1 color))))))
    (run-interaction *experiment*)
    (comprehend-all "What color is the large cube?"
                    :cxn-inventory *cxn-inventory*
                    :gold-standard-meaning
                    (fresh-variables
                     '((get-context ?context)
                       (filter ?set-1 ?context ?shape-1)
                       (bind shape ?shape-1 cube)
                       (filter ?set-2 ?set-1 ?size-1)
                       (bind size ?size-1 large)
                       (unique ?obj-1 ?set-2)
                       (query ?tgt ?obj-1 ?attr-1)
                       (bind attribute ?attr-1 color))))))
;(test-item-based-partial-analysis-2)















