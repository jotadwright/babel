(ql:quickload :pattern-finding-old)
(in-package :pattern-finding-old)

(progn
  (deactivate-all-monitors)
  (activate-monitor trace-fcg)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor trace-interactions-in-wi))


(progn
  (deactivate-all-monitors)
  (activate-monitor display-metrics)
  (activate-monitor trace-fcg)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor summarize-results-after-n-interactions)
  (activate-monitor show-type-hierarchy-after-n-interactions)
  (activate-monitor trace-interactions-in-wi))


;;;; TO DO

;;;; Turn many functions into methods, especially for dealing with
;;;; fcg-construction vs construction;
;;;; construction-inventory vs fcg-construction-set;
;;;; units vs units??

;;;; Handle anti-unification cases where the pattern delta is empty.

;;;; Handle the above cases differently from the "regular" cases?
;;;; Do we want the highest cost solution in this case? Do we want the
;;;; solution with the highest scoring cxn in this case?

;;;; Include partial analyses!!! Get the top-args and slot-args from
;;;; the final transient structure before anti-unifying???
;;;; Anti-unify unit per unit to keep cxns with single slot???
;;;; Or make the anti-unification with item-based cxn independant
;;;; of the number of slots; and re-use the partial analysis repairs
;;;; from before?

;;;; Add recursion!


(progn
  (wi::reset)
  (notify reset-monitors)
  (reset-id-counters)
  (defparameter *experiment*
    (make-instance 'pattern-finding-experiment
                   :entries `((:corpus-directory . ,(babel-pathname :directory '("experiments" "grammar-learning" "cooking" "data")))
                              (:corpus-file . ,(make-pathname :name "benchmark-ingredients-uniform" :type "jsonl"))))))

(length (corpus *experiment*))

;;;; Running interactions             

(run-interaction *experiment*)
(run-series *experiment* 262)

;;;; Showing the cxn inventory and categorial network

(defparameter *cxn-inventory* (grammar (first (agents *experiment*))))
(add-element (make-html *cxn-inventory*))
(add-element (make-html (categorial-network (grammar (first (agents *experiment*))))))

;;;; Manually trying out sentences

(comprehend-all "Is there a blue block?"
                :cxn-inventory *cxn-inventory*
                :gold-standard-meaning '((get-context ?context)
                                         (filter ?set-1 ?context ?shape-1)
                                         (bind shape-category ?shape-1 cube)
                                         (filter ?set-2 ?set-1 ?color-1)
                                         (bind color-category ?color-1 blue)
                                         (exist ?target ?set-2)))

;;;; Time travel

(go-back-n-interactions *experiment* 1)
(remove-cxns-learned-at *experiment* 4)

(defun go-back-n-interactions (experiment n)
  (setf (interactions experiment)
        (subseq (interactions experiment) n)))

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


;;;; Changing the order of repairs on the fly

(defparameter *cxn-inventory* (grammar (first (agents *experiment*))))

(loop for repair in (get-repairs *cxn-inventory*)
      do (delete-repair *cxn-inventory* repair))

(loop for repair-name in '(nothing->holistic
                           anti-unify-partial-analysis 
                           anti-unify-cxn-inventory
                           add-categorial-links)
      do (add-repair *cxn-inventory* repair-name))


;;;; Manual input

(progn
  (wi::reset)
  (notify reset-monitors)
  (reset-id-counters)
  (defparameter *experiment*
    (make-instance 'pattern-finding-experiment
                   :entries '((:mode . :testing)))))

;;; Domonstrate anti-unification with an item-based cxn
;;; with more than one slot!
(def-fcg-cxn what-color-is-the-x-y-z-cxn
             ((?item-based-unit
               (syn-cat (phrase-type item-based)
                        (lex-class what-color-is-the-cat-1))
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
               (syn-cat (lex-class what-color-is-the-slot-cat-1))
               --
               (form-args (?x-farg))
               (syn-cat (lex-class what-color-is-the-slot-cat-1)))
              (?y-slot
               (meaning-args (?y-marg))
               (syn-cat (lex-class what-color-is-the-slot-cat-2))
               --
               (form-args (?y-farg))
               (syn-cat (lex-class what-color-is-the-slot-cat-2)))
              (?z-slot
               (meaning-args (?z-marg))
               (syn-cat (lex-class what-color-is-the-slot-cat-3))
               --
               (form-args (?z-farg))
               (syn-cat (lex-class what-color-is-the-slot-cat-3))))
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

;;;; Demonstrate anti-unification with an item-based cxn
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
(remove-cxns-learned-at *experiment* 1)
;; what color is the X Y Z cxn + large metal cube cxn + small rubber ball cxn
;; what A is the X Y Z cxn + size-tiny-matte-cylinder-cxn
;; + color X Y Z cxn + links to large-metal-cube and small-rubber-ball
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
                   (bind attribute ?attr-1 color))))

;;;; Demonstrate anti-unification with a holistic cxn
