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

;;;; Partial analysis by handling anti-unification cases where the
;;;; pattern delta is empty. Maybe the inclusion of args in anti-unification 
;;;; can also help to check if the generalisation is indeed identical to the
;;;; cxn that was used for anti-unification


(progn
  (wi::reset)
  (notify reset-monitors)
  (reset-id-counters)
  (defparameter *experiment*
    (make-instance 'pattern-finding-experiment)))

;;;; Running interactions             

(run-interaction *experiment*)
(run-series *experiment* 10)

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

(setf (corpus *experiment*)
      `(("How many large cubes are there?"
         ,@(fresh-variables
            '((get-context ?context)
              (filter ?set1 ?context ?shape1)
              (bind shape ?shape1 cube)
              (filter ?set2 ?set1 ?size1)
              (bind size ?size1 large)
              (count ?target ?set2))))
        ("How many small balls are there?"
         ,@(fresh-variables
            '((get-context ?context)
              (filter ?set1 ?context ?shape1)
              (bind shape ?shape1 sphere)
              (filter ?set2 ?set1 ?size1)
              (bind size ?size1 small)
              (count ?target ?set2))))))
(run-interaction *experiment*)

(setf (corpus *experiment*)
      `(("How many large cubes are there?"
         ,@(fresh-variables
            '((get-context ?context)
              (filter ?set-1 ?context ?shape-1)
              (bind shape ?shape-1 cube)
              (filter ?set-2 ?set-1 ?size-1)
              (bind size ?size-1 large)
              (count ?target ?set-2))))
        ("How many blue small balls are there?"
         ,@(fresh-variables
            '((get-context ?context)
              (filter ?set-1 ?context ?shape-1)
              (bind shape ?shape-1 sphere)
              (filter ?set-2 ?set-1 ?size-1)
              (bind size ?size-1 small)
              (filter ?set-3 ?set-2 ?color-1)
              (bind color ?color-1 blue)
              (count ?target ?set-3))))))
(run-interaction *experiment*)

(setf (corpus *experiment*)
      `(("How many blue small balls are there?"
         ,@(fresh-variables
            '((get-context ?context)
              (filter ?set-1 ?context ?shape-1)
              (bind shape ?shape-1 sphere)
              (filter ?set-2 ?set-1 ?size-1)
              (bind size ?size-1 small)
              (filter ?set-3 ?set-2 ?color-1)
              (bind color ?color-1 blue)
              (count ?target ?set-3))))
        ("How many large cubes are there?"
         ,@(fresh-variables
            '((get-context ?context)
              (filter ?set-1 ?context ?shape-1)
              (bind shape ?shape-1 cube)
              (filter ?set-2 ?set-1 ?size-1)
              (bind size ?size-1 large)
              (count ?target ?set-2))))))
(run-interaction *experiment*)

;; test cases;
;; partial analysis repair with 2 holistic parts

(setf (question-data *experiment*)
      `(("What shape is the bla?"
         ,@(fresh-variables
            '((get-context ?context-1)
              (filter ?set-1 ?context-1 ?bind-1)
              (bind bla-cat ?bind-1 bla)
              (unique ?object-1 ?set-1)
              (query ?answer-1 ?object-1 ?attribute-1)
              (bind attribute ?attribute-1 shape))))
        ("What color is the bla?" ,@(fresh-variables
                                     '((get-context ?context-1)
                                       (filter ?set-1 ?context-1 ?bind-1)
                                       (bind bla-cat ?bind-1 bla)
                                       (unique ?object-1 ?set-1)
                                       (query ?answer-1 ?object-1 ?attribute-1)
                                       (bind attribute ?attribute-1 color))))
        ("Is there a cube?" ,@(fresh-variables
                               '((get-context ?context)
                                 (filter ?set-1 ?context ?bind)
                                 (bind shape ?bind cube)
                                 (exist ?target ?set-1))))
        ("Is there a sphere?" ,@(fresh-variables
                               '((get-context ?context)
                                 (filter ?set-1 ?context ?bind)
                                 (bind shape ?bind sphere)
                                 (exist ?target ?set-1))))
        ("What color is the sphere?" ,@(fresh-variables
                                        '((get-context ?context)
                                          (filter ?set-1 ?context ?bind)
                                          (bind shape ?bind sphere)
                                          (unique ?object ?set-1)
                                          (query ?answer ?object ?attribute)
                                          (bind attribute ?attribute color))))))

(run-interaction *experiment*)
        

;; partial analysis repair with an item-based cxn

(setf (question-data *experiment*)
      `(("Is there a large blue cube" ,@(fresh-variables
                                         '((get-context ?set-0)
                                           (filter ?set-1 ?set-0 ?shape-1)
                                           (bind shape ?shape-1 cube)
                                           (filter ?set-2 ?set-1 ?color-1)
                                           (bind color ?color-1 blue)
                                           (filter ?set-3 ?set-2 ?size-1)
                                           (bind size ?size-1 large)
                                           (exist ?target ?set-3))))
        ("Is there a small red ball" ,@(fresh-variables
                                           '((get-context ?set-0)
                                             (filter ?set-1 ?set-0 ?shape-1)
                                             (bind shape ?shape-1 sphere)
                                             (filter ?set-2 ?set-1 ?color-1)
                                             (bind color ?color-1 red)
                                             (filter ?set-3 ?set-2 ?size-1)
                                             (bind size ?size-1 small)
                                             (exist ?target ?set-3))))
        ("large blue cylinder" ,@(fresh-variables
                                  '((filter ?set-1 ?set-0 ?shape-1)
                                    (bind shape ?shape-1 cylinder)
                                    (filter ?set-2 ?set-1 ?color-1)
                                    (bind color ?color-1 blue)
                                    (filter ?set-3 ?set-2 ?size-1)
                                    (bind size ?size-1 large))))
        ("What is the large blue cylinder made of?" ,@(fresh-variables
                                                       '((get-context ?set-0)
                                                         (filter ?set-1 ?set-0 ?shape-1)
                                                         (bind shape ?shape-1 cylinder)
                                                         (filter ?set-2 ?set-1 ?color-1)
                                                         (bind color ?color-1 blue)
                                                         (filter ?set-3 ?set-2 ?size-1)
                                                         (bind size ?size-1 large)
                                                         (unique ?obj-1 ?set-3)
                                                         (query ?target ?obj-1 ?attribute-1)
                                                         (bind attribute ?attribute-1 material))))))

(run-interaction *experiment*)
                                                     

;; partial analysis repair with an item-based cxn + holistic

