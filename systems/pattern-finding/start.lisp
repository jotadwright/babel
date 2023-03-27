(ql:quickload :pattern-finding)
(in-package :pattern-finding)


(progn
  (deactivate-all-monitors)
  (activate-monitor display-metrics)
  (monitors::activate-monitor fcg::trace-fcg)
  (monitors::activate-monitor pf::print-a-dot-for-each-interaction)
  (activate-monitor summarize-results-after-n-interactions)
  (activate-monitor show-type-hierarchy-after-n-interactions)
  (activate-monitor trace-interactions-in-wi)
  )


;;;; TO DO

;;;; Turn many functions into methods, especially for dealing with
;;;; fcg-construction vs construction; construction-inventory vs fcg-construction-set;
;;;; units vs units??

;;;; Change the way in which grammatical categories for slots are created
;;;; For example: 'what-X-is-the-blue-Y-(XY)' for the 'color-cube-cxn' filling both the X and Y slots!
;;;; and 'what-X-is-the-Y-(X)' for the color-cxn + 'what-X-is-the-Y-(Y) for the blue-cube-cxn
;;;; and 'what-X-is-the-Y-Z-(Y)' for the blue-cxn + 'what-X-is-the-Y-Z-(Z)' for the cube-cxn


(progn
  (wi::reset)
  (notify reset-monitors)
  (reset-id-counters)
  (defparameter *experiment*
    (make-instance 'pattern-finding-experiment
                   :entries `((:meaning-representation . :irl) 
                              (:corpus-files-root . ,(merge-pathnames
                                                      (make-pathname :directory '(:relative "clevr-grammar-learning"))
                                                      cl-user:*babel-corpora*))
                              (:corpus-data-file . ,(make-pathname :directory '(:relative "train")
                                                                   :name "stage-1" :type "jsonl"))
                              (:number-of-samples . nil)
                              (:shuffle-data-p . nil)
                              (:sort-data-p . t)
                              (:remove-duplicate-data-p . t)))))

;;;; Running interactions             

(run-interaction *experiment*)
(run-series *experiment* 6)

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
(remove-cxns-learned-at *experiment* 7)

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

;; test cases;
;; partial analysis repair with 2 holistic parts

(setf (question-data *experiment*)
      `(("What shape is the bla?" ,@(fresh-variables
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

