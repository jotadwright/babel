(in-package :fcg)

;; (ql:quickload :fcg)

(def-fcg-constructions grammar-1
  :feature-types ((form set-of-predicates :handle-regex-sequences)
                  (meaning set-of-predicates)
                  (form-args sequence)
                  (meaning-args sequence)
                  (subunits set)
                  (footprints set))
  :hashed t
  :fcg-configurations (;; to activate heuristic search
                       (:construction-inventory-processor-mode . :heuristic-search) ;; use dedicated cip
                       (:node-expansion-mode . :full-expansion) ;; always fully expands node immediately
                       (:cxn-supplier-mode . :hashed) ;; use hashing
                       ;; for using heuristics
                       (:search-algorithm . :best-first) ;; :depth-first, :breadth-first
                       (:heuristics :nr-of-applied-cxns :nr-of-units-matched) ;; list of heuristic functions (modes of #'apply-heuristic)
                       (:heuristic-value-mode . :sum-heuristics-and-parent) ;; how to use results of heuristic functions for scoring a node
                      ; (:hash-mode . :hash-string-meaning)
                       (:de-render-mode . :de-render-sequence)
                       (:render-mode . :render-sequences)
                       (:category-linking-mode . :neighbours)
                       (:parse-goal-tests :no-applicable-cxns :connected-semantic-network)))

(def-fcg-cxn what-color-is-the-cube?-cxn-1
             ((?holistic-unit
               (category what-color-is-the-cube?-1)
               (meaning-args (?target))
               (form-args (?left-1 ?right-1)))
              <-
              (?holistic-unit
               (HASH meaning ((get-context ?context)
                              (filter ?set-1 ?context ?shape-1)
                              (bind shape-category ?shape-1 cube)
                              (unique ?object-1 ?set-1)
                              (query ?target ?object-1 ?attribute-1)
                              (bind attribute-category ?attribute-1 color)))
               --
               (HASH form ((sequence "what color is the cube?" ?left-1 ?right-1))))))


(def-fcg-cxn what-slot-1-is-the-cube?-cxn-1
             ((?item-based-unit
               (category what-slot-1-is-the-cube?-cat-1)
               (meaning-args (?target))
               (form-args (?left-1 ?right-2)))
              <-
              (?item-based-unit
               (HASH meaning ((get-context ?context)
                              (filter ?set-1 ?context ?shape-1)
                              (bind shape-category ?shape-1 cube)
                              (unique ?object-1 ?set-1)
                              (query ?target ?object-1 ?attribute-1)))
               --
               (HASH form ((sequence "what " ?left-1 ?right-1)
                           (sequence " is the cube?" ?left-2 ?right-2))))
              (?slot-1
               (category what-slot-1-is-the-cube?-cat-1-1)
               (meaning-args (?attribute-1))
               --
               (form-args (?right-1 ?left-2))
               (category what-slot-1-is-the-cube?-cat-1-1))))

 
(def-fcg-cxn color-cxn-1
             ((?holistic-unit
               (category color-1)
               (form-args (?left-1 ?right-1))
               (meaning-args (?attribute-1)))
              <-
              (?holistic-unit
               (HASH meaning ((bind attribute-category ?attribute-1 color)))
               --
               (HASH form ((sequence "color" ?left-1 ?right-1))))))


(def-fcg-cxn what-color-is-the-slot-1?-cxn-1
             ((?item-based-unit
               (category what-color-is-the-slot-1?-1)
               (meaning-args (?target))
               (form-args (?left-1 ?right-2)))
              <-
              (?item-based-unit
               (HASH meaning ((get-context ?context)
                              (filter ?set-1 ?context ?shape-1)
                              (unique ?object-1 ?set-1)
                              (query ?target ?object-1 ?attribute-1)
                              (bind attribute-category ?attribute-1 color)))
               --
               (HASH form ((sequence "what color is the " ?left-1 ?right-1)
                           (sequence "?" ?left-2 ?right-2))))
              (?slot-1
               (category what-color-is-the-slot-1?-slot-1-1)
               (meaning-args (?shape-1))
               --
               (form-args (?right-1 ?left-2))
               (category what-color-is-the-slot-1?-slot-1-1))))


(def-fcg-cxn cube-cxn
               ((?holistic-unit
                 (category cube-1)
                 (form-args (?left-1 ?right-1))
                 (meaning-args (?shape-1)))
                <-
                (?holistic-unit
                 (HASH meaning ((bind shape-category ?shape-1 cube)))
                 --
                 (HASH form ((sequence "cube" ?left-1 ?right-1))))))


(add-categories '(what-color-is-the-cube?-1
                  what-slot-1-is-the-cube?-cat-1
                  what-slot-1-is-the-cube?-cat-1-1
                  what-color-is-the-slot-1?-1
                  what-color-is-the-slot-1?-slot-1-1
                  color-1
                  cube-1) *fcg-constructions*)

(add-link 'color-1 'what-slot-1-is-the-cube?-cat-1-1 *fcg-constructions*)
(add-link 'cube-1 'what-color-is-the-slot-1?-slot-1-1 *fcg-constructions*)

;; (activate-monitor trace-fcg)
;; (comprehend-all "what color is the cube?")



(def-fcg-cxn what-color-is-the-slot-1?-cxn-2
             ((?item-based-unit
               (category what-color-is-the-slot-1?-1)
               (meaning-args (?target ?context))
               (form-args (?left-1 ?right-2)))
              <-
              (?item-based-unit
               (HASH meaning ((get-context ?context)
                              (filter ?set-1 ?input-set ?shape-1)
                              (unique ?object-1 ?set-1)
                              (query ?target ?object-1 ?attribute-1)
                              (bind attribute-category ?attribute-1 color)))
               --
               (HASH form ((sequence "what color is the " ?left-1 ?right-1)
                           (sequence "?" ?left-2 ?right-2))))
              (?slot-1
               (category what-color-is-the-slot-1?-slot-1-1)
               (meaning-args (?shape-1 ?input-set))
               --
               (form-args (?right-1 ?left-2))
               (category what-color-is-the-slot-1?-slot-1-1))))
  

  


(def-fcg-cxn red-cxn
             ((?holistic-unit
               (category red-1)
               (form-args (?left-1 ?right-1))
               (meaning-args (?color-1)))
              <-
              (?holistic-unit
               (HASH meaning ((bind color-category ?color-1 red)))
               --
               (HASH form ((sequence "red" ?left-1 ?right-1))))))

#|(def-fcg-cxn slot-1-slot-2-cxn-1
               ((?item-based-unit
                 )
                <-
                (?slot-1
                 (meaning-args (?slot-1-arg-1))
                 (category slot-1-slot-2-slot-1-1)
                 --
                 (form-args (?left-1 ?right-1))
                 (category slot-1-slot-2-slot-1-1))
                (?slot-2
                 (meaning-args (?slot-2-arg-1))
                 (category slot-1-slot-2-slot-2-1)
                 --
                 (form-args (?left-2 ?right-2))
                 (category slot-1-slot-2-slot-2-1))
                (?item-based-unit
                 (HASH meaning ((filter ?output-set ?input-set ?slot-2-arg-1)
                 --
                (HASH form ((sequence " " ?right-1 ?left-2)))
                ))))) |#







(defparameter *o-1* '((:form . "What is the color of the cube")
                      (:meaning . ((get-context ?context)
                                   (filter ?set-1 ?context ?shape-1)
                                   (bind shape-category ?shape-1 cube)
                                   (unique ?object-1 ?set-1)
                                   (query ?target ?object-1 ?attribute-1)
                                   (bind attribute-category ?attribute-1 color)))))

(defparameter *o-2* '((:form . "What is the size of the cube")
                      (:meaning . ((get-context ?context)
                                   (filter ?set-1 ?context ?shape-1)
                                   (bind shape-category ?shape-1 cube)
                                   (unique ?object-1 ?set-1)
                                   (query ?target ?object-1 ?attribute-1)
                                   (bind attribute-category ?attribute-1 size)))))


(defun form (ex) (cdr (assoc :form ex)))
(defun meaning (ex) (cdr (assoc :meaning ex)))




(anti-unify-predicate-network (meaning *o-1*) (meaning *o-2*))

