(in-package :fcg)

;; (ql:quickload :fcg)

(def-fcg-constructions sandbox-grammar-1
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

(progn
(def-fcg-cxn what-color-is-the-cube?-cxn-1
             ((?holistic-unit
               (category what-color-is-the-cube?-cxn-cat-1)
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
               (category what-slot-1-is-the-cube?-cxn-cat-1)
               (meaning-args (?target))
               (form-args (?left-1 ?right-2))
               (subunits (?slot-1)))
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
               (category what-slot-1-is-the-cube?-slot-1-cat-1)
               (meaning-args (?attribute-1))
               --
               (form-args (?right-1 ?left-2))
               (category what-slot-1-is-the-cube?-slot-1-cat-1))))

 
(def-fcg-cxn color-cxn-1
             ((?holistic-unit
               (category color-cxn-cat-1)
               (form-args (?left-1 ?right-1))
               (meaning-args (?attribute-1)))
              <-
              (?holistic-unit
               (HASH meaning ((bind attribute-category ?attribute-1 color)))
               --
               (HASH form ((sequence "color" ?left-1 ?right-1))))))

(def-fcg-cxn color-cxn-2
             (<-
              (?holistic-unit
               (HASH meaning ((bind attribute-category ?attribute-1 color)))
               (meaning-args (?attribute-1))
               (category color-cxn-cat-1)
               --
               (category color-cxn-cat-1)
               (form-args (?left-1 ?right-1))
               (HASH form ((sequence "color" ?left-1 ?right-1))))))

(def-fcg-cxn size-cxn-1
             ((?holistic-unit
               (category size-cxn-cat-1)
               (form-args (?left-1 ?right-1))
               (meaning-args (?attribute-1)))
              <-
              (?holistic-unit
               (HASH meaning ((bind attribute-category ?attribute-1 size)))
               --
               (HASH form ((sequence "size" ?left-1 ?right-1))))))

(def-fcg-cxn cube-cxn-1
               ((?holistic-unit
                 (category cube-cxn-cat-1)
                 (form-args (?left-1 ?right-1))
                 (meaning-args (?shape-1)))
                <-
                (?holistic-unit
                 (HASH meaning ((bind shape-category ?shape-1 cube)))
                 --
                 (HASH form ((sequence "cube" ?left-1 ?right-1))))))

(def-fcg-cxn cube-cxn-2
               (<-
                (?holistic-unit
                 (category cube-cxn-cat-1)
                 (meaning-args (?shape-1))
                 (HASH meaning ((bind shape-category ?shape-1 cube)))
                 --
                 (category cube-cxn-cat-1)
                 (form-args (?left-1 ?right-1))
                 (HASH form ((sequence "cube" ?left-1 ?right-1))))))

(def-fcg-cxn what-color-is-the-slot-1?-cxn-1
             ((?item-based-unit
               (category what-color-is-the-slot-1?-cxn-cat-1)
               (meaning-args (?target))
               (form-args (?left-1 ?right-2))
               (subunits (?slot-1)))
              <-
              (?item-based-unit
               (HASH meaning ((get-context ?context) ;;werkt direct op de context
                              (filter ?set-1 ?context ?shape-1)
                              (unique ?object-1 ?set-1)
                              (query ?target ?object-1 ?attribute-1)
                              (bind attribute-category ?attribute-1 color)))
               --
               (HASH form ((sequence "what color is the " ?left-1 ?right-1)
                           (sequence "?" ?left-2 ?right-2))))
              (?slot-1
               (category what-color-is-the-slot-1?-slot-1-cat-1)
               (meaning-args (?shape-1))
               --
               (form-args (?right-1 ?left-2))
               (category what-color-is-the-slot-1?-slot-1-cat-1))))

(def-fcg-cxn what-color-is-the-slot-1?-cxn-2 
             ((?item-based-unit
               (category what-color-is-the-slot-1?-cxn-cat-2)
               (meaning-args (?target ?context))
               (form-args (?left-1 ?right-2))
               (subunits (?slot-1)))
              <-
              (?item-based-unit
               (HASH meaning ((get-context ?input-set)
                              (unique ?object-1 ?output-set)
                              (query ?target ?object-1 ?attribute-1)
                              (bind attribute-category ?attribute-1 color)))
               --
               (HASH form ((sequence "what color is the " ?left-1 ?right-1)
                           (sequence "?" ?left-2 ?right-2))))
              (?slot-1
               (category what-color-is-the-slot-1?-slot-1-cat-2)
               (meaning-args (?output-set ?input-set))
               --
               (form-args (?right-1 ?left-2))
               (category what-color-is-the-slot-1?-slot-1-cat-2))))

(def-fcg-cxn red-cxn-1
             ((?holistic-unit
               (category red-cxn-cat-1)
               (form-args (?left-1 ?right-1))
               (meaning-args (?color-1)))
              <-
              (?holistic-unit
               (HASH meaning ((bind color-category ?color-1 red)))
               --
               (HASH form ((sequence "red" ?left-1 ?right-1))))))

(def-fcg-cxn rubber-cxn-1
             ((?holistic-unit
               (category rubber-cxn-cat-1)
               (form-args (?left-1 ?right-1))
               (meaning-args (?material-1)))
              <-
              (?holistic-unit
               (HASH meaning ((bind material-category ?material-1 rubber)))
               --
               (HASH form ((sequence "rubber" ?left-1 ?right-1))))))

(def-fcg-cxn slot-1-slot-2-cxn-1 ;;red cube
               ((?item-based-unit
                 (category slot-1-slot-2-cxn-cat-1)
                 (meaning-args (?output-set ?input-set))
                 (form-args (?left-1 ?right-2))
                 (subunits (?slot-1 ?slot-2)))
                <-
                (?slot-1
                 (meaning-args (?slot-1-arg-1))
                 (category slot-1-slot-2-slot-1-cat-1)
                 --
                 (form-args (?left-1 ?right-1))
                 (category slot-1-slot-2-slot-1-cat-1))
                (?slot-2
                 (meaning-args (?slot-2-arg-1))
                 (category slot-1-slot-2-slot-2-cat-1)
                 --
                 (form-args (?left-2 ?right-2))
                 (category slot-1-slot-2-slot-2-cat-1))
                (?item-based-unit
                 (HASH meaning ((filter ?slot-2-filtered-set ?input-set ?slot-2-arg-1)
                                (filter ?output-set ?slot-2-filtered-set ?slot-1-arg-1)))
                  --
                  (HASH form ((sequence " " ?right-1 ?left-2))))))


(def-fcg-cxn what-slot-1-is-the-slot-2?-cxn-1
             ((?item-based-unit
               (category what-slot-1-is-the-slot-2?-cxn-cat-1)
               (meaning-args (?target))
               (form-args (?left-1 ?right-3))
               (subunits (?slot-1 ?slot-2)))
              <-
              (?item-based-unit
               (HASH meaning ((get-context ?context) ;;werkt direct op de context
                              (filter ?set-1 ?context ?shape-1)
                              (unique ?object-1 ?set-1)
                              (query ?target ?object-1 ?attribute-1)))
               --
               (HASH form ((sequence "what " ?left-1 ?right-1)
                           (sequence " is the " ?left-2 ?right-2)
                           (sequence "?" ?left-3 ?right-3))))
              (?slot-1
               (category what-slot-1-is-the-slot-2?-slot-1-cat-1)
               (meaning-args (?attribute-1))
               --
               (form-args (?right-1 ?left-2))
               (category what-slot-1-is-the-slot-2?-slot-1-cat-1))
              (?slot-2
               (category what-slot-1-is-the-slot-2?-slot-2-cat-1)
               (meaning-args (?shape-1))
               --
               (form-args (?right-2 ?left-3))
               (category what-slot-1-is-the-slot-2?-slot-2-cat-1))))


(def-fcg-cxn what-slot-1-is-the-slot-2?-cxn-2 ;;GELEERD VAN TS
             ((?item-based-unit
               (category what-slot-1-is-the-slot-2?-cxn-cat-2)
               (meaning-args (?target))
               (form-args (?left-1 ?right-3))
               (subunits (?slot-1 ?slot-2)))
              <-
              (?item-based-unit
               (HASH meaning ((get-context ?input-set)
                              (unique ?object-1 ?output-set)
                              (query ?target ?object-1 ?attribute-1)))
               --
               (HASH form ((sequence "what " ?left-1 ?right-1)
                           (sequence " is the " ?left-2 ?right-2)
                           (sequence "?" ?left-3 ?right-3))))
              (?slot-1
               (category what-slot-1-is-the-slot-2?-slot-1-cat-2)
               (meaning-args (?attribute-1))
               --
               (form-args (?right-1 ?left-2))
               (category what-slot-1-is-the-slot-2?-slot-1-cat-2))
              (?slot-2
               (category what-slot-1-is-the-slot-2?-slot-2-cat-2)
               (meaning-args (?output-set ?input-set))
               --
               (form-args (?right-2 ?left-3))
               (category what-slot-1-is-the-slot-2?-slot-2-cat-2))))


(def-fcg-cxn what-slot-1-is-the-slot-2?-cxn-3 ;; slot units are created by the item-based cxn
             ((?item-based-unit
               (category what-slot-1-is-the-slot-2?-cxn-cat-1)
               (meaning-args (?target))
               (form-args (?left-1 ?right-3))
               (subunits (?slot-1 ?slot-2)))
              (?slot-1
               (category what-slot-1-is-the-slot-2?-slot-1-cat-1)
               (meaning-args (?attribute-1))
               (form-args (?right-1 ?left-2)))
              (?slot-2
               (category what-slot-1-is-the-slot-2?-slot-2-cat-1)
               (meaning-args (?shape-1))
               (form-args (?right-2 ?left-3)))
              <-
              (?item-based-unit
               (HASH meaning ((get-context ?context) ;;werkt direct op de context
                              (filter ?set-1 ?context ?shape-1)
                              (unique ?object-1 ?set-1)
                              (query ?target ?object-1 ?attribute-1)))
               --
               (HASH form ((sequence "what " ?left-1 ?right-1)
                           (sequence " is the " ?left-2 ?right-2)
                           (sequence "?" ?left-3 ?right-3))))))

)

(add-categories '(what-color-is-the-cube?-cxn-cat-1
                  what-slot-1-is-the-cube?-cxn-cat-1
                  what-slot-1-is-the-cube?-slot-1-cat-1
                  what-color-is-the-slot-1?-cxn-cat-1
                  what-color-is-the-slot-1?-slot-1-cat-1
                  color-cxn-cat-1
                  cube-cxn-cat-1
                  red-cxn-cat-1
                  what-color-is-the-slot-1?-cxn-cat-2
                  what-color-is-the-slot-1?-slot-1-cat-2
                  slot-1-slot-2-cxn-cat-1
                  slot-1-slot-2-slot-1-cat-1
                  slot-1-slot-2-slot-2-cat-1
                  what-slot-1-is-the-slot-2?-cxn-cat-1
                  what-slot-1-is-the-slot-2?-slot-1-cat-1
                  what-slot-1-is-the-slot-2?-slot-2-cat-1
                  what-slot-1-is-the-slot-2?-cxn-cat-2
                  what-slot-1-is-the-slot-2?-slot-1-cat-2
                  what-slot-1-is-the-slot-2?-slot-2-cat-2
                  size-cxn-cat-1
                  rubber-cxn-cat-1)
                *fcg-constructions*)

(progn
  (add-link 'color-cxn-cat-1 'what-slot-1-is-the-cube?-slot-1-cat-1 *fcg-constructions*)
  (add-link 'cube-cxn-cat-1 'what-color-is-the-slot-1?-slot-1-cat-1 *fcg-constructions*)
  (add-link 'red-cxn-cat-1 'slot-1-slot-2-slot-1-cat-1 *fcg-constructions*)
  (add-link 'cube-cxn-cat-1 'slot-1-slot-2-slot-2-cat-1 *fcg-constructions*)
  (add-link 'slot-1-slot-2-cxn-cat-1 'what-color-is-the-slot-1?-slot-1-cat-2 *fcg-constructions*)
  (add-link 'color-cxn-cat-1 'what-slot-1-is-the-slot-2?-slot-1-cat-1 *fcg-constructions*)
  (add-link 'cube-cxn-cat-1 'what-slot-1-is-the-slot-2?-slot-2-cat-1 *fcg-constructions*)
  (add-link 'color-cxn-cat-1 'what-slot-1-is-the-slot-2?-slot-1-cat-2 *fcg-constructions*)
  (add-link 'slot-1-slot-2-cxn-cat-1 'what-slot-1-is-the-slot-2?-slot-2-cat-2 *fcg-constructions*)
  (add-link 'size-cxn-cat-1 'what-slot-1-is-the-slot-2?-slot-1-cat-1 *fcg-constructions*)
  (add-link 'size-cxn-cat-1 'what-slot-1-is-the-cube?-slot-1-cat-1 *fcg-constructions*)
  (add-link 'size-cxn-cat-1 'what-slot-1-is-the-slot-2?-slot-1-cat-2 *fcg-constructions*)
  (add-link 'rubber-cxn-cat-1 'slot-1-slot-2-slot-2-cat-1 *fcg-constructions*)
  (add-link 'rubber-cxn-cat-1 'slot-1-slot-2-slot-1-cat-1 *fcg-constructions*)
  (add-link 'slot-1-slot-2-cxn-cat-1 'slot-1-slot-2-slot-1-cat-1 *fcg-constructions*)
  (add-link 'slot-1-slot-2-cxn-cat-1 'slot-1-slot-2-slot-2-cat-1 *fcg-constructions*)
  )

;; (activate-monitor trace-fcg)

;; (comprehend-all "what color is the cube?")
;; (comprehend-all "what size is the cube?")

(comprehend-all "red rubber cube")
(comprehend-all "rubber cube")
;;(comprehend-all "what color is the red cube?")
;;(comprehend-all "what size is the red cube?")

(comprehend-all "what size is the red rubber cube?")


