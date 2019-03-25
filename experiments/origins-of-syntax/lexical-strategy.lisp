(in-package :origins-of-syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing the lexical strategy ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod set-diagnostics-and-repairs ((agent syntax-agent) (mode (eql :lexical-strategy)))
  "Sets the diagnositics and repairs for the n-gram-stragegy: nothing to do."
  (declare (ignore agent mode)))

(defmethod competing-cxns ((cxn fcg-construction) (cxn-inventory fcg-construction-set) (mode (eql :lexical-strategy)))
  "Competing-cxns have the same categories as cxn, but in a different order."
  (declare (ignore cxn cxn-inventory mode)))

(defmethod initialize-lexicon ((word-list list) (mode (eql :lexical-strategy)))
  "Creates and returns a construction inventory with lexical constructions for word-list."
  (let* ((grammar-name (make-const "SYNTAX-GRAMMAR"))
         (cxn-inventory
          (eval `(def-fcg-constructions ,grammar-name
                   :cxn-inventory ,grammar-name
                   :feature-types ((args sequence)
                                   (form set-of-predicates)
                                   (meaning set-of-predicates)
                                   (subunits set)
                                   (footprints set))
                   :fcg-configurations ((:production-goal-tests :no-meaning-in-root)
                                        (:parse-goal-tests :no-strings-in-root :single-interpretation-in-world-comprehension)
                                        (:cxn-supplier-mode . :ordered-by-label-nr-of-categories-and-score)
                                        (:create-initial-structure-mode . :root-with-redundant-meaning)
                                        (:production-order lex morph)
                                        (:max-nr-of-nodes . 2500)
                                        )

                (def-fcg-cxn ,(make-symbol (upcase "combination-3-cxn"))
                             ((?combination-unit
                               (args (?x))
                               (unit-type combination)
                               (subunits (?lex-1-unit ?lex-2-unit ?lex-3-unit))
                               (footprints (NOT combination-cxn)))
                              (?lex-1-unit
                               (footprints (combination-cxn)))
                              (?lex-2-unit
                               (footprints (combination-cxn)))
                              (?lex-3-unit
                               (footprints (combination-cxn)))
                              <-
                              (?lex-1-unit
                               (args (?x))
                               (syn-cat (lex-class ?syn-cat-1))
                               (footprints (NOT combination-cxn))
                               --
                               (syn-cat (lex-class ?syn-cat-1))
                               (footprints (NOT combination-cxn)))
                              (?lex-2-unit
                               (args (?x))
                               (syn-cat (lex-class ?syn-cat-2))
                               (footprints (NOT combination-cxn))
                               --
                               (syn-cat (lex-class ?syn-cat-2))
                               (footprints (NOT combination-cxn)))
                              (?lex-3-unit
                               (args (?x))
                               (syn-cat (lex-class ?syn-cat-3))
                               (footprints (NOT combination-cxn))
                               --
                               (syn-cat (lex-class ?syn-cat-3))
                               (footprints (NOT combination-cxn)))))
                
                (def-fcg-cxn ,(make-symbol (upcase "combination-2-cxn"))
                             ((?combination-unit
                               (args (?x))
                               (unit-type combination)
                               (subunits (?lex-1-unit ?lex-2-unit))
                               (footprints (NOT combination-cxn)))
                              (?lex-1-unit
                               (footprints (combination-cxn)))
                              (?lex-2-unit
                               (footprints (combination-cxn)))
                              <-
                              (?lex-1-unit
                               (args (?x))
                               (unit-type word)
                               (footprints (NOT combination-cxn))
                               --
                               (unit-type word)
                               (footprints (NOT combination-cxn)))
                              (?lex-2-unit
                               (args (?x))
                               (unit-type word)
                               (footprints (NOT combination-cxn))
                               --
                               (unit-type word)
                               (footprints (NOT combination-cxn))))))
                

                ))) ;;to add
    
    (add-words cxn-inventory word-list)))


(defmethod find-same-cxn ((cxn fcg-construction) (cxn-inventory fcg-construction-set) (mode (eql :lexical-strategy)))
  "Two cxns are the same if they have the same name."
  (find-cxn cxn cxn-inventory :key #'name :test #'string=))