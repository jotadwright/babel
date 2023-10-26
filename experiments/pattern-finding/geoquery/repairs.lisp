(in-package :pf-for-sql)

#|(progn
  (deactivate-all-monitors)
  (activate-monitor trace-fcg)
  (activate-monitor print-a-dot-for-each-interaction))|#

(def-fcg-constructions experiment-parameters
  :feature-types ((form set-of-predicates)
                  (meaning set-of-predicates)
                  (form-args sequence)
                  (meaning-args sequence)
                  (subunits set-of-predicates))
  :hashed t
  :fcg-configurations (;; to activate heuristic search
                       (:construction-inventory-processor-mode . :heuristic-search) ;; use dedicated cip
                       (:node-expansion-mode . :full-expansion) ;; always fully expands node immediately
                       (:cxn-supplier-mode . :all-cxns) 
                       ;; for using heuristics
                       (:search-algorithm . :best-first) ;; :depth-first, :breadth-first
                       (:heuristics :nr-of-applied-cxns :nr-of-units-matched) ;; list of heuristic functions (modes of #'apply-heuristic)
                       (:heuristic-value-mode . :sum-heuristics-and-parent) ;; how to use results of heuristic functions for scoring a node
                     ;  (:hash-mode . :hash-sequence-meaning)
                       (:de-render-mode . :de-render-sequence)
                       (:render-mode . :render-sequences)
                       (:category-linking-mode . :neighbours)
                       (:parse-goal-tests :no-applicable-cxns :connected-semantic-network)))


;; First repair : learning a holophrastic contruction ;;

(defun learn-holophrase (form-string meaning-predicates &key (cxn-inventory *fcg-constructions*))
  "Learning a holophrastic cxn"
  ;; First check if this cxn already exists
  ;(unless (find-equivalent-cxn form-string meaning-predicates cxn-inventory)
  
    (let* ((cxn-name (make-const (upcase (mkstr form-string "-cxn"))))
           (cxn-cat (make-const (upcase (mkstr form-string "-cxn-cat"))))
           (meaning-args (irl:get-target-var meaning-predicates))
           (holistic-cxn (make-instance 'fcg-construction ;;TO DO: Alter-ego cxn
                                        :name cxn-name
                                        :contributing-part (list (make-instance 'contributing-unit
                                                                                :name '?holistic-unit
                                                                                :unit-structure `((category ,cxn-cat)
                                                                                                  (form-args (?left-1 ?right-1))
                                                                                                  (meaning-args ,meaning-args))))
                                        :conditional-part (list (make-instance 'conditional-unit
                                                                               :name '?holistic-unit
                                                                               :formulation-lock `((HASH meaning ,meaning-predicates))
                                                                               :comprehension-lock `((HASH form ((sequence ,form-string ?left-1 ?right-1))))))
                                        :cxn-inventory cxn-inventory
                                        :attributes `((:cxn-cat . ,cxn-cat) (:sequence . ,form-string) (:meaning ,meaning-predicates)))))
      (when holistic-cxn
      (add-cxn holistic-cxn cxn-inventory)
      (add-category cxn-cat cxn-inventory)
      
      holistic-cxn)))

