(in-package :pf-for-sql)

;;------------;;
;; HOLOPHRASE ;;
;;------------;;

(defclass unknown-utterance (problem)
  ())

(defclass diagnose-unknown-utterance (diagnostic)
  ((trigger :initform 'fcg:new-node)))

(defclass add-holophrase (repair) 
  ((trigger :initform 'fcg:new-node)))

(defmethod diagnose ((diagnostic diagnose-unknown-words) (node cip-node)
                     &key &allow-other-keys)
  "Diagnose that the fully expanded structure contains untreated strings"
  (when (fully-expanded? node)
    (let ((strings-in-root (get-strings (assoc 'root
                                               (left-pole-structure
                                                (car-resulting-cfs (cipn-car node)))))))
      (when strings-in-root
        (let ((problem (make-instance 'unknown-words)))
          (set-data problem 'strings strings-in-root)
          problem)))))

(defun make-cxn-name (form-string)
  (let* ((parts (uiop:split-string form-string :separator " "))
         (string-name (format nil "~a" (first parts))))
    (pop parts)
    (loop for part in parts
          do (setf string-name (format nil "~a-~a" string-name part)))
    (setf string-name (format nil "~a-cxn" string-name))
    string-name))

;(make-cxn-name "give me the cities in usa")

(defun find-equivalent-cxn (form-string meaning-predicates cxn-inventory)
  (loop for cxn in (constructions cxn-inventory)
        when (and (string= (attr-val cxn :sequence) form-string)
                  (equalp (attr-val cxn :meaning) meaning-predicates))
          return cxn))

(defun learn-holophrase (form-string meaning-predicates agent-grammar)
  "Learning a holophrastic cxn ; takes as argument a form (a question in natural language) and a meaning (a predicate network)."  
    (let* ((cxn-name (make-symbol (make-cxn-name form-string)))
           (cxn-cat (make-symbol (format nil "~a-cat" (make-cxn-name form-string))))
           (holistic-cxn (make-instance 'fcg-construction
                        :name cxn-name
                        :contributing-part (list (make-instance 'contributing-unit
                                                                :name '?holistic-unit
                                                                :unit-structure `((category ,cxn-cat)
                                                                                  (form-args (?left-1 ?right-1))
                                                                                  (meaning-args (irl:get-target-var meaning)))))
                        :conditional-part (list (make-instance 'conditional-unit
                                                               :name '?holistic-unit
                                                               :formulation-lock `((HASH meaning ,meaning-predicates))
                                                               :comprehension-lock `((HASH form ((sequence ,form-string ?left-1 ?right-1))))))
                        :attributes `((:cxn-cat . ,cxn-cat) (:sequence . ,form-string) (:meaning ,@meaning-predicates))
                        :description "A geo construction"
                        :cxn-inventory agent-grammar)))
      ;(unless (find-equivalent-cxn form-string meaning-predicates agent-grammar)
      (add-cxn holistic-cxn agent-grammar)
      (add-category cxn-cat agent-grammar)))

;(learn-holophrase "name all the lakes of us" '((DOT ?COLUMN-1 ?ALIAS-0 ?COLUMN-2) (AS ?FILTER-0 ?TABLE-0 ?ALIAS-0) (FROM ?FILTER-1 ?FILTER-0) (SELECT ?RESULT-0 ?COLUMN-1 ?FILTER-1) (BIND COLUMN ?COLUMN-2 LAKE_NAME) (BIND CONCEPT ?ALIAS-0 LAKEALIAS0) (BIND TABLE ?TABLE-0 LAKE)))

;(learn-holophrase "what is capital of iowa" '((DOT ?COLUMN-1 ?ALIAS-0 ?COLUMN-3) (DOT ?COLUMN-2 ?ALIAS-0 ?COLUMN-4) (EQUALS ?FILTER-0 ?COLUMN-2 ?COMPARATOR-0) (WHERE ?FILTER-1 ?FILTER-0) (AS ?FILTER-2 ?TABLE-0 ?ALIAS-0) (FROM ?FILTER-3 ?FILTER-2) (SELECT ?RESULT-0 ?COLUMN-1 ?FILTER-3 ?FILTER-1) (BIND COLUMN ?COLUMN-4 STATE_NAME) (BIND COLUMN ?COLUMN-3 CAPITAL) (BIND CONCEPT ?COMPARATOR-0 iowa) (BIND CONCEPT ?ALIAS-0 STATEALIAS0) (BIND TABLE ?TABLE-0 STATE)))

;(comprehend "what is capital of iowa" :construction-inventory *fcg-constructions*)