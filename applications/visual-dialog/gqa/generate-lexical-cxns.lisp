;(ql:quickload :clevr-dialog-grammar)
(in-package :clevr-dialog-grammar)

#|(def-fcg-constructions dialog-grammar-constructicon
  :feature-types ((args set-of-predicates)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits set)
                  (footprints set))
  :fcg-configurations ((:construction-inventory-processor-mode . :heuristic-search)
                       (:node-expansion-mode . :full-expansion)
                       (:cxn-supplier-mode . :hashed)
                       (:search-algorithm . :best-first)
                       (:hash-mode . :hash-string-meaning-lex-id)
                       (:heuristics :nr-of-applied-cxns :nr-of-units-matched :cxn-score)
                       (:heuristic-value-mode . :sum-heuristics-and-parent)
                       
                       (:parse-goal-tests :no-applicable-cxns
                                          :no-strings-in-root
                                          :connected-semantic-network
                                          :connected-structure)
                       (:production-goal-tests :no-applicable-cxns
                                               :no-meaning-in-root :connected-structure)
                       (:max-nr-of-nodes . 20000)
                       (:de-render-mode . :de-render-scene-and-memory))
  :visualization-configurations ((:with-search-debug-data . t)
                                 (:hide-features . nil)
                                 (:show-constructional-dependencies . nil))
  :hashed t
  :cxn-inventory *clevr-dialog*)
            |#

;; To generate the adjectives
;; example of the result:

#|(def-fcg-cxn gray-lex-cxn
             ((?gray-lex-unit
               (args ((target ?color)))
               (sem-cat (sem-class property))
               (syn-cat (syn-class adjective)
                        (starts-with consonant)))
              <-
              (?gray-lex-unit
               (HASH meaning ((bind color-category ?color gray)))
               --
               (HASH form ((string ?gray-lex-unit "gray")))))
             :attributes (:meaning gray
                          :string "gray")
             :cxn-inventory *clevr-dialog*)|#


(defmethod hash ((construction construction)
                 (mode (eql :hash-string-meaning-lex-id))
                 &key &allow-other-keys)
  "Returns the string and meaning from the attributes of the construction"
  (when (or (attr-val construction :string)
            (attr-val construction :meaning)
            (attr-val construction :lex-id))
    (remove-duplicates
     (append (if (attr-val construction :string)
               (listify (attr-val construction :string))
               (list nil))
             (if (attr-val construction :meaning)
               (listify (attr-val construction :meaning))
               (list nil))
             (if (attr-val construction :lex-id)
               (listify (attr-val construction :lex-id))
               (list nil))))))
;;juist

(defmethod add-adjective-lex-cxn (cxn-inventory attribute category)
  (let ((cxn-name (internal-symb (upcase (format nil "~a-adj-lex-cxn" (hyphenize attribute)))))
        (unit-name (make-var (upcase (format nil "~a-unit" (hyphenize attribute)))))
        (out-var (make-var category))
        (category-name (internal-symb (upcase (format nil "gqa-~a-category" category)))))
    (eval `(def-fcg-cxn ,cxn-name
                        ((,unit-name
                          (args ((target ,out-var)))
                          (sem-cat (sem-class property))
                          (syn-cat (syn-class adjective)
                                   (starts-with ?starts-with)
                                   (leftmost-unit ,unit-name)
                                   (rightmost-unit ,unit-name)))
                         <-
                         (,unit-name
                          (HASH meaning ((bind ,category-name ,out-var ,(internal-symb (upcase (format nil "gqa-~a" (hyphenize attribute)))))))
                          --
                          (HASH form ((string ,unit-name ,attribute)))))
                        :cxn-inventory ,cxn-inventory
                        :attributes (:string ,attribute 
                                     :meaning ,(internal-symb (upcase (hyphenize attribute))))))))

(defmethod add-adjectives-lex-cxn (cxn-inventory attribute category)
  (let* ((cxn-name (internal-symb (upcase (format nil "~a-adj-lex-cxn" (hyphenize attribute)))))
        (unit-name (make-var (upcase (format nil "~a-unit" (hyphenize attribute)))))
        (out-var (make-var category))
        (category-name (internal-symb (upcase (format nil "gqa-~a-category" category))))
        (attrs (split-sequence::split-sequence #\Space attribute))
        (var-names (loop for attr in attrs
                         collect (make-var attr)))
         )
    (eval `(def-fcg-cxn ,cxn-name
                        ((,unit-name
                          (args ((target ,out-var)))
                          (sem-cat (sem-class property))
                          (syn-cat (syn-class adjective)
                                   (starts-with ?starts-with)
                                   (leftmost-unit ,(first var-names))
                                   (rightmost-unit ,(last-elt var-names))))
                         <-
                         (,unit-name
                          (HASH meaning ((bind ,category-name ,out-var ,(internal-symb (upcase (format nil "gqa-~a" (hyphenize attribute)))))))
                          --
                          (HASH form (,@(loop for word in attrs
                                                for var-name in var-names
                                              collect `(string ,var-name ,word))
                                      ,@(loop for word on var-names
                                                when (second word)
                                              collect `(meets ,(first word) ,(second word)))))))
                        :cxn-inventory ,cxn-inventory
                        :attributes (:string ,attrs 
                                     :meaning ,(internal-symb (upcase (hyphenize attribute))))))))

;; example of noun

#|(def-fcg-cxn thing-lex-cxn
             ((?thing-lex-unit
               (args ((target ?object)))
               (sem-cat (sem-class thing)
                        (grammar clevr))
               (syn-cat (syn-class noun)
                        (number ?number)
                        (starts-with ?first-letter)
                        (leftmost-unit ?thing-lex-unit)
                        (rightmost-unit ?thing-lex-unit)))
              <-
              (?thing-lex-unit
               (HASH meaning ((bind shape-category ?object thing)))
               --
               (lex-id obj)
               (number ?number)
               (starts-with ?first-letter)))
             :attributes (:meaning "thing"
                          :lex-id obj)
             :cxn-inventory *clevr-dialog*)|#


(defmethod add-substantive-lex-cxn (cxn-inventory noun category)
  (let* ((cxn-name (internal-symb (upcase (format nil "~a-lex-cxn" (hyphenize noun)))))
         (unit-name (make-var (upcase (format nil "~a-unit" (hyphenize noun)))))
         (out-var (make-var category))
         (category-name (internal-symb (upcase (format nil "gqa-~a-category" category)))))
    (eval `(def-fcg-cxn ,cxn-name
                        ((,unit-name
                          (args ((target ,out-var)))
                          (sem-cat (sem-class ,(internal-symb (upcase  (hyphenize category))))
                                   (grammar clevr)) ;; TODO, test this?
                          (syn-cat (syn-class noun)
                                   (number singular)
                                   (starts-with ?starts-with)
                                   (leftmost-unit ,unit-name)
                                   (rightmost-unit ,unit-name)))
                         <-
                         (,unit-name
                          (HASH meaning ((bind ,category-name ,out-var ,(internal-symb (upcase (format nil "gqa-~a" (hyphenize noun)))))))
                          --
                          (HASH form ((string ,unit-name ,noun)))))
                        :cxn-inventory ,cxn-inventory
                        :attributes (:string ,noun 
                                     :meaning ,(internal-symb (upcase (hyphenize noun))))))))

(defmethod add-substantives-lex-cxn (cxn-inventory noun category)
  (let* ((cxn-name (internal-symb (upcase (format nil "~a-lex-cxn" (hyphenize noun)))))
         (unit-name (make-var (upcase (format nil "~a-unit" (hyphenize noun)))))
         (out-var (make-var category))
         (category-name (internal-symb (upcase (format nil "gqa-~a-category" category))))
         (attrs (split-sequence::split-sequence #\Space noun))
         (var-names (loop for attr in attrs
                          collect (make-var attr)))
         )
    (eval `(def-fcg-cxn ,cxn-name
                        ((,unit-name
                          (args ((target ,out-var)))
                          (sem-cat (sem-class ,(internal-symb (upcase (hyphenize category))))
                                   (grammar clevr))
                          (syn-cat (syn-class noun)
                                   (number singular)
                                   (starts-with ?starts-with)
                                   (leftmost-unit ,(first var-names))
                                   (rightmost-unit ,(last-elt var-names))))
                         <-
                         (,unit-name
                          (HASH meaning ((bind ,category-name ,out-var ,(internal-symb (upcase (format nil "gqa-~a" (hyphenize noun)))))))
                          --
                          (HASH form (,@(loop for word in attrs
                                                for var-name in var-names
                                              collect `(string ,var-name ,word))
                                      ,@(loop for word on var-names
                                                when (second word)
                                              collect `(meets ,(first word) ,(second word)))))))
                        :cxn-inventory ,cxn-inventory
                        :attributes (:string ,attrs 
                                     :meaning ,(internal-symb (upcase (hyphenize noun))))))))

(defmethod add-category-lex-cxn (cxn-inventory category)
  (let* ((cxn-name (internal-symb (upcase (format nil "~a-lex-cxn" (hyphenize category)))))
         (unit-name (make-var (upcase (format nil "~a-unit" (hyphenize category)))))
         (out-var (make-var category))
                 )
    (eval `(def-fcg-cxn ,cxn-name
                        ((,unit-name
                          (args ((target ,out-var)))
                          (sem-cat (sem-class attribute)
                                   (grammar clevr))
                          (syn-cat (syn-class noun)
                                   (leftmost-unit ,unit-name)
                                   (rightmost-unit ,unit-name)))
                         <-
                         (,unit-name
                          (HASH meaning ((bind attribute-category ,out-var ,(internal-symb (upcase (format nil "gqa-~a" (hyphenize category)))))))
                          --
                          (HASH form ((string ,unit-name ,(downcase category))))))
                        :cxn-inventory ,cxn-inventory
                        :attributes (:string ,category 
                                     :meaning ,(internal-symb (upcase (hyphenize category))))))))

;; also add the category cxn which is a noun cxn
(defmethod add-categories-lex-cxn (cxn-inventory category)
  (let* ((cxn-name (internal-symb (upcase (format nil "~a-lex-cxn" (hyphenize category)))))
         (unit-name (make-var (upcase (format nil "~a-unit" (hyphenize category)))))
         (out-var (make-var category))
         ;(category-name (internal-symb (upcase (format nil "gqa-~a-category" category))))
         (attrs (split-sequence::split-sequence #\Space category))
         (var-names (loop for attr in attrs
                          collect (make-var attr)))
         )
    (eval `(def-fcg-cxn ,cxn-name
                        ((,unit-name
                          (args ((target ,out-var)))
                          (sem-cat (sem-class attribute)
                                   (grammar clevr))
                          (syn-cat (syn-class noun)
                                   (leftmost-unit ,(first var-names))
                                   (rightmost-unit ,(last-elt var-names))))
                         <-
                         (,unit-name
                          (HASH meaning ((bind attribute-category ,out-var ,(internal-symb (upcase (format nil "gqa-~a" (hyphenize category)))))))
                          --
                          (HASH form (,@(loop for word in attrs
                                                for var-name in var-names
                                              collect `(string ,var-name ,(downcase word)))
                                      ,@(loop for word on var-names
                                                when (second word)
                                              collect `(meets ,(first word) ,(second word)))))))
                        :cxn-inventory ,cxn-inventory
                        :attributes (:string ,attrs 
                                     :meaning ,(internal-symb (upcase (hyphenize category))))))))
#|(def-fcg-cxn color-noun-lex-cxn
             ((?color-lex-unit
               (args ((target ?attribute)))
               (sem-cat (sem-class attribute)
                        (grammar clevr))
               (syn-cat (syn-class noun)
                        (leftmost-unit ?color-lex-unit)
                        (rightmost-unit ?color-lex-unit)))
              <-
              (?color-lex-unit
               (HASH meaning ((bind attribute-category ?attribute color)))
               --
               (HASH form ((string ?color-lex-unit "color")))))
             :attributes (:meaning color
                          :string "color")
             :cxn-inventory *clevr-dialog*)|#


(defun generate-lexical-constructions (cxn-inventory metadata-file)
  (let* ((metadata (visual-dialog::decode-json-from-source metadata-file))
         (metadata-types (rest (assoc :values metadata))))
    (loop for type in metadata-types
          for category = (symbol-name (first type))
          for vals = (rest type)
          if (not (string= category "NAME")) 
          do (if (find #\Space category)
               (add-categories-lex-cxn cxn-inventory category)
               (add-category-lex-cxn cxn-inventory category))
              (loop for val in vals
                   do (if (find  #\Space val) ;; if space then multiple words so string and meets in cxn
                        (add-adjectives-lex-cxn cxn-inventory val category)
                        (add-adjective-lex-cxn cxn-inventory val category))
                   )
             (export (append (mapcar #'(lambda (x) (internal-symb (upcase (hyphenize (format nil "gqa-~a" x))))) vals)
                             (mapcar #'(lambda (x) (internal-symb (upcase (format nil "gqa-~a-category" x)))) vals)))
            else do
              (if (find #\Space category)
               (add-categories-lex-cxn cxn-inventory category)
               (add-category-lex-cxn cxn-inventory category))
              (loop for val in vals
                    do (if (find  #\Space val) ;; if space then multiple words so string and meets in cxn
                         (add-substantives-lex-cxn cxn-inventory val category)
                         (add-substantive-lex-cxn cxn-inventory val category)))
              (export (append (mapcar #'(lambda (x) (internal-symb (upcase (hyphenize (format nil "gqa-~a" x))))) vals)
                              (mapcar #'(lambda (x) (internal-symb (upcase (hyphenize (format nil "gqa-~a" category))))) vals)
                              (mapcar #'(lambda (x) (internal-symb (upcase (format nil "gqa-~a-category" category)))) vals))))
    cxn-inventory))



#|(export '(;;all object properties
          gqa-material-category 
          ))|#
         
(generate-lexical-constructions *clevr-dialog* visual-dialog::*gqa-metadata*)
;(add-element (make-html  *clevr-dialog*))

;(activate-monitor trace-fcg)
;(deactivate-all-monitors)
;(comprehend "a plastic straw is to the right of all things" :cxn-inventory *clevr-dialog*) ;;succes!!
;(comprehend "an old fashioned straw is to the right of all things" :cxn-inventory *clevr-dialog*) ;;succes!
;(comprehend "a strawberry is to the right of all things" :cxn-inventory *clevr-dialog*) ;;succes!
;(comprehend "a big sphere is to the right of all things" :cxn-inventory *clevr-dialog*) ;;succes!
;(comprehend "a partly cloudy sphere is to the right of all things" :cxn-inventory *clevr-dialog*) ;;succes!

