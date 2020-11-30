;;;; lex-and-morph.lisp

(in-package :coco-grammar)

;; ----------------------------------------------------- ;;
;; This file contains contains functions that read json  ;;
;; data and automatically create lex and morph cxns      ;;
;; ----------------------------------------------------- ;;

;;;; NOUNS
(defgeneric add-coco-lex-cxn (cxn-inventory lex-id)
  (:documentation "Generate a lexical cxn for the given COCO noun"))
(defgeneric add-coco-singular-morph-cxn (cxn-inventory lex-id form &key overwrite-cxn-name)
  (:documentation "Generate a morphological cxn for the given COCO noun in its singular form"))
(defgeneric add-coco-plural-morph-cxn (cxn-inventory lex-id form &key overwrite-cxn-name)
  (:documentation "Generate a morphological cxn for the given COCO noun in its plural form"))

(defmethod add-coco-lex-cxn (cxn-inventory lex-id)
  (let ((cxn-name (internal-symb (upcase (string-append (hyphenize lex-id) "-lex-cxn"))))
        (unit-name (make-var (upcase (string-append (hyphenize lex-id) "-unit"))))
        (out-var (make-var 'category)))
    (eval `(def-fcg-cxn ,cxn-name
                        ((,unit-name
                          (args ((sources nil)
                                 (target ,out-var)))
                          (sem-cat (sem-class shape))
                          (syn-cat (lex-class noun)
                                   (syn-function nominal)
                                   (number ?number)))
                         <-
                         (,unit-name
                          (HASH meaning ((bind coco-category ,out-var ,(internal-symb (hyphenize lex-id)))))
                          --
                          (lex-id ,(internal-symb (hyphenize lex-id)))
                          (syn-cat (number ?number))))
                        :cxn-inventory ,cxn-inventory
                        :cxn-set lex
                        :attributes (:lex-id ,(internal-symb (hyphenize lex-id))
                                     :meaning ,(internal-symb (hyphenize lex-id)))))))

(defmethod add-coco-singular-morph-cxn (cxn-inventory lex-id form &key overwrite-cxn-name)
  (let ((cxn-name (internal-symb
                   (upcase
                    (string-append
                     (if overwrite-cxn-name
                       (string-append (hyphenize form) "-singular")
                       (hyphenize form))
                     "-morph-cxn"))))
        (unit-name (make-var (upcase (string-append (hyphenize form) "-unit")))))
    (eval `(def-fcg-cxn ,cxn-name
                        ((,unit-name
                          (footprints (morph)))
                         <-
                         (,unit-name
                          (lex-id ,(internal-symb (hyphenize lex-id)))
                          (syn-cat (lex-class noun)
                                   (number singular))
                          (footprints (NOT morph))
                          --
                          (HASH form ((string ,unit-name ,form)))))
                        :cxn-inventory ,cxn-inventory
                        :cxn-set morph
                        :attributes (:string ,form
                                     :lex-id ,(internal-symb (hyphenize lex-id)))))))

(defmethod add-coco-plural-morph-cxn (cxn-inventory lex-id form &key overwrite-cxn-name)
  (let ((cxn-name (internal-symb
                   (upcase
                    (string-append
                     (if overwrite-cxn-name
                       (string-append (hyphenize form) "-plural")
                       (hyphenize form))
                     "-morph-cxn"))))
        (unit-name (make-var (upcase (string-append (hyphenize form) "-unit")))))
    (eval `(def-fcg-cxn ,cxn-name
                        ((,unit-name
                          (footprints (morph)))
                         <-
                         (,unit-name
                          (lex-id ,(internal-symb (hyphenize lex-id)))
                          (syn-cat (lex-class noun)
                                   (number plural))
                          (footprints (NOT morph))
                          --
                          (HASH form ((string ,unit-name ,form)))))
                        :cxn-inventory ,cxn-inventory
                        :cxn-set morph
                        :attributes (:string ,form
                                     :lex-id ,(internal-symb (hyphenize lex-id)))))))
  

;;;; RELATIONS
(defgeneric add-relation-lex-cxn (cxn-inventory lex-id)
  (:documentation "Add the lex cxn for a spatial relation"))
(defgeneric add-relation-morph-cxn (cxn-inventory lex-id form)
  (:documentation "Add the morph cxn for a spatial relation"))

(defmethod add-relation-lex-cxn (cxn-inventory lex-id)
  (let ((cxn-name (internal-symb (upcase (string-append (hyphenize lex-id) "-lex-cxn"))))
        (unit-name (make-var (upcase (string-append (hyphenize lex-id) "-unit"))))
        (out-var (make-var 'spatial-relation)))
    (eval `(def-fcg-cxn ,cxn-name
                        ((,unit-name
                          (args ((sources nil)
                                 (target ,out-var)))
                          (sem-cat (sem-class spatial-relation)
                                   (sem-type ,(internal-symb (hyphenize lex-id))))
                          (syn-cat (lex-class preposition)))
                         <-
                         (,unit-name
                          (HASH meaning ((bind spatial-relation-category ,out-var ,(internal-symb (hyphenize lex-id)))))
                          --
                          (lex-id ,(internal-symb (hyphenize lex-id)))))
                        :cxn-inventory ,cxn-inventory
                        :cxn-set lex
                        :attributes (:lex-id ,(internal-symb (hyphenize lex-id)) 
                                     :meaning ,(internal-symb (hyphenize lex-id)))))))

(defmethod add-relation-morph-cxn (cxn-inventory lex-id form)
  (let ((cxn-name (internal-symb (upcase (string-append (hyphenize form) "-morph-cxn"))))
        (unit-name (make-var (upcase (string-append (hyphenize form) "-unit")))))
    (eval `(def-fcg-cxn ,cxn-name
                        ((,unit-name
                          (footprints (morph)))
                         <-
                         (,unit-name
                          (lex-id ,(internal-symb (hyphenize lex-id)))
                          (syn-cat (lex-class preposition)
                                   )
                          (footprints (NOT morph))
                          --
                          (HASH form ((string ,unit-name ,form)))))
                        :cxn-inventory ,cxn-inventory
                        :cxn-set morph
                        :attributes (:string ,form
                                     :lex-id ,(internal-symb (hyphenize lex-id)))))))

;;;; TYPE
;; NOTE: type cxns have their property type also in the footprint
;; this way, other constructions can specify (NOT this-type)
(defun add-coco-type-cxn (cxn-inventory type)
  (let ((cxn-name (internal-symb (upcase (string-append (hyphenize type) "-lex-cxn"))))
        (unit-name (make-var (upcase (string-append (hyphenize type) "-unit"))))
        (out-var (make-var 'attribute)))
    (eval `(def-fcg-cxn ,cxn-name
                        ((,unit-name
                          (args ((sources nil)
                                 (target ,out-var)))
                          (sem-cat (sem-class attribute))
                          (syn-cat (lex-class noun))
                          (property-type ,(internal-symb (hyphenize type)))
                          (footprints (morph ,(internal-symb (hyphenize type)))))
                         <-
                         (,unit-name
                          (HASH meaning ((bind attribute-category ,out-var ,(internal-symb (hyphenize type)))))
                          --
                          (HASH form ((string ,unit-name ,(downcase type))))))
                        :cxn-inventory ,cxn-inventory
                        :cxn-set lex
                        :attributes (:string ,(downcase type)
                                     :meaning ,(internal-symb (hyphenize type)))))))

(defun generate-lex-and-morph-cxns (cxn-inventory)
  ;; read the json file
  (let* ((metadata-file (babel-pathname :directory '("grammars" "coco-grammar" "data")
                                        :name "coco-lex" :type "json"))
         (metadata (decode-json-from-source metadata-file)))
    ;; add lex and morph cxns for the nouns
    ;; check that the singular and plural form is different!
    (loop for coco-noun in (rest (assoc :nouns metadata))
          for lex-id = (rest (assoc :lex-id coco-noun))
          for singular-form = (rest (assoc :singular coco-noun))
          for plural-form = (rest (assoc :plural coco-noun))
          for overwrite-cxn-name-p = (string= singular-form plural-form)
          do (add-coco-lex-cxn cxn-inventory lex-id)
          do (add-coco-singular-morph-cxn cxn-inventory lex-id singular-form
                                          :overwrite-cxn-name overwrite-cxn-name-p)
          do (add-coco-plural-morph-cxn cxn-inventory lex-id plural-form
                                        :overwrite-cxn-name overwrite-cxn-name-p))
    ;; add lex and morph cxns for the relations
    (loop for coco-relation in (rest (assoc :relations metadata))
          for lex-id = (rest (assoc :lex-id coco-relation))
          do (add-relation-lex-cxn cxn-inventory lex-id)
          do (loop for form in (rest (assoc :forms coco-relation))
                   do (add-relation-morph-cxn cxn-inventory lex-id form)))
    ;; add lex cxns for the types
    (loop for coco-type in (rest (assoc :types metadata))
          do (add-coco-type-cxn cxn-inventory coco-type))))



