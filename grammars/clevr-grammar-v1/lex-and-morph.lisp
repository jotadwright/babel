;;;; lex-and-morph.lisp

(in-package :clevr-grammar-v1)

;; ----------------------------------------------------- ;;
;; This file contains contains functions that read json  ;;
;; data and automatically create lex and morph cxns      ;;
;; ----------------------------------------------------- ;;

(defgeneric add-lex-cxn-of-clevr-type (cxn-inventory lex-id type)
  (:documentation "Generate a lexical cxn of the given CLEVR datatype, for the given word"))
(defgeneric add-morph-cxn-of-clevr-type (cxn-inventory word lex-id type)
  (:documentation "Generate a morphological cxn of the given CLEVR datatype, for the given word"))

;;;; SHAPE
;; NOTE: shapes and things have syn-cat/number feature to differentiate
;; between singular and plural
(defmethod add-lex-cxn-of-clevr-type (cxn-inventory lex-id (type (eql :*shape)))
  (let ((cxn-name (internal-symb (upcase (string-append (hyphenize lex-id) "-lex-cxn"))))
        (unit-name (make-var (upcase (string-append (hyphenize lex-id) "-unit"))))
        (out-var (make-var 'shape)))
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
                          (HASH meaning ((bind shape-category ,out-var ,(internal-symb (hyphenize lex-id)))))
                          --
                          (lex-id ,(internal-symb (hyphenize lex-id)))
                          (syn-cat 
                                   (number ?number))))
                        :cxn-inventory ,cxn-inventory
                        :cxn-set (hashed hashed-lex)
                        :attributes (:lex-id ,(internal-symb (hyphenize lex-id)) 
                                     :meaning ,(internal-symb (hyphenize lex-id))
                                     :clevr-datatype ,(symbol-name type))))))

(defmethod add-morph-cxn-of-clevr-type (cxn-inventory word lex-id (type (eql :*shape)))
  (let* ((plural-word (string-append word "s"))
         (sing-cxn-name (internal-symb (upcase (string-append (hyphenize word) "-morph-cxn"))))
         (sing-unit-name (make-var (upcase (string-append (hyphenize word) "-unit"))))
         (plural-cxn-name (internal-symb (upcase (string-append (hyphenize plural-word) "-morph-cxn"))))
         (plural-unit-name (make-var (upcase (string-append (hyphenize plural-word) "-unit")))))
    (eval `(def-fcg-cxn ,sing-cxn-name
                        ((,sing-unit-name
                          (footprints (morph)))
                         <-
                         (,sing-unit-name
                          (lex-id ,(internal-symb (hyphenize lex-id)))
                          (syn-cat (lex-class noun)
                                   
                                   (number singular))
                          (footprints (NOT morph))
                          --
                          (HASH form ((string ,sing-unit-name ,word)))))
                        :cxn-inventory ,cxn-inventory
                        :cxn-set (hashed hashed-morph)
                        :attributes (:string ,word :lex-id ,(internal-symb (hyphenize lex-id)))))
    (eval `(def-fcg-cxn ,plural-cxn-name
                        ((,plural-unit-name
                          (footprints (morph)))
                         <-
                         (,plural-unit-name
                          (lex-id ,(internal-symb (hyphenize lex-id)))
                          (syn-cat (lex-class noun)
                                   
                                   (number plural))
                          (footprints (NOT morph))
                          --
                          (HASH form ((string ,plural-unit-name ,plural-word)))))
                        :cxn-inventory ,cxn-inventory
                        :cxn-set (hashed hashed-morph)
                        :attributes (:string ,plural-word :lex-id ,(internal-symb (hyphenize lex-id)))))))

;;;; THING
(defmethod add-lex-cxn-of-clevr-type (cxn-inventory lex-id (type (eql :*thing)))
  (let ((cxn-name (internal-symb (upcase (string-append (hyphenize lex-id) "-lex-cxn"))))
        (unit-name (make-var (upcase (string-append (hyphenize lex-id) "-unit"))))
        (out-var (make-var 'shape)))
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
                          (HASH meaning ((bind shape-category ,out-var thing)))
                          --
                          (lex-id ,(internal-symb (hyphenize lex-id)))
                          (syn-cat 
                                   (number ?number))))
                        :cxn-inventory ,cxn-inventory
                        :cxn-set (hashed hashed-lex)
                        :attributes (:lex-id ,(internal-symb (hyphenize lex-id))
                                     :meaning ,(internal-symb (hyphenize lex-id))
                                     :clevr-datatype ,(symbol-name type))))))

(defmethod add-morph-cxn-of-clevr-type (cxn-inventory word lex-id (type (eql :*thing)))
  (let* ((plural-word (string-append word "s"))
         (sing-cxn-name (internal-symb (upcase (string-append (hyphenize word) "-morph-cxn"))))
         (sing-unit-name (make-var (upcase (string-append (hyphenize word) "-unit"))))
         (plural-cxn-name (internal-symb (upcase (string-append (hyphenize plural-word) "-morph-cxn"))))
         (plural-unit-name (make-var (upcase (string-append (hyphenize plural-word) "-unit")))))
    (eval `(def-fcg-cxn ,sing-cxn-name
                        ((,sing-unit-name
                          (footprints (morph)))
                         <-
                         (,sing-unit-name
                          (syn-cat (lex-class noun)
                                   
                                   (number singular))
                          (footprints (NOT morph))
                          (lex-id ,(internal-symb (hyphenize lex-id)))
                          --
                          (HASH form ((string ,sing-unit-name ,word)))))
                        :cxn-inventory ,cxn-inventory
                        :cxn-set (hashed hashed-morph)
                        :attributes (:string ,word :lex-id ,(internal-symb (hyphenize lex-id)))))
    (eval `(def-fcg-cxn ,plural-cxn-name
                        ((,plural-unit-name
                          (footprints (morph)))
                         <-
                         (,plural-unit-name
                          (syn-cat (lex-class noun)
                                   
                                   (number plural))
                          (footprints (NOT morph))
                          (lex-id ,(internal-symb (hyphenize lex-id)))
                          --
                          (HASH form ((string ,plural-unit-name ,plural-word)))))
                        :cxn-inventory ,cxn-inventory
                        :cxn-set (hashed hashed-morph)
                        :attributes (:string ,plural-word :lex-id ,(internal-symb (hyphenize lex-id)))))))

;;;; COLOR
;; NOTE: Colors have no morph cxns since there are no synonyms
(defmethod add-lex-cxn-of-clevr-type (cxn-inventory lex-id (type (eql :*color)))
  (let ((cxn-name (internal-symb (upcase (string-append (hyphenize lex-id) "-lex-cxn"))))
        (unit-name (make-var (upcase (string-append (hyphenize lex-id) "-unit"))))
        (out-var (make-var 'color)))
    (eval `(def-fcg-cxn ,cxn-name
                        ((,unit-name
                          (args ((sources nil)
                                 (target ,out-var)))
                          (sem-cat (sem-class color))
                          (syn-cat (lex-class adjective)))
                         <-
                         (,unit-name
                          (HASH meaning ((bind color-category ,out-var ,(internal-symb (hyphenize lex-id)))))
                          --
                          (HASH form ((string ,unit-name ,(downcase lex-id))))))
                        :cxn-inventory ,cxn-inventory
                        :cxn-set (hashed hashed-lex)
                        :attributes (:lex-id ,(internal-symb (hyphenize lex-id))
                                     :string ,(downcase lex-id)
                                     :meaning ,(internal-symb (hyphenize lex-id))
                                     :clevr-datatype ,(symbol-name type))))))

;;;; SIZE
(defmethod add-lex-cxn-of-clevr-type (cxn-inventory lex-id (type (eql :*size)))
  (let ((cxn-name (internal-symb (upcase (string-append (hyphenize lex-id) "-lex-cxn"))))
        (unit-name (make-var (upcase (string-append (hyphenize lex-id) "-unit"))))
        (out-var (make-var 'size)))
    (eval `(def-fcg-cxn ,cxn-name
                        ((,unit-name
                          (args ((sources nil)
                                 (target ,out-var)))
                          (sem-cat (sem-class size))
                          (syn-cat (lex-class adjective)))
                         <-
                         (,unit-name
                          (HASH meaning ((bind size-category ,out-var ,(internal-symb (hyphenize lex-id)))))
                          --
                          (lex-id ,(internal-symb (hyphenize lex-id)))))
                        :cxn-inventory ,cxn-inventory
                        :cxn-set (hashed hashed-lex)
                        :attributes (:lex-id ,(internal-symb (hyphenize lex-id))
                                     :meaning ,(internal-symb (hyphenize lex-id))
                                     :clevr-datatype ,(symbol-name type))))))

(defmethod add-morph-cxn-of-clevr-type (cxn-inventory word lex-id (type (eql :*size)))
  (let ((cxn-name (internal-symb (upcase (string-append (hyphenize word) "-morph-cxn"))))
        (unit-name (make-var (upcase (string-append (hyphenize word) "-unit")))))
    (eval `(def-fcg-cxn ,cxn-name
                        ((,unit-name
                          (footprints (morph)))
                         <-
                         (,unit-name
                          (lex-id ,(internal-symb (hyphenize lex-id)))
                          (syn-cat (lex-class adjective))
                          (footprints (NOT morph))
                          --
                          (HASH form ((string ,unit-name ,word)))))
                        :cxn-inventory ,cxn-inventory
                        :cxn-set (hashed hashed-morph)
                        :attributes (:string ,word :lex-id ,(internal-symb (hyphenize lex-id)))))))

;;;; MATERIAL
(defmethod add-lex-cxn-of-clevr-type (cxn-inventory lex-id (type (eql :*material)))
  (let ((cxn-name (internal-symb (upcase (string-append (hyphenize lex-id) "-lex-cxn"))))
        (unit-name (make-var (upcase (string-append (hyphenize lex-id) "-unit"))))
        (out-var (make-var 'material)))
    (eval `(def-fcg-cxn ,cxn-name
                        ((,unit-name
                          (args ((sources nil)
                                 (target ,out-var)))
                          (sem-cat (sem-class material))
                          (syn-cat (lex-class adjective)))
                         <-
                         (,unit-name
                          (HASH meaning ((bind material-category ,out-var ,(internal-symb (hyphenize lex-id)))))
                          --
                          (lex-id ,(internal-symb (hyphenize lex-id)))))
                        :cxn-inventory ,cxn-inventory
                        :cxn-set (hashed hashed-lex)
                        :attributes (:lex-id ,(internal-symb (hyphenize lex-id))
                                     :meaning ,(internal-symb (hyphenize lex-id))
                                     :clevr-datatype ,(symbol-name type))))))

(defmethod add-morph-cxn-of-clevr-type (cxn-inventory word lex-id (type (eql :*material)))
  (let ((cxn-name (internal-symb (upcase (string-append (hyphenize word) "-morph-cxn"))))
        (unit-name (make-var (upcase (string-append (hyphenize word) "-unit")))))
    (eval `(def-fcg-cxn ,cxn-name
                        ((,unit-name
                          (footprints (morph)))
                         <-
                         (,unit-name
                          (syn-cat (lex-class adjective))
                          (lex-id ,(internal-symb (hyphenize lex-id)))
                          (footprints (NOT morph))
                          --
                          (HASH form ((string ,unit-name ,word)))))
                        :cxn-inventory ,cxn-inventory
                        :cxn-set (hashed hashed-morph)
                        :attributes (:string ,word :lex-id ,(internal-symb (hyphenize lex-id)))))))

;;;; RELATION
(defmethod add-lex-cxn-of-clevr-type (cxn-inventory lex-id (type (eql :*relation)))
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
                        :cxn-set (hashed hashed-lex)
                        :attributes (:lex-id ,(internal-symb (hyphenize lex-id)) 
                                     :meaning ,(internal-symb (hyphenize lex-id))
                                     :clevr-datatype ,(symbol-name type))))))

(defmethod add-morph-cxn-of-clevr-type (cxn-inventory word lex-id (type (eql :*relation)))
  (let ((cxn-name (internal-symb (upcase (string-append (hyphenize word) "-morph-cxn"))))
        (unit-name (make-var (upcase (string-append (hyphenize word) "-unit")))))
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
                          (HASH form ((string ,unit-name ,word)))))
                        :cxn-inventory ,cxn-inventory
                        :cxn-set (hashed hashed-morph)
                        :attributes (:string ,word :lex-id ,(internal-symb (hyphenize lex-id)))))))

;;;; TYPE
;; NOTE: type cxns have their property type also in the footprint
;; this way, other constructions can specify (NOT this-type)
(defun add-clevr-type-cxn (cxn-inventory type)
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
                        :cxn-set (hashed hashed-lex)
                        :attributes (:string ,(downcase type)
                                     :meaning ,(internal-symb (hyphenize type)))))))

(defun generate-lexical-constructions (cxn-inventory)
  ;; This function will read metadata.json and read all 'types'
  ;; Types are denoted as :*shape, :*color, etc
  ;; For each type, check if the vocabulary for this type is non-empty
  ;; e.g. :*shape has [cube, cylinder, ...]
  ;; If it is non-empty, generate a lexical construction for every
  ;; element in the vocab. The type information will be passed on,
  ;; since a lex cxn for a shape is different than for a color.
  ;; Additionally, there will be a generic 'thing-cxn' that is similar
  ;; to a shape. This is added manually in the end.
  (let* ((metadata-file (babel-pathname :directory '("grammars" "clevr-grammar-v1" "data")
                                        :name "metadata" :type "json"))
         (metadata (decode-json-from-source metadata-file))
         (metadata-types (cdr (assoc :types metadata))))
    (loop for (type . vocab) in metadata-types
          for type-str = (upcase (subseq (mkstr type) 1))
          when vocab
          do (progn
               (loop for lex-id in vocab
                     do (add-lex-cxn-of-clevr-type cxn-inventory (upcase lex-id) type))
               (unless (string= type-str "RELATION")
                 (add-clevr-type-cxn cxn-inventory type-str))))
    (add-lex-cxn-of-clevr-type cxn-inventory "THING" :*thing)
    cxn-inventory))

(defun generate-morphological-constructions (cxn-inventory)
  ;; This function will read synonyms.json and read all keys
  ;; (e.g. thing, sphere, left of, ...). For each key, get the
  ;; list of synonyms and generate morphological constructions that
  ;; will map to the lexical construction of the key, created in
  ;; previous function.
  
  ;; IMPORTANT! The file synonyms.json contains entries for "above"
  ;; and "below", but these are not specified in the types in metadata.json
  ;; (so they have no lexical cxns). For the moment, these are left out.
  ;; One option would be to consider these as synonyms for "behind" and
  ;; "in front of", but they are not specified in the dataset as such.

  ;; Also, metadata.json contains 'cylinder' as a shape, but there are
  ;; no synonyms specified in synonyms.json. So, we add the morph cxns
  ;; for cylinder manually.
  (let* ((synonyms-file (babel-pathname :directory '("grammars" "clevr-grammar-v1" "data")
                                        :name "synonyms" :type "json"))
         (synonyms (decode-json-from-source synonyms-file)))
    (loop for (key . list-of-synonyms) in synonyms
          for lex-id-str = (hyphenize (upcase (mkstr key)))
          for lex-id-cxn = (find lex-id-str
                                 (constructions-list cxn-inventory)
                                 :key (lambda (cxn) (attr-val cxn :lex-id))
                                 :test #'string=)
          for lex-id-type = (when lex-id-cxn (attr-val lex-id-cxn :clevr-datatype))
          when lex-id-cxn
          do (loop for word in list-of-synonyms
                   do (add-morph-cxn-of-clevr-type cxn-inventory
                                                   (downcase word)
                                                   (upcase lex-id-str)
                                                   (make-kw lex-id-type) 
                                                   )))
    (add-morph-cxn-of-clevr-type cxn-inventory "cylinder" "CYLINDER" :*shape) 
    cxn-inventory))