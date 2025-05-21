(in-package :clevr-world)

;; ################################
;; clevr categories
;; ################################

(export '(category 
          concept-category
          boolean-category
          
          img-path pathname-entity get-pathname
          ;shapes sizes colors materials spatial-relations attributes
          category-value))

(defclass category (entity) ()
  (:documentation "Abstract base class for all categories"))

(defclass boolean-category (category)
  ((bool :type bool :initarg :bool :reader bool))
  (:documentation "An entity representation of booleans"))

(defclass concept-category (category)
  ((concept-id :type symbol :initarg :concept-id :reader concept-id))
  (:documentation "An entity representation of concepts."))

(defclass pathname-entity (entity)
  ((pathname :type (or null pathname) :initarg :pathname
             :reader get-pathname :initform nil))
  (:documentation "Using pathnames as irl entities"))

;; ################################
;; category-value
;; ################################

(defgeneric category-value (category)
  (:documentation "Obtain the value of the category"))

(defmethod category-value ((boolean-category boolean-category))
  (bool boolean-category))

(defmethod category-value ((concept-category concept-category))
  (concept-id concept-category))


;; ################################
;; clevr ontology
;; ################################

(defgeneric add-category-to-ontology (ontology value category)
  (:documentation "Add a value of the given category to the ontology"))

(defmethod add-category-to-ontology (ontology value (category (eql 'attribute)))
  (push-data ontology 'attributes (make-instance 'attribute-category :id value :attribute value)))

(defun build-clevr-ontology ()
  "Build the clevr ontology from the metadata file.
   How to handle 'thing'? Also consider this a shape?"
  (let* ((metadata-file (babel-pathname :directory '("sharing" "clevr-world" "data")
                                        :name "metadata" :type "json"))
         (metadata (decode-json-as-alist-from-source metadata-file))
         (metadata-types (cdr (assoc :types metadata)))
         (clevr-ontology (make-blackboard)))

    ;; manually add true and false to the ontology
    (push-data clevr-ontology 'booleans (make-instance 'boolean-category :id 'yes :bool t))
    (push-data clevr-ontology 'booleans (make-instance 'boolean-category :id 'no :bool nil))
    ;; manually add all attributes to the ontology
    (loop for attribute in '(shape size material color)
          do (add-category-to-ontology clevr-ontology attribute 'attribute))

    ;(add-category-to-ontology clevr-ontology 'attribute 'attribute) ;; TODO HIER MEERDERE DUMMY'S MAKEN DIE WE LATER LEREN
    clevr-ontology))

;; ################################
;; global variables
;; ################################

(export '(*clevr-ontology*))

(defparameter *clevr-ontology* (build-clevr-ontology))

;; ################################
;; copy object
;; ################################


(defmethod copy-object ((bool-cat boolean-category))
  (make-instance 'boolean-category
                 :id (id bool-cat)
                 :bool (bool bool-cat)))

(defmethod copy-object ((attribute-cat attribute-category))
  (make-instance 'attribute-category
                 :id (id attribute-cat)
                 :attribute (attribute attribute-cat)))


(defmethod copy-object ((pe pathname-entity))
  (make-instance 'pathname-entity :id (id pe)
                 :pathname (pathname pe)))

(defmethod print-object ((pe pathname-entity) stream)
  (if *print-pretty*
      (pprint-logical-block (stream nil)
        (if (pathname-name (get-pathname pe))
          (format stream "<pathname .../~a.~a>"
                  (pathname-name (get-pathname pe))
                  (pathname-type (get-pathname pe)))
          (format stream "<pathname .../~a/>"
                  (last-elt (pathname-directory (get-pathname pe))))))
      (call-next-method)))