(in-package :mwm-evaluation)

;;---------------------------;;
;; Some classes and functions;;
;;---------------------------;;

;; Make new concept-entity class that is a subclass of concept and entity
(defclass concept-entity (concept entity)
  ())

;; Make subclasses for concept-entity for each of the concept types
(defclass color-concept (concept-entity) ())
(defclass size-concept (concept-entity) ())
(defclass material-concept (concept-entity) ())
(defclass shape-concept (concept-entity) ())
(defclass spatial-concept (concept-entity) ())

;;function that restores a concept using cl-store and makes it an instance of the concept-entity class
(defun restore-concept (path class)
  (let ((concept (cl-store:restore path)))
    (Make-instance class
                   :id (pathname->conceptname path)
                   :form (form concept)
                   :meaning (copy-object (meaning concept)))))

;;function that takes the pathname of a conceptfile and extracts the conceptname out of it
(defun pathname->conceptname (path)
  (intern (upcase (first (split (pathname-name path) #\-)))))

;; weighted similarity method that can be used to compare the prototypical values of an object and a concept (see: "Babel/experiments/multidimensional-word-meanings/concept.lisp" for the original method)
(defmethod weighted-similarity ((object mwm-object) (concept concept-entity))
  (loop for prototype in (meaning concept)
        for similarity = (mwm::similarity object prototype)
        collect (* (mwm::certainty prototype) similarity) into weighted-similarities
        finally (return (average weighted-similarities))))