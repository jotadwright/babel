(in-package :mwm-evaluation)

;; geleerde concepten staan onder
;; Babel/experiments/multidimensional-word-meanings/learned-concepts.zip
;; cl-store:restore om .store files in te lezen
(defclass concept-entity (concept entity)
  ())

(defclass color-concept (concept-entity) ())
(defclass size-concept (concept-entity) ())
(defclass material-concept (concept-entity) ())
(defclass shape-concept (concept-entity) ())
(defclass spatial-concept (concept-entity) ())

(defun restore-concept (path class)
  (let ((concept (cl-store:restore path)))
    (Make-instance class
                   :id (pathname->conceptname path)
                   :form (form concept)
                   :meaning (copy-object (meaning concept)))))

(defun pathname->conceptname (path)
  (intern (upcase (first (split (pathname-name path) #\-)))))





(defmethod weighted-similarity ((object mwm-object) (concept concept-entity))
  (loop for prototype in (meaning concept)
        for similarity = (mwm::similarity object prototype)
        collect (* (mwm::certainty prototype) similarity) into weighted-similarities
        finally (return (average weighted-similarities))))