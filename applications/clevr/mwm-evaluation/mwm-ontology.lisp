(in-package :mwm-evaluation)

;;----------------------------------------------;;
;; Functions and classes for making the ontology;;
;;----------------------------------------------;;

;; Make new concept-entity class that is a subclass of concept and entity
(defclass concept-entity (concept entity)
  ())

;; Make subclasses for concept-entity for each of the concept types
(defclass color-concept (concept-entity) ())
(defclass size-concept (concept-entity) ())
(defclass material-concept (concept-entity) ())
(defclass shape-concept (concept-entity) ())
(defclass spatial-concept (concept-entity) ())


;;function that takes the pathname of a conceptfile and extracts the conceptname out of it
(defun pathname->conceptname (path)
  (intern (upcase (first (split (pathname-name path) #\-)))))


;;function that restores a concept using cl-store and makes it an instance of the concept-entity class
(defun restore-concept (path concept-class)
  (let ((concept (cl-store:restore path)))
    (Make-instance concept-class
                   :id (pathname->conceptname path)
                   :form (form concept)
                   :meaning (copy-object (meaning concept)))))

;; Make an ontology (instance of #'blackboard)
(defun make-mwm-ontology (concepts-pathname ontology-name)
  (defparameter ontology-name (make-blackboard))
  (loop for pathname in (directory concepts-pathname)
        do (cond ((if (member (pathname->conceptname pathname) '(blue brown cyan gray green purple red yellow)) t nil)
                   (push-data ontology-name 'colors (restore-concept pathname 'color-concept)))
              ((if (member (pathname->conceptname pathname) '(metal rubber)) t nil)
                   (push-data ontology-name 'materials (restore-concept pathname 'material-concept)))
              ((if (member (pathname->conceptname pathname) '(cube cylinder sphere)) t nil)
                   (push-data ontology-name 'shapes (restore-concept pathname 'shape-concept)))
              ((if (member (pathname->conceptname pathname) '(large small)) t nil)
                   (push-data ontology-name 'sizes (restore-concept pathname 'size-concept)))
              ((if (member (pathname->conceptname pathname) '(left right)) t nil)
                   (push-data ontology-name 'spatial-relations (restore-concept pathname 'spatial-concept)))
              ((eql (pathname->conceptname pathname) 'front) (push-data ontology-name 'spatial-relations (let ((concept (cl-store:restore pathname)))
                                                   (Make-instance 'spatial-concept
                                                                  :id 'behind
                                                                  :form "behind"
                                                                  :meaning (copy-object (meaning concept))))))
              ((eql (pathname->conceptname pathname) 'behind) (push-data ontology-name 'spatial-relations (let ((concept (cl-store:restore pathname)))
                                                   (Make-instance 'spatial-concept
                                                                  :id 'front
                                                                  :form "front"
                                                                  :meaning (copy-object (meaning concept))))))
              ))
  (set-data ontology-name 'thing (list (make-instance 'shape-concept
                                                      :id 'thing
                                                      :form "thing"
                                                      :meaning nil)))
  (push-data ontology-name 'booleans (make-instance 'boolean-category :id 'yes :bool t))
  (push-data ontology-name 'booleans (make-instance 'boolean-category :id 'no :bool nil))
  (loop for attribute in '(shape size material color)
          do (clevr-world::add-category-to-ontology ontology-name attribute 'attribute)))


;; weighted similarity method that can be used to compare the prototypical values of an object and a concept (see: "Babel/experiments/multidimensional-word-meanings/concept.lisp" for the original method)
(defmethod weighted-similarity ((object mwm-object) (concept concept-entity))
  (loop for prototype in (meaning concept)
        for similarity = (mwm::similarity object prototype)
        collect (* (mwm::certainty prototype) similarity) into weighted-similarities
        finally (return (average weighted-similarities))))