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
(defun make-mwm-ontology (concepts-pathname)
  (defparameter *my-ontology* (make-blackboard))
      (loop for pathname in (directory concepts-pathname)
        do (cond ((member (pathname->conceptname pathname) '(blue brown cyan gray green purple red yellow))
                  (push-data *my-ontology* 'colors (restore-concept pathname 'color-concept)))
              ((member (pathname->conceptname pathname) '(metal rubber))
                   (push-data *my-ontology* 'materials (restore-concept pathname 'material-concept)))
              ( (member (pathname->conceptname pathname) '(cube cylinder sphere))
                   (push-data *my-ontology* 'shapes (restore-concept pathname 'shape-concept)))
              ( (member (pathname->conceptname pathname) '(large small)) 
                   (push-data *my-ontology* 'sizes (restore-concept pathname 'size-concept)))
              ( (member (pathname->conceptname pathname) '(left right)) 
                   (push-data *my-ontology* 'spatial-relations (restore-concept pathname 'spatial-concept)))
              ((eql (pathname->conceptname pathname) 'front) (push-data *my-ontology* 'spatial-relations (let ((concept (cl-store:restore pathname)))
                                                   (Make-instance 'spatial-concept
                                                                  :id 'behind
                                                                  :form "behind"
                                                                  :meaning (copy-object (meaning concept))))))
              ((eql (pathname->conceptname pathname) 'behind) (push-data *my-ontology* 'spatial-relations (let ((concept (cl-store:restore pathname)))
                                                   (Make-instance 'spatial-concept
                                                                  :id 'front
                                                                  :form "front"
                                                                  :meaning (copy-object (meaning concept))))))
              ))
       (set-data *my-ontology* 'thing (list (make-instance 'shape-concept
                                                      :id 'thing
                                                      :form "thing"
                                                      :meaning nil)))
       (push-data *my-ontology* 'booleans (make-instance 'boolean-category :id 'yes :bool t))
       (push-data *my-ontology* 'booleans (make-instance 'boolean-category :id 'no :bool nil))
       (loop for attribute in '(shape size material color)
             do (clevr-world::add-category-to-ontology *my-ontology* attribute 'attribute)))
