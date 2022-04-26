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
    (make-instance concept-class
                   :id (pathname->conceptname path)
                   :form (form concept)
                   :meaning (copy-object (meaning concept)))))


;; Make an ontology (instance of #'blackboard)
(defun make-mwm-ontology (concepts-pathname world-type)
  (let ((ontology (make-blackboard)))
    (loop for pathname in (directory concepts-pathname)
          do (cond ((member (pathname->conceptname pathname)
                            '(blue brown cyan gray green purple red yellow))
                    (push-data ontology 'colors
                               (restore-concept pathname 'color-concept)))
                   ((member (pathname->conceptname pathname) '(metal rubber))
                    (push-data ontology 'materials
                               (restore-concept pathname 'material-concept)))
                   ((member (pathname->conceptname pathname) '(cube cylinder sphere))
                    (push-data ontology 'shapes
                               (restore-concept pathname 'shape-concept)))
                   ((member (pathname->conceptname pathname) '(large small)) 
                    (push-data ontology 'sizes
                               (restore-concept pathname 'size-concept)))
                   ((member (pathname->conceptname pathname) '(left right)) 
                    (push-data ontology 'x-spatial-relations
                               (restore-concept pathname 'spatial-concept)))
                   ((member (pathname->conceptname pathname) '(front behind))
                    (push-data ontology 'yz-spatial-relations
                               (restore-concept pathname 'spatial-concept)))))
    (set-data ontology 'thing
              (list (make-instance 'shape-concept :id 'thing  :form "thing" :meaning nil)))
    (push-data ontology 'booleans (make-instance 'boolean-category :id 'yes :bool t))
    (push-data ontology 'booleans (make-instance 'boolean-category :id 'no :bool nil))
    (loop for attribute in '(shape size material color)
          do (clevr-world::add-category-to-ontology ontology attribute 'attribute))
    (set-data ontology 'world-type world-type)
    (set-data ontology 'extracted-scenes-path *extracted-scenes-path*)
    ontology))
