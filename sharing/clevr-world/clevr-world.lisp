(in-package :clevr-world)

;; This package provides an interface to work with CLEVR data
;; This can be the original CLEVR dataset, the CLEVR CoGenT set
;; or any other generated CLEVR-like dataset, as long as the
;; file structure is identical.

;; As a starting point, this package defines the global variable
;; *clevr-data-path*

;; The location of this folder is relative to the *babel-corpora*
;; path, so make sure this is set in your init-babel-user.lisp
;; file. The default *clevr-data-path* is
;; /path/to/babel-corpora/CLEVR/CLEVR-v1.0

;; The packages assumes this directory has scenes/, questions/
;; and images/ subdirectories. Each of these should again have
;; subfolders for the different splits of the data, e.g.
;; train/, val/ and test/. The scenes are assumed to be stored
;; one scene per file, one a single line. The questions are
;; assumted to be stored per question set, grouping together
;; all questions for a scene in one file, also having one
;; question per line.

;; ################################
;; global variables
;; ################################

(export '(*clevr-data-path*))

(defparameter *clevr-data-path*
  (merge-pathnames (make-pathname :directory '(:relative "CLEVR" "CLEVR-v1.0"))
                   cl-user:*babel-corpora*)
  "The root directory of the clevr data.")

;; ################################
;; generics
;; ################################

(defgeneric load-object (type filename &key &allow-other-keys)
  (:documentation "Load an object of 'type' from the file 'filename"))

(defgeneric s-expr->object (type s-expr &key &allow-other-keys)
  (:documentation "Transform the given s-expression into an object of 'type"))

(defgeneric object->s-expr (thing &key &allow-other-keys)
  (:documentation "Transform the object into an s-expression"))

;; ################################
;; clevr object
;; ################################

(export '(clevr-object shape size color material relationships
          coordinates rotation x-pos y-pos z-pos feature-values))

(defclass clevr-object (entity)
  ((shape         :type symbol :initarg :shape         :accessor shape)
   (size          :type symbol :initarg :size          :accessor size)
   (color         :type symbol :initarg :color         :accessor color)
   (material      :type symbol :initarg :material      :accessor material)
   (relationships :type list   :initarg :relationships :accessor relationships)
   (coordinates   :type list   :initarg :coordinates   :accessor coordinates)
   (rotation      :type number :initarg :rotation      :accessor rotation))
  (:documentation "An object in the CLEVR world"))

(defmethod x-pos ((object clevr-object))
  (first (coordinates object)))

(defmethod y-pos ((object clevr-object))
  (second (coordinates object)))

(defmethod z-pos ((object clevr-object))
  (third (coordinates object)))

(defmethod feature-values ((object clevr-object))
  (list (cons :shape (shape object))
        (cons :size (size object))
        (cons :color (color object))
        (cons :material (material object))))

(defun key->symbol (s-expr key)
  (internal-symb
   (upcase
    (mkstr
     (rest
      (assoc key s-expr))))))

(defmethod s-expr->object ((type (eql 'object)) s-expr
                           &key relationships id-dict-entry id-dict)
  "Create an instance of 'clevr-object from an s-expression"
  (let* ((relationships (loop for (relation . list-of-ids) in relationships
                              collect (cons relation
                                            (loop for id in list-of-ids
                                                  collect (rest (assoc id id-dict)))))))
    (make-instance 'clevr-object :id (cdr id-dict-entry)
                   :shape (key->symbol s-expr :shape)
                   :size (key->symbol s-expr :size)
                   :color (key->symbol s-expr :color)
                   :material (key->symbol s-expr :material)
                   :relationships relationships
                   :coordinates (rest (assoc :pixel--coords s-expr))
                   :rotation (rest (assoc :rotation s-expr)))))

(defmethod object->s-expr ((object clevr-object) &key)
  "Returns a clevr-object as an s-expr of the form
   (<type> <id> ((<fname> (<feature-type> <value>))
                 (<fname> (<feature-type> <value>))"
  (list 'object (id object)
        (list (cons 'shape (shape object))
              (cons 'size (size object))
              (cons 'color (color object))
              (cons 'material (material object)))))

(defmethod copy-object ((obj clevr-object))
  (make-instance 'clevr-object :id (id obj)
                 :shape (shape obj) :size (size obj)
                 :color (color obj) :material (material obj)
                 :relationships (copy-object (relationships obj))
                 :coordinates (copy-object (coordinates obj))
                 :rotation (copy-object (rotation obj))))

(defmethod print-object ((object clevr-object) stream)
  (if *print-pretty*
      (pprint-logical-block (stream nil)
	(format stream "~(~:w~)" (object->s-expr object)))
      (call-next-method)))

;; ################################
;; clevr scene
;; ################################

(export '(clevr-scene objects index name source-path data-set image
          equal-entity find-entity-by-id empty-set-p))

(defclass clevr-scene (entity)
  ((objects     :type list     :initarg :objects     :accessor objects)
   (index       :type number   :initarg :index       :accessor index)
   (name        :type string   :initarg :name        :accessor name)
   (source-path :type pathname :initarg :source-path :accessor source-path)
   (data-set    :type string   :initarg :data-set    :accessor data-set)
   (image       :type string   :initarg :image       :accessor image))
  (:documentation "A scene in the CLEVR world"))

(defmethod initialize-instance :around ((scene clevr-scene) &rest initargs &key id)
  (apply #'call-next-method scene :id (or id (make-id 'scene)) initargs))

(defmethod equal-entity ((scene-1 clevr-scene)
                         (scene-2 clevr-scene))
  "Two scenes are equal if they are a permutation of each other.
   #'equal-entity compares the IDs of the objects"
  (permutation-of? (objects scene-1) (objects scene-1) :test #'equal-entity))

(defmethod find-entity-by-id ((scene clevr-scene) (id symbol))
  (find id (objects scene) :key #'id))

(defmethod empty-set-p ((scene clevr-scene))
  "Check if a given scene is empty"
  (null (objects scene)))

(defmethod load-object ((type (eql 'scene)) filename &key)
  "Load a scene from file"
  (let ((s-expr
         (with-open-file (stream filename :direction :input)
           (first (mapcar #'decode-json-from-string
                          (stream->list stream))))))
    (s-expr->object 'scene s-expr :directory filename)))

(defun collect-relations-for-object (all-relationships index)
  "Collect the spatial relationships for a single object"
  (loop for (key . list-of-lists) in all-relationships
        for relationship = (internal-symb (upcase (mkstr key)))
        collect (cons relationship (nth index list-of-lists))))

(defmethod s-expr->object ((type (eql 'scene)) s-expr
                           &key directory)
  "Create an instance of clevr-scene from an s-expression"
  (let* ((all-objects (rest (assoc :objects s-expr)))
         (all-relationships (rest (assoc :relationships s-expr)))
         (id-dict (loop for i from 0 below (length all-objects)
                        collect (cons i (make-id 'obj))))
         (data-set (last-elt (pathname-directory directory))))
    (make-instance 'clevr-scene
                   :index (rest (assoc :image--index s-expr))
                   :source-path directory
                   :name (pathname-name directory)
                   :data-set (last-elt (pathname-directory directory))
                   :image (merge-pathnames
                           (make-pathname :directory `(:relative "images" ,data-set)
                                          :name (rest (assoc :image--filename s-expr)))
                           *clevr-data-path*)
                   :objects (loop for object in all-objects
                                  for (index . id) in id-dict
                                  for object-relationships
                                  = (collect-relations-for-object
                                     all-relationships index)
                                  collect (s-expr->object 'object object
                                                          :relationships object-relationships
                                                          :id-dict-entry (cons index id)
                                                          :id-dict id-dict)))))

(defmethod object->s-expr ((scene clevr-scene) &key)
  (loop for object in (objects scene)
        collect (object->s-expr object)))

(defmethod copy-object ((scene clevr-scene))
  (make-instance 'clevr-scene :id (id scene)
                 :index (copy-object (index scene))
                 :source-path (copy-object (source-path scene))
                 :data-set (copy-object (data-set scene))
                 :image (copy-object (image scene))
                 :objects (copy-object (objects scene))))

(defmethod print-object ((scene clevr-scene) stream)
  (if *print-pretty*
      (pprint-logical-block (stream nil)
        (format stream "<clevr-scene:~
                        ~:_ index: ~a,~:_ name: ~a,~:_ source-path: ~a,~:_ data-set: ~a,~:_ entities:~:_ ~:w"
                (index scene) (name scene) (source-path scene) (data-set scene) (objects scene))
        (format stream ">"))
      (format stream "<clevr-scene: ~a>" (id scene))))

;; ################################
;; clevr program node
;; ################################

(export '(clevr-function function-name args))

(defclass clevr-function (entity tree-node)
  ((function-name :type string :initarg :function-name :accessor function-name)
   (args          :type list   :initarg :args          :accessor args))
  (:documentation "A function in a CLEVR program"))

(defmethod initialize-instance :around ((function clevr-function) &rest initargs &key id)
  (apply #'call-next-method function :id (or id (make-id 'function)) initargs))

(defmethod s-expr->object ((type (eql 'function)) s-expr &key i)
  (make-instance 'clevr-function :id i
                 :function-name (rest (assoc :function s-expr))
                 :args (rest (assoc :value--inputs s-expr))
                 :children (rest (assoc :inputs s-expr))))

;; to do
;; object->s-expr, copy-object and print-object
   

;; ################################
;; clevr program
;; ################################

(export '(clevr-program))

(defclass clevr-program (entity tree) ()
  (:documentation "A program in the CLEVR world"))

(defmethod initialize-instance :around ((program clevr-program) &rest initargs &key id)
  (apply #'call-next-method program :id (or id (make-id 'program)) initargs))

(defun tree-insert (program all-functions node &optional parent)
  (let* ((children-ids (children node))
         (children (loop for id in children-ids
                         collect (find id all-functions :key #'id :test #'=))))
    (setf (children node) nil)
    (unless (find-node program (id node) :key #'id :test #'=)
      (add-node program node :parent parent))
    (loop for child in children
          do (tree-insert program all-functions child node))))

(defmethod s-expr->object ((type (eql 'program)) s-expr &key)
  (let ((list-of-functions
         (loop for function in s-expr
               for i from 0
               collect (s-expr->object 'function function :i i)))
        (program (make-instance 'clevr-program)))
    (tree-insert program list-of-functions
                 (first (reverse list-of-functions)))
    program))

;; to do
;; object->s-expr, copy-object and print-object

;; ################################
;; clevr question
;; ################################

(export '(clevr-question question answer program index
          scene-index template question-family))

(defclass clevr-question (entity)
  ((question        :type string :initarg :question        :accessor question)
   (answer                       :initarg :answer          :accessor answer)
   (program         :type list   :initarg :program         :accessor program)
   (index           :type number :initarg :index           :accessor index)
   (scene-index     :type number :initarg :scene-index     :accessor scene-index)
   (template        :type string :initarg :template        :accessor template)
   (question-family :type number :initarg :question-family :accessor question-family))
  (:documentation "A question in the CLEVR world"))

(defmethod initialize-instance :around ((q clevr-question) &rest initargs &key id)
  (apply #'call-next-method q :id (or id (make-id 'question)) initargs))

(defmethod s-expr->object ((type (eql 'question)) s-expr &key)
  (make-instance 'clevr-question
                 :index (rest (assoc :question--index s-expr))
                 :question (downcase (rest (assoc :question s-expr)))
                 :scene-index (rest (assoc :image--index s-expr))
                 :answer (rest (assoc :answer s-expr))
                 :program (s-expr->object 'program
                                          (rest (assoc :program s-expr)))
                 :template (rest (assoc :template--filename s-expr))
                 :question-family (rest (assoc :question--family--index s-expr))))

(defmethod object->s-expr ((q clevr-question) &key)
  "Returns a clevr-question as an s-expr of the form
   (<type> <id> <question> <answer>)"
  (list 'question (id q) (question q) (answer q)))

(defmethod copy-object ((q clevr-question))
  (make-instance 'clevr-question :id (id q)
                 :question (question q) :answer (answer q)
                 :program (program q) :index (index q)
                 :scene-index (scene-index q) :template (template q)
                 :question-family (question-family q)))

(defmethod print-object ((q clevr-question) stream)
  (if *print-pretty*
      (pprint-logical-block (stream nil)
	(format stream "~(~:w~)" (object->s-expr q)))
      (call-next-method)))

;; ################################
;; clevr question set
;; ################################

(export '(clevr-question-set question scene-index source-path data-set))

(defclass clevr-question-set (entity)
  ((questions    :type list     :initarg :questions   :accessor questions)
   (scene-index  :type number   :initarg :scene-index :accessor scene-index)
   (source-path  :type pathname :initarg :source-path :accessor source-path)
   (data-set     :type string   :initarg :data-set    :accessor data-set))
  (:documentation "A question set groups all the questions for a CLEVR scene"))

(defmethod initialize-instance :around ((set clevr-question-set) &rest initargs &key id)
  (apply #'call-next-method set :id (or id (make-id 'question-set)) initargs))

(defmethod load-object ((type (eql 'question-set)) filename &key)
  "Load a question-set from file"
  (let ((s-expr
         (with-open-file (stream filename :direction :input)
           (mapcar #'decode-json-from-string
                   (stream->list stream)))))
    (s-expr->object 'question-set s-expr :directory filename)))

(defmethod s-expr->object ((type (eql 'question-set)) s-expr
                           &key directory)
  (let ((scene-index
         (rest (assoc :image--index (first s-expr))))
        (data-set
         (last-elt (pathname-directory directory))))
    (make-instance 'clevr-question-set
                   :scene-index scene-index
                   :source-path directory
                   :data-set data-set
                   :questions (loop for expr in s-expr
                                    collect (s-expr->object 'question expr)))))

(defmethod object->s-expr ((set clevr-question-set) &key)
  (loop for q in (questions set)
        collect (object->s-expr q)))

(defmethod copy-object ((set clevr-question-set))
  (make-instance 'clevr-question-set :id (id set)
                 :scene-index (scene-index set)
                 :source-path (source-path set)
                 :data-set (data-set set)
                 :questions (copy-object (questions set))))

(defmethod print-object ((set clevr-question-set) stream)
  (if *print-pretty*
      (pprint-logical-block (stream nil)
        (format stream "<clevr-question-set:~
                        ~:_ scene-index: ~a,~:_ source-path: ~a,~:_ data-set: ~a,~:_ questions:~:_ ~:w"
                (scene-index set) (source-path set) (data-set set) (questions set))
        (format stream ">"))
      (format stream "<clevr-question-set: ~a>" (id set))))

;; ################################
;; clevr world
;; ################################

(export '(clevr-world scenes questions data-sets current-scene
          next-scene questions-for-scene))

(defclass clevr-world (entity)
  ((scenes        :type list :initarg :scenes    :accessor scenes)
   (questions     :type list :initarg :questions :accessor questions :initform nil)
   (data-sets     :type list :initarg :data-sets :accessor data-sets)
   (current-scene :type (or null clevr-scene) :initform nil :accessor current-scene))
  (:documentation "The CLEVR world"))

(defmethod initialize-instance :around ((world clevr-world)
                                        &key (data-sets nil)
                                        (exclude-scenes nil)
                                        (load-questions nil))
  ;; check for *clevr-data-path*
  (unless (probe-file *clevr-data-path*)
    (error "Could not find the 'clevr-data-path' directory in ~%~a" *clevr-data-path*))
  ;; check data-sets
  (unless (and (listp data-sets)
               (length> data-sets 0)
               (apply #'always (mapcar #'stringp data-sets)))
    (error "Keyword argument :data-sets should be a list of strings"))
  ;; initialize the instance
  (call-next-method)
  ;; load the scenes
  (let ((clevr-scenes-path
         (merge-pathnames (make-pathname :directory '(:relative "scenes"))
                          *clevr-data-path*)))
    (unless (probe-file clevr-scenes-path)
      (error "Could not find a 'scenes' subdirectory in ~a~%" *clevr-data-path*))
    (setf (slot-value world 'scenes)
          (loop with files
                = (loop for data-set in data-sets
                        for set-directory = (merge-pathnames (make-pathname :directory `(:relative ,data-set))
                                                             clevr-scenes-path)
                        unless (probe-file set-directory)
                        do (error "~a is not a subdirectory of ~%~a" data-set clevr-scenes-path)
                        append (directory (make-pathname :directory (pathname-directory set-directory)
                                                         :name :wild :type "json")))
                for file in files
                for scene-name = (pathname-name file)
                for scene = (unless (find scene-name exclude-scenes :test #'equal)
                              (load-object 'scene file))
                when scene collect scene)))
  ;; load the questions
  (when load-questions
    (let ((clevr-questions-path
           (merge-pathnames (make-pathname :directory '(:relative "questions"))
                            *clevr-data-path*)))
      (unless (probe-file clevr-questions-path)
        (error "Could not find a 'questions' subdirectory in ~a~%" *clevr-data-path*))
      (setf (slot-value world 'questions)
            (loop with files
                  = (loop for data-set in data-sets
                          for set-directory = (merge-pathnames (make-pathname :directory `(:relative ,data-set))
                                                               clevr-questions-path)
                          unless (probe-file set-directory)
                          do (error "~a is not a subdirectory of ~%~a" data-set clevr-questions-path)
                          append (directory (make-pathname :directory (pathname-directory set-directory)
                                                           :name :wild :type "json")))
                  for file in files
                  for question-set = (load-object 'question-set file)
                  when question-set collect question-set))))
  ;; set the current scene
  (if (scenes world)
    (setf (current-scene world) (random-elt (scenes world)))
    (warn "No scenes were loaded.")))

(defmethod next-scene ((world clevr-world))
  "Return a random scene"
  (setf (current-scene world)
        (random-elt (scenes world)))
  (current-scene world))

(defgeneric questions-for-scene (world scene)
  (:documentation "Fetch all questions for the given scene"))

(defmethod questions-for-scene ((world clevr-world) (scene clevr-scene))
  ;; Scenes from different data-sets can have the same ID (since these)
  ;; are simply numbers counting from 0. Therefore, if multiple scenes
  ;; with the given ID are found, check the data-set slot
  (unless (questions world)
    (error "No questions were loaded."))
  (let ((found (find-all (index scene) (questions world) :key #'scene-index)))
    (if (length> found 1)
      (find (data-set scene) found :key #'data-set :test #'string=)
      (first found))))

(defmethod questions-for-scene ((world clevr-world) (scene symbol))
  ;; when using the scene-id
  (let ((scene-object (find scene (scenes world) :key #'id)))
    (questions-for-scene world scene-object)))

(defmethod questions-for-scene ((world clevr-world) (scene string))
  ;; when using the scene-name
  (let ((scene-object (find scene (scenes world) :key #'name :test #'string=)))
    (questions-for-scene world scene-object)))

(defmethod questions-for-scene ((world clevr-world) (scene number))
  ;; when using the scene-index
  (let ((scene-object (find scene (scenes world) :key #'index :test #'=)))
    (questions-for-scene world scene-object)))


(defmethod copy-object ((world clevr-world)) world)

(defmethod print-object ((world clevr-world) stream)
  (if *print-pretty*
      (pprint-logical-block (stream nil)
        (if (questions world)
          (format stream "<clevr-world:~:_ scenes: ~a,~:_ questions: ~a,~:_ current-scene: ~a,~:_ "
                  (mapcar #'name (scenes world))
                  (mapcar #'id (questions world))
                  (when (current-scene world)
                    (id (current-scene world))))
          (format stream "<clevr-world:~:_ scenes: ~a,~:_ current-scene: ~a,~:_ "
                  (mapcar #'name (scenes world))
                  (when (current-scene world)
                    (id (current-scene world)))))
        (format stream ">"))
      (call-next-method)))

;; ################################
;; utils
;; ################################

(export '(load-clevr-scene load-clevr-question-set))

(defun load-clevr-scene (filename)
  (load-object 'scene filename))

(defun load-clevr-question-set (filename)
  (load-object 'question-set filename))


;;;; Testing
;(defparameter *world* (make-instance 'clevr-world :data-sets '("val") :load-questions t))