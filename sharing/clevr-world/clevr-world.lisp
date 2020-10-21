(in-package :clevr-world)

;; The :clevr-world package provides a high-level API
;; for handling the clevr data. See the README for
;; information about the data storage format, the API
;; and the class structure.

;; ################################
;; global variables
;; ################################

(export '(*clevr-data-path* reset-clevr-data-path))

(defparameter *clevr-data-path*
  (merge-pathnames (make-pathname :directory '(:relative "CLEVR-v1.0"))
                   cl-user:*babel-corpora*)
  "The root directory of the clevr data.")

(defun reset-clevr-data-path ()
  (setf *clevr-data-path*
        (merge-pathnames (make-pathname :directory '(:relative "CLEVR-v1.0"))
                         cl-user:*babel-corpora*)))

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

(export '(clevr-object-set objects equal-entity find-entity-by-id empty-set-p
          clevr-scene index name source-path data-set image))

(defclass clevr-object-set (entity)
  ((objects :type list :initarg :objects :accessor objects :initform nil))
  (:documentation "A set of clevr-object instances"))

(defclass clevr-scene (clevr-object-set)
  ((index       :type number   :initarg :index       :accessor index)
   (name        :type string   :initarg :name        :accessor name)
   (source-path :type pathname :initarg :source-path :accessor source-path)
   (data-set    :type string   :initarg :data-set    :accessor data-set)
   (image       :type pathname :initarg :image       :accessor image))
  (:documentation "A scene in the CLEVR world"))

(defmethod initialize-instance :around ((set clevr-object-set) &rest initargs &key id)
  (apply #'call-next-method set :id (or id (make-id 'set)) initargs))

(defmethod initialize-instance :around ((scene clevr-scene) &rest initargs &key id)
  (apply #'call-next-method scene :id (or id (make-id 'scene)) initargs))

(defmethod equal-entity ((set-1 clevr-object-set)
                         (set-2 clevr-object-set))
  "Two sets are equal if they are a permutation of each other.
   #'equal-entity compares the IDs of the objects"
  (permutation-of? (objects set-1) (objects set-2) :test #'equal-entity))

(defmethod find-entity-by-id ((set clevr-object-set) (id symbol))
  "Find an entity in the set"
  (find id (objects set) :key #'id))

(defmethod empty-set-p ((set clevr-object-set))
  "Check if a given set is empty"
  (null (objects set)))

(defmethod load-object ((type (eql 'scene)) filename &key)
  "Load a scene from file"
  (let ((s-expr (decode-json-from-source filename)))
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
         (data-set (last-elt (pathname-directory directory)))
         (image-filename-and-type (split (rest (assoc :image--filename s-expr)) #\.))
         (img-filename (first image-filename-and-type))
         (img-filetype (second image-filename-and-type)))
    (make-instance 'clevr-scene
                   :index (rest (assoc :image--index s-expr))
                   :source-path directory
                   :name (pathname-name directory)
                   :data-set (last-elt (pathname-directory directory))
                   :image (merge-pathnames
                           (make-pathname :directory `(:relative "images" ,data-set)
                                          :name img-filename :type img-filetype)
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
  ((id :type (or symbol number) :initarg :id :accessor id)
   (function-name :type symbol :initarg :function-name :accessor function-name)
   (args          :type list   :initarg :args          :accessor args          :initform nil))
  (:documentation "A function in a CLEVR program"))

(defmethod initialize-instance :around ((function clevr-function) &rest initargs &key id)
  (apply #'call-next-method function :id (or id (make-id 'function)) initargs))

(defmethod s-expr->object ((type (eql 'function)) s-expr &key)
  (make-instance 'clevr-function
                 :function-name (internal-symb
                                 (upcase
                                  (cond ((assoc :function s-expr)
                                         (rest (assoc :function s-expr)))
                                        ((assoc :type s-expr)
                                         (rest (assoc :type s-expr))))))
                 :args (mapcar #'internal-symb
                               (mapcar #'upcase
                                       (rest (assoc :value--inputs s-expr))))
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
  (let* ((children-indexes (children node))
         (children (loop for index in children-indexes
                         collect (nth index all-functions))))
                         ;collect (find id all-functions :key #'id :test #'=))))
    (setf (children node) nil)
    (unless (find-node program (id node) :key #'id)
      (add-node program node :parent parent))
    (loop for child in children
          do (tree-insert program all-functions child node))))

(defmethod s-expr->object ((type (eql 'program)) s-expr &key)
  (let ((list-of-functions
         (loop for function in s-expr
               collect (s-expr->object 'function function)))
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
   (program                      :initarg :program         :accessor program)
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
                 :template (or (rest (assoc :template--filename s-expr)) "")
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

(export '(clevr-question-set questions scene-index source-path data-set))

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

(export '(clevr-world scenes question-sets data-sets current-scene
          current-question-set random-scene all-scenes all-questions
          all-scenes-and-questions do-for-scenes
          do-for-scenes-and-questions get-scene-by-index
          find-scene-by-name))

(defclass clevr-world (entity)
  ((scenes        :type list :initarg :scenes        :accessor scenes)
   (question-sets :type list :initarg :question-sets :accessor question-sets :initform nil)
   (data-sets     :type list :initarg :data-sets     :accessor data-sets)
   (current-scene :type (or null clevr-scene) :initform nil :accessor current-scene)
   (current-question-set :type (or null clevr-question-set) :initform nil :accessor current-question-set))
  (:documentation "The CLEVR world"))

(defmethod initialize-instance :around ((world clevr-world)
                                        &key (data-sets nil)
                                        (exclude-scenes nil)
                                        (load-questions nil)
                                        (question-data-sets nil))
  "Initialize the clevr world. Specify which data sets to load (e.g. (\"val\" \"train\"))
   You can specify which scenes to exclude by their name (e.g. CLEVR_val_000002)
   You can specify whether or not to also load the questions.
   By default, the questions will be loaded from the same data-sets/sub-folders as
   specified in data-sets. If this should be different, these can be specified
   using question-data-sets."
  ;; check for *clevr-data-path*
  (unless (probe-file *clevr-data-path*)
    (error "Could not find the 'clevr-data-path' directory in ~%~a" *clevr-data-path*))
  ;; check data-sets
  (unless (and (listp data-sets)
               (length> data-sets 0)
               (apply #'always (mapcar #'stringp data-sets)))
    (error "Keyword argument :data-sets should be a list of strings"))
  ;; when provided, check question-data-sets
  (when question-data-sets
    (unless (and (listp question-data-sets)
                 (length> question-data-sets 0)
                 (apply #'always (mapcar #'stringp data-sets)))
      (error "Keyword argument :question-data-sets should be a list of strings")))
  ;; initialize the instance
  (call-next-method)
  ;; load the scenes
  (let ((clevr-scenes-path
         (merge-pathnames (make-pathname :directory '(:relative "scenes"))
                          *clevr-data-path*)))
    (unless (probe-file clevr-scenes-path)
      (error "Could not find a 'scenes' subdirectory in ~a~%" *clevr-data-path*))
    (setf (slot-value world 'scenes)
          (loop for data-set in data-sets
                for set-directory = (merge-pathnames (make-pathname :directory `(:relative ,data-set))
                                                     clevr-scenes-path)
                unless (probe-file set-directory)
                do (error "~a is not a subdirectory of ~%~a" data-set clevr-scenes-path)
                append (sort (directory
                              (make-pathname :directory (pathname-directory set-directory)
                                             :name :wild :type "json"))
                             #'string< :key #'namestring))))                
  ;; load the questions, if requested
  (when load-questions
    (let ((clevr-questions-path
           (merge-pathnames (make-pathname :directory '(:relative "questions"))
                            *clevr-data-path*)))
      (unless (probe-file clevr-questions-path)
        (error "Could not find a 'questions' subdirectory in ~a~%" *clevr-data-path*))
      (setf (slot-value world 'question-sets)
            (loop for data-set in (or question-data-sets data-sets)
                  for set-directory = (merge-pathnames (make-pathname :directory `(:relative ,data-set))
                                                       clevr-questions-path)
                  unless (probe-file set-directory)
                  do (error "~a is not a subdirectory of ~%~a" data-set clevr-questions-path)
                  append (sort (directory
                                (make-pathname :directory (pathname-directory set-directory)
                                               :name :wild :type "json"))
                               #'string< :key #'namestring)))))
  ;; exclude scenes
  (loop for scene in exclude-scenes
        for scene-position = (position scene (scenes world) :key #'pathname-name :test #'string=)
        for exclude-question = (nth scene-position (questions world))
        do (setf (scenes world) (remove scene (scenes world) :key #'pathname-name :test #'string=))
        do (setf (questions world) (remove exclude-question (questions world))))
  ;; set the current scene
  (if (scenes world)
    (setf (current-scene world)
          (load-object 'scene (random-elt (scenes world))))
    (warn "No scenes were loaded."))
  ;; set the current question set
  (when load-questions
    (if (question-sets world)
      (let* ((scene-index (position (source-path (current-scene world)) (scenes world)))
             (question-set-path (nth scene-index (question-sets world))))
        (setf (current-question-set world)
              (load-object 'question-set question-set-path)))
      (warn "No question sets loaded."))))


(defmethod get-scene-by-index ((world clevr-world) index)
  "Get a particular scene by its index"
  (assert (and (>= index 0) (< index (length (scenes world)))))
  (setf (current-scene world)
        (load-object 'scene (nth index (scenes world))))
  (when (question-sets world)
    (setf (current-question-set world)
          (load-object 'question-set (nth index (question-sets world)))))
  (values (current-scene world)
          (current-question-set world)))


(defmethod find-scene-by-name (name (world clevr-world))
  (let ((filename (find name (scenes world) :key #'namestring :test #'search)))
    (when filename
      (setf (current-scene world)
            (load-object 'scene filename))
      (when (question-sets world)
        (let* ((scene-index (position (source-path (current-scene world)) (scenes world)))
               (question-set-path (nth scene-index (question-sets world))))
          (setf (current-question-set world)
                (load-object 'question-set question-set-path))))
      (values (current-scene world)
              (current-question-set world)))))


(defmethod random-scene ((world clevr-world))
  "Choose a random scene and load it into memory.
   If possible, load the associated question-set
   as well."
  (setf (current-scene world)
        (load-object 'scene (random-elt (scenes world))))
  (when (question-sets world)
    (let* ((scene-index (position (source-path (current-scene world)) (scenes world)))
           (question-set-path (nth scene-index (question-sets world))))
      (setf (current-question-set world)
            (load-object 'question-set question-set-path))))
  (values (current-scene world)
          (current-question-set world)))

(defmethod all-scenes ((world clevr-world))
  "Loads all scenes into memory and returns them in a flat list"
  (loop for scene-path in (scenes world)
        collect (load-object 'scene scene-path)))

(defmethod all-questions ((world clevr-world))
  "Loads all questions into memory and returns them in a flat list"
  (loop for q-set-path in (question-sets world)
        for q-set = (load-object 'question-set q-set-path)
        append (questions q-set)))

(defmethod all-scenes-and-questions ((world clevr-world))
  "Loads all scenes and all questions into memory.
   Returns a list of (scene . question-set) pairs"
  (loop for scene-path in (scenes world)
        for q-set-path in (question-sets world)
        collect (cons (load-object 'scene scene-path)
                      (load-object 'question-set q-set-path))))

(defmethod do-for-scenes ((world clevr-world) fn &key shuffled)
  "Do function 'fn' for each scene. Stop when 'fn' returns NIL"
  ;; this could be a macro
  (loop for scene-file in (if shuffled (shuffle (scenes world)) (scenes world))
        for scene = (load-object 'scene scene-file)
        for result = (funcall fn scene)
        unless result
        return nil))

(defun identical-shuffle (&rest lists)
  "Shuffle a number of lists in an identical manner"
  (assert
      (loop with len = (length (first lists))
            for list in lists
            always (= (length list) len)))
  (let* ((list-1 (first lists))
         (index (iota (length list-1)))
         (shuffled-index (shuffle index)))
    (apply #'values
           (loop for list in lists
                 collect (loop for i in shuffled-index
                               collect (nth i list))))))

(defmethod do-for-scenes-and-questions ((world clevr-world) fn &key shuffled)
  "Do function 'fn' for each scene and question-set.
   Stop when 'fn' returns NIL."
  ;; this could be a macro
  (multiple-value-bind (scenes question-sets)
      (if shuffled
        (identical-shuffle (scenes world) (question-sets world))
        (values (scenes world) (question-sets world)))
    (loop for scene-file in scenes
          for q-set-file in question-sets
          for scene = (load-object 'scene scene-file)
          for q-set = (load-object 'question-set q-set-file)
          for result = (funcall fn scene q-set)
          unless result
          return nil)))

(defmethod copy-object ((world clevr-world)) world)

(defmethod print-object ((world clevr-world) stream)
  (if *print-pretty*
      (pprint-logical-block (stream nil)
        (if (question-sets world)
          (format stream "<clevr-world:~:_ scenes: ~a,~:_ questions: ~a,~:_ current-scene: ~a,~:_ "
                  (mapcar #'namestring (scenes world))
                  (mapcar #'namestring (question-sets world))
                  (when (current-scene world)
                    (name (current-scene world))))
          (format stream "<clevr-world:~:_ scenes: ~a,~:_ current-scene: ~a,~:_ "
                  (mapcar #'name (scenes world))
                  (when (current-scene world)
                    (id (current-scene world)))))
        (format stream ">"))
      (call-next-method)))

;; ################################
;; utils
;; ################################

(export '(complete-digits load-clevr-scene load-clevr-question-set))

(defun complete-digits (index)
  "Given an index (as string), prepend zeros
   until it is 6 digits long"
  (let* ((nr-of-zeros (- 6 (length index)))
         (zeros (loop repeat nr-of-zeros collect "0")))
    (list-of-strings->string (append zeros (list index)) :separator "")))

(defun load-clevr-scene (filename)
  "utility function for loading a scene separately"
  (load-object 'scene filename))

(defun load-clevr-question-set (filename)
  "utility function for loading a question-set separately"
  (load-object 'question-set filename))


;;;; Testing
;(defparameter *world* (make-instance 'clevr-world :data-sets '("val") :load-questions nil))