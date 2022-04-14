(in-package :visual-dialog)

;; ################################
;; object
;; ################################

(defclass object (object-or-set)
  ((attributes :type (or symbol list) :initarg :attributes :initform '() :accessor attributes)
   (attention :type attention :initarg :attention :initform nil :accessor attention)
   (relationships :type (or symbol list) :initarg :relationships :initform (make-var 'relationships) :accessor relationships)
   (topic :type symbol :initarg :topic :initform 'no :accessor topic)
   (coordinates :type list :initarg :coordinates :initform nil :accessor coordinates)
   (rotation :type (or null number) :initarg :rotation :initform nil :accessor rotation))
  (:documentation "An object in the world, it can have attributes, relationships and it can be the topic or not"))

;; ################################
;; object set
;; ################################

(defclass object-or-set (entity) ())

(defclass object-set (object-or-set)
  ((objects :type list :initarg :objects :accessor objects :initform nil)
   (scene-configuration :type (or null relation-set) :accessor scene-configuration :initarg :scene-configuration :initform nil)
   (image-index :type (or null number) :initarg :image-index :accessor image-index :initform nil)
   (image-filename :type (or null string) :initarg :image-filename :accessor image-filename :initform nil))
  (:documentation  "A set of objects in the CLEVR world"))

(defclass number-or-bool (entity) ())

;; ################################
;; dialog 
;; ################################

(defclass dialog (entity)
  ((caption :type (or nil string) :initarg :caption :accessor caption)
   (questions :type list :initarg :questions :accessor questions)
   (answers :type list :initarg :answers :accessor answers)
   ;(partial-scene-graph :type list :initarg :partial-scene-graph :accessor partial-scene-graph)
   )
  (:documentation "A dialog in the world"))

;; ################################
;; dialog set
;; ################################

(defclass dialog-set (entity)
  ((dialogs      :type list     :initarg :dialogs     :accessor dialogs)
   (scene-index  :type number   :initarg :scene-index :accessor scene-index)
   (source-path  :type pathname :initarg :source-path :accessor source-path)
   (data-set     :type string   :initarg :data-set    :accessor data-set))
  (:documentation "A dialog set groups all the dialogs for a scene"))

;; ################################
;; scene
;; ################################

(defclass scene (object-set)
  ((index       :type (or null number)   :initarg :index       :accessor index)
   (name        :type (or null string)   :initarg :name        :accessor name)
   (source-path :type (or null pathname) :initarg :source-path :accessor source-path)
   (data-set    :type (or null string)   :initarg :data-set    :accessor data-set)
   (image       :type(or null pathname) :initarg :image       :accessor image))
  (:documentation "A scene in the world"))

;; ################################
;; world
;; ################################

(defclass world (entity configuration)
  (;(dataset :type (or 'clevr 'mnist) :initarg :dataset :initform nil :accessor dataset)
   (scenes        :type list :initarg :scenes    :initform nil    :accessor scenes)
   (dialog-sets   :type list :initarg :dialog-sets   :accessor dialog-sets   :initform nil)
   (datasplit     :type list :initarg :datasplit     :accessor datasplit)
   (current-scene :type (or null scene) :initform nil :accessor current-scene)
   (current-dialog-set :type (or null dialog-set) :initform nil :accessor current-dialog-set))
  (:documentation "The world"))

(defmethod initialize-instance :after ((world world) &key )
  "Initialize the clevr world. Specify which data sets to load (e.g. (\"val\" \"train\"))
   You can specify which scenes to exclude by their name (e.g. CLEVR_val_000002)
   You can specify whether or not to also load the questions.
   By default, the questions will be loaded from the same data-sets/sub-folders as
   specified in data-sets. If this should be different, these can be specified
   using question-data-sets."
  (let ((data-path
         (if (eql (get-configuration world :dataset) :clevr)
            *clevr-data-path*
            *mnist-data-path*))
        (datasplit (cond ((eql (get-configuration world :datasplit) :train)
                          (list "train"))
                         ((eql (get-configuration world :datasplit) :val)
                          (list "val"))
                         ((eql (get-configuration world :datasplit) :test)
                          (list "test")))))
  ;; check for *clevr-data-path*
  (unless (probe-file data-path)
    (error "Could not find the 'data-path' directory in ~%~a" data-path))
  ;; check data-sets
  (unless (and (listp datasplit)
               (length> datasplit 0)
               (apply #'always (mapcar #'stringp datasplit)))
    (error "Keyword argument :data-sets should be a list of strings"))
  ;; load the scenes
  (let ((scenes-path
         (if (eql (get-configuration world :mode) :symbolic)
           (merge-pathnames (make-pathname :directory '(:relative "scenes"))
                            data-path)
           (merge-pathnames (make-pathname :directory '(:relative "images"))
                            data-path))))
    (unless (probe-file scenes-path)
      (error "Could not find a 'scenes' subdirectory in ~a~%" data-path))
    (setf (slot-value world 'scenes)
          (loop for data-set in datasplit
                for set-directory = (merge-pathnames (make-pathname :directory `(:relative ,data-set))
                                                     scenes-path)
                unless (probe-file set-directory)
                do (error "~a is not a subdirectory of ~%~a" data-set scenes-path)
                append (sort (directory
                              (make-pathname :directory (pathname-directory set-directory)
                                             :name :wild :type (if (eql (get-configuration world :mode) :symbolic) "json" "png")))
                             #'string< :key #'namestring))))
  ;; load the dialogs, if requested
    (let ((dialogs-path
           (merge-pathnames (make-pathname :directory '(:relative "dialogs"))
                            data-path)))
      (unless (probe-file dialogs-path)
        (error "Could not find a 'dialogs' subdirectory in ~a~%" data-path))
      (setf (slot-value world 'dialog-sets)
            (loop for data-set in datasplit
                  for set-directory = (merge-pathnames (make-pathname :directory `(:relative ,data-set))
                                                       dialogs-path)
                  unless (probe-file set-directory)
                  do (error "~a is not a subdirectory of ~%~a" data-set dialogs-path)
                  append (sort (directory
                                (make-pathname :directory (pathname-directory set-directory)
                                               :name :wild :type "json"))
                               #'string< :key #'namestring))))
  #|;; set the current scene
  (if (scenes world)
    (setf (current-scene world)
          (load-object 'scene (random-elt (scenes world)) :dataset dataset))
          ; (random-elt (scenes world)))
    (warn "No scenes were loaded."))
  ;; set the current dialog set
  (when load-dialogs
    (if (dialog-sets world)
      (let* ((scene-index (position (source-path (current-scene world)) (scenes world)))
             (dialog-set-path (nth scene-index (dialog-sets world))))
        (setf (current-dialog-set world)
              (if (eq dataset 'clevr)
                (load-object 'dialog-set dialog-set-path :dataset dataset)
                (load-object 'dialog-set dialog-set-path :dataset dataset))
              ))
      (warn "No dialog sets loaded.")))|#))

;; ################################
;; world model
;; ################################

; A world model has SET-ITEMS which is a list of TURNS
(defclass world-model (object-or-set)
  ((set-items :type list :initarg :set-items :accessor set-items :initform nil)
   (dataset :type symbol :initarg :dataset :accessor dataset :initform nil)
   (path :type path :initarg :path :accessor path :initform nil)
   ))

(defmethod make-context ((world world))
  (make-instance 'world-model
                 :id 'context
                 ;:dataset (dataset world)
                 :path (image (current-scene world))
                 :set-items
                 (list
                  (make-instance 'turn
                                 :timestamp 'permanent
                                 :object-set
                                 (make-instance 'object-set
                                                :objects (loop for object in (objects (current-scene world))
                                                               collect (copy-object-no-relations object))
                                                :scene-configuration (if (eql (get-configuration world :dataset) :clevr)
                                                                       (make-clevr-scene-configuration world)
                                                                       (make-mnist-scene-configuration world)
                                                ))))))

;; ################################
;; relation set
;; ################################

(defclass relation-set (object-or-set)
  ((leftmost :type symbol :initarg :leftmost :initform nil :accessor leftmost)
   (rightmost :type symbol :initarg :rightmost :initform nil :accessor rightmost)
   (most-in-front :type symbol :initarg :most-in-front :initform nil :accessor most-in-front)
   (most-in-back :type symbol :initarg :most-in-back :initform nil :accessor most-in-back)
   (immediate-right :type (or symbol list) :initarg :immediate-right :initform nil :accessor immediate-right)
   (immediate-front :type (or symbol list) :initarg :immediate-front :initform nil :accessor immediate-front)
   (middle :type (or symbol list) :initarg :middle :initform nil :accessor middle))
  (:documentation "Relations in scene"))

;; ################################
;; turn
;; ################################

(defclass turn (object-or-set)
  ((timestamp :type (or number symbol) :initarg :timestamp :accessor timestamp :initform nil)
   (object-set :type (or null object-set) :accessor object-set :initarg :object-set :initform nil)
   (question-type :type symbol :initarg :question-type :accessor question-type :initform 'not-applicable)
   (question :type (or string symbol) :initarg :question :accessor question :initform 'not-applicable)
   (answer :type symbol :initarg :answer :accessor answer :initform 'not-applicable)
   (topic-list :type (or null list) :initarg :topic-list :accessor topic-list :initform nil)))

;; ################################
;; pathname-entity
;; ################################

(defclass pathname-entity (entity)
  ((path :type pathname :initarg :path :accessor path)))

;; ################################
;; attention
;; ################################

(defclass attention (object-or-set)
  ((img-path :type (or null pathname) :initarg :img-path
             :accessor img-path :initform nil))
  (:documentation "A symbolic representation of an intermediate attention"))
