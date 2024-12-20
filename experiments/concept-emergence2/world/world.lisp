(in-package :cle)

;; ---------------------------
;; + Sets up a dataset/world +
;; ---------------------------

;; A note on dataloading
;;   the code assumes that data is placed at the following location
;; Scenes:
;;    ~/Corpora/concept-emergence2/<dataset_name>/scenes/<train/test>/<dataset_name>_<train/test>_<scene_index>.json
;; Feature set information (which feature set to use in an experiment)
;;    ~/Corpora/concept-emergence2/-info/<feature-set-name>.csv
;; A feature-set csv contains a header with columns: channel, type, symbolic-attribute
;;    - channel: name of the feature channel
;;    - type: continuous or categorical
;;    - symbolic-attribute: name of the symbolic attribute
;;       e.g. channels like 'width' and 'height' belong to the 'size' attribute


;; ---------
;; + World +
;; ---------

(defclass world (entity)
  (
   ;; names of the views over the dataset
   (view-names :type list :initform nil :accessor view-names)
   ;; views of the world
   (views :type hash-table :initform (make-hash-table :test #'equal) :accessor views)
   )
  (:documentation "Captures the environments in which the agents interact."))

(defclass world-view ()
  (;; view name
   (view-name :type str :initform nil :initarg :view-name :accessor view-name)
   ;; train, val or test?
   (dataset-split :type string :initarg :dataset-split :accessor dataset-split)
   (data :type list :initform nil :accessor data)
   ;; information about the loaded the feature-set
   (feature-set :type list :initform nil :accessor feature-set)
   (channel-type :type hash-table :initform (make-hash-table :test #'equal) :accessor channel-type)
   (symbolic-attribute :type hash-table :initform (make-hash-table :test #'equal) :accessor symbolic-attribute)
   )
  (:documentation "A view over the dataset"))

(defmethod initialize-instance :after ((world world) &key experiment)
  "Initializes the world by loading the dataset."
  (setf (view-names world) (get-configuration experiment :dataset))

  ;; create the views
  (loop for view-name in (view-names world)
        for view = (make-instance 'world-view
                                  :view-name view-name
                                  :dataset-split (get-configuration experiment :dataset-split))
        do (setf (gethash view-name (views world)) view))

  ;; load the features
  (load-features world (get-configuration experiment :feature-set))
  
  ;; load the dataset
  (load-data world))

;; ---------------------
;; + Scenes at runtime +
;; ---------------------

(defclass runtime-world (world)
  (;; params
   (min-context-size :type int :initform nil :accessor min-context-size)
   (max-context-size :type int :initform nil :accessor max-context-size)
   )
  (:documentation "An environment where the scenes are created at runtime."))

(defmethod initialize-instance :after ((world runtime-world) &key experiment)
  (setf (min-context-size world) (get-configuration experiment :min-context-size))
  (setf (max-context-size world) (get-configuration experiment :max-context-size)))

;; ----------------------
;; + Precomputed scenes +
;; ----------------------

(defclass precomputed-world (world)
  (;;params
   )
   (:documentation "An environment where the scenes are created beforehand."))

;; -----------------
;; + Load features +
;; -----------------
(defun load-features (world feature-sets)
  "Set the available features/sensors."
  (loop for view-name in (view-names world)
        for feature-set in feature-sets
        for view = (gethash view-name (views world))
        for info-path = (merge-pathnames (make-pathname :directory `(:relative "concept-emergence2" "-info")
                                                        :name feature-set
                                                        :type "csv")
                                         cl-user:*babel-corpora*)
        for csv = (read-csv info-path)
        do (loop for row in csv
                 for channel = (intern (upcase (first row)))
                 for type = (intern (upcase (second row)))
                 for symbolic-attribute = (parse-keyword (third row))
                 do (setf (feature-set view)
                          (cons channel (feature-set view)))
                 do (setf (gethash channel (channel-type view))
                          type)
                 do (setf (gethash channel (symbolic-attribute view))
                          symbolic-attribute))))

;; ---------------
;; + Dataloading +
;; ---------------

(defmethod load-data ((world precomputed-world))
  "Load the world which consists of of a set of precomputed scenes."
  (loop for view-name in (view-names world)
        for view = (gethash view-name (views world))
        for fpath = (merge-pathnames (make-pathname :directory `(:relative
                                                                 "concept-emergence2"
                                                                 "split-by-scenes"
                                                                 ,(view-name view)
                                                                 "scenes"
                                                                 ,(dataset-split view)))
                                     cl-user:*babel-corpora*)
        do (when (not (probe-file fpath))
             (error "Could not find a 'scenes' subdirectory in '~a'~%" fpath))
           (format t "~% Loading precomputed scenes...")
           (let* ((fpaths (sort (directory (make-pathname :directory (pathname-directory fpath)
                                                          :name :wild :type "json"))
                                #'string< :key #'namestring)))
             (setf (data view) fpaths))
           (format t "~% Completed loading.~%~%")))

(defmethod load-data ((world runtime-world))
  "Load the dataset which consists of a set of objects."
  (loop for view-name in (view-names world)
        for view = (gethash view-name (views world))
        for fpath = (merge-pathnames (make-pathname :directory `(:relative
                                                                 "concept-emergence2"
                                                                 "split-by-objects"
                                                                 ,(view-name view)
                                                                 "scenes"
                                                                 ,(dataset-split view))
                                                    :name (format nil
                                                                  "~a_~a"
                                                                  (view-name view)
                                                                  (dataset-split view))
                                                    :type "json")
                                     cl-user:*babel-corpora*)
        do (when (not (probe-file fpath))
             (error "Could not find a 'scenes' subdirectory in ~a~%" fpath))
           ;; load the dataset
           (format t "~% Loading the scenes...")
           (let* ((raw-data (decode-json-as-alist-from-source fpath))
                  (objects (s-expr->cle-objects raw-data (feature-set view))))
             (setf (data view) objects))
           (format t "~% Completed loading.~%~%")))


;; ------------------------
;; + Sample random scenes +
;; ------------------------
(defmethod random-scene ((world precomputed-world) view-name)
  "Load a random scene by fpath into memory."
  (let* ((fpath (random-elt (data (get-view world view-name))))
         (s-expr (decode-json-as-alist-from-source fpath))
         (scene (s-expr->cle-scene s-expr world view-name)))
    scene))

(defmethod random-scene ((world runtime-world) view-name)
  "Create a scene by randomly sampling objects"
  (let* ((context-size (sample-context-size world))
         (objects (random-elts (data (get-view world view-name)) context-size))
         (scene (objects->cle-scene objects world view-name)))
    scene))

(defun sample-context-size (world)
  (let* ((min-context-size (min-context-size world))
         (max-context-size (max-context-size world))
         (context-size (random-from-range min-context-size max-context-size)))
    context-size))

;; --------------------
;; + Helper functions +
;; --------------------
(defmethod get-view ((world world) view-name)
  (gethash view-name (views world)))
  

(defmethod get-feature-set ((world world) view-name)
  (feature-set (get-view world view-name)))

(defmethod get-channel-type ((world world) channel view-name)
  (gethash channel (channel-type (get-view world view-name))))

(defmethod get-channels-with-symbolic-attribute ((world world) view-name symbolic-attribute)
  (loop for view = (get-view world view-name)
        for channel being the hash-keys of (symbolic-attribute view)
        when (equal (gethash channel (symbolic-attribute view))
                    symbolic-attribute)
          collect channel))

(defmethod is-channel-available ((world world) view-name symbolic-attribute raw-attributes)
  (let* ((associated-channels (get-channels-with-symbolic-attribute world view-name symbolic-attribute))
         (continuous-attributes (loop for key being the hash-keys of raw-attributes
                                      collect key)))
    (loop for channel in associated-channels
          if (member channel continuous-attributes)
            return t
          finally (return nil))))

(defmethod channel-continuous-p (world view-name channel)
  (equalp (get-channel-type world channel view-name)
         'continuous))

(defmethod channel-categorical-p (world view-name channel)
  (equalp (get-channel-type world channel view-name)
         'categorical))

(defmethod copy-object ((world world)) world)
