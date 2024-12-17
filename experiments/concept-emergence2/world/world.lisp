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

(defclass world (entity)
  (;; init-ed params
   (dataset-name :type string :accessor dataset-name)
   (dataset-split :type string :accessor dataset-split) 
   (feature-set :type string :initform nil :accessor feature-set)
    ;; information about the loaded the feature-set
   (channel-type :type hash-table :initform (make-hash-table :test #'equal) :accessor channel-type)
   (symbolic-attribute :type hash-table :initform (make-hash-table :test #'equal) :accessor symbolic-attribute)
   ;; the loaded current scene
   (current-scene :type (or null dataset-scene) :initform nil :accessor current-scene))
  (:documentation "Captures the environments in which the agents interact."))

(defmethod initialize-instance :after ((world world) &key experiment)
  "Initializes the world by loading the dataset."
  (setf (dataset-name world) (get-configuration experiment :dataset))
  (setf (dataset-split world) (get-configuration experiment :dataset-split))
  
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
   ;; list of filepaths or in-memory loaded objects
   (objects :type list :initform nil :accessor objects))
  (:documentation "An environment where the scenes are created at runtime."))

(defmethod initialize-instance :after ((world runtime-world) &key experiment)
  (setf (min-context-size world) (get-configuration experiment :min-context-size))
  (setf (max-context-size world) (get-configuration experiment :max-context-size)))

;; ----------------------
;; + Precomputed scenes +
;; ----------------------

(defclass precomputed-world (world)
  (;; list of filepaths
   (fpaths :type list :initform nil :accessor fpaths))
   (:documentation "An environment where the scenes are created beforehand."))

;; -----------------
;; + Load features +
;; -----------------
(defun load-features (world feature-set)
  "Set the available features/sensors."
  (let* ((info-path (merge-pathnames (make-pathname :directory `(:relative "concept-emergence2" "-info")
                                                    :name feature-set
                                                    :type "csv")
                                     cl-user:*babel-corpora*))
         (csv (read-csv info-path)))
    (loop for row in csv
          for channel = (intern (upcase (first row)))
          for type = (intern (upcase (second row)))
          for symbolic-attribute = (parse-keyword (third row))
          do (setf (feature-set world)
                   (cons channel (feature-set world)))
          do (setf (gethash channel (channel-type world))
                   type)
          do (setf (gethash channel (symbolic-attribute world))
                   symbolic-attribute))))

;; ---------------
;; + Dataloading +
;; ---------------

(defmethod load-data ((world precomputed-world))
  "Load the dataset by scenes."
  (let ((fpath (merge-pathnames (make-pathname :directory `(:relative
                                                            "concept-emergence2"
                                                            "split-by-scenes"
                                                            ,(dataset-name world)
                                                            "scenes"
                                                            ,(dataset-split world)))
                                cl-user:*babel-corpora*)))
    (unless (probe-file fpath)
      (error "Could not find a 'scenes' subdirectory in '~a'~%" fpath))
    ;; set the scenes (sorted by name)
    (let* ((fpaths (sort (directory (make-pathname :directory (pathname-directory fpath)
                                                         :name :wild :type "json"))
                               #'string< :key #'namestring)))
      (setf (fpaths world) fpaths))))

(defmethod load-data ((world runtime-world))
  (let ((fpath (merge-pathnames (make-pathname :directory `(:relative
                                                            "concept-emergence2"
                                                            "split-by-objects"
                                                            ,(dataset-name world)
                                                            "scenes"
                                                            ,(dataset-split world))
                                               :name (format nil
                                                             "~a_~a"
                                                             (dataset-name world)
                                                             (dataset-split world))
                                               :type "json")
                                cl-user:*babel-corpora*)))
    (unless (probe-file fpath)
      (error "Could not find a 'scenes' subdirectory in ~a~%" fpath))
    ;; load the dataset
    (let ((raw-data (decode-json-as-alist-from-source fpath)))
      (setf (objects world) (s-expr->cle-objects raw-data (feature-set world))))))

;; --------------------
;; + Helper functions +
;; --------------------

(defmethod get-feature-set ((world world))
  (feature-set world))

(defmethod get-channel-type ((world world) channel)
  (gethash channel (channel-type world)))

(defmethod get-channels-with-symbolic-attribute ((world world) symbolic-attribute)
  (loop for channel being the hash-keys of (symbolic-attribute world)
        when (equal (gethash channel (symbolic-attribute world))
                    symbolic-attribute)
          collect channel))

(defmethod is-channel-available ((world world) symbolic-attribute raw-attributes)
  (let* ((associated-channels (get-channels-with-symbolic-attribute world symbolic-attribute))
         (continuous-attributes (loop for key being the hash-keys of raw-attributes
                                      collect key)))
    (loop for channel in associated-channels
          if (member channel continuous-attributes)
            return t
          finally (return nil))))

(defmethod channel-continuous-p (world channel)
  (equal (get-channel-type world channel) 'continuous))

(defmethod channel-categorical-p (world channel)
  (equal (get-channel-type world channel) 'categorical))

(defmethod copy-object ((world world)) world)
