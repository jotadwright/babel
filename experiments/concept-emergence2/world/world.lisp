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
  (;; params
   (dataset-name :type string :initform nil :accessor dataset-name)
   (dataset-split :type string :initform nil :accessor dataset-split)
   (dataset-loader :type keyword :initform nil :accessor dataset-loader)
   (min-context-size :type int :initform nil :accessor min-context-size)
   (max-context-size :type int :initform nil :accessor max-context-size)
   ;; list of available channels
   (feature-set :type list :initform nil :accessor feature-set)
   (channel-type :type hash-table :initform (make-hash-table :test #'equal) :accessor channel-type)
   (symbolic-attribute :type hash-table :initform (make-hash-table :test #'equal) :accessor symbolic-attribute)
   ;; list of filepaths or in-memory loaded objects
   (data :type hash-table :initform (make-hash-table :test #'equal) :accessor data)
   ;; the loaded current scene
   (current-scene :type (or null dataset-scene) :initform nil :accessor current-scene)
   ;; scene ids (if deterministic)
   (scene-ids :type list :initform nil :accessor scene-ids)
   ;; current scene index (if deterministic)
   (current-scene-idx :type int :initform 0 :accessor current-scene-idx))
  (:documentation "Loads the scenes of a dataset"))

(defmethod initialize-instance :around ((world world) &key
                                        dataset-loader
                                        dataset-name
                                        dataset-split
                                        feature-set
                                        min-context-size
                                        max-context-size)
  "Initializes the world by loading the dataset."
  ;; initialize the instance (so that experiment slot is set)
  (call-next-method)

  ;; set dataset-loader type
  (setf (slot-value world 'dataset-name) dataset-name)
  (setf (slot-value world 'dataset-split) dataset-split)
  (setf (slot-value world 'dataset-loader) dataset-loader)
  (setf (slot-value world 'min-context-size) min-context-size)
  (setf (slot-value world 'max-context-size) max-context-size)

  ;; load the features
  (load-features world feature-set)
  
  ;; load the dataset
  (case dataset-loader
    (:split-by-scenes
     (load-by-scenes world))
    (:split-by-objects
     (load-by-objects world))
    (otherwise
     (error "Invalid dataset-loader configuration: '~a'~%" utterance))))

;; -------------------------
;; + Dataloading by scenes +
;; -------------------------

(defun load-by-scenes (world)
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
    (let* ((scene-fpaths (sort (directory (make-pathname :directory (pathname-directory fpath)
                                                         :name :wild :type "json"))
                               #'string< :key #'namestring))
           (scenes-ht (make-hash-table :test #'equal)))
      (loop for fpath in scene-fpaths
            do (setf (gethash fpath scenes-ht) fpath))
      (setf (slot-value world 'data) scenes-ht))))

;; --------------------------
;; + Dataloading by objects +
;; --------------------------

(defun load-by-objects (world)
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
      (setf (slot-value world 'data) (s-expr->cle-objects raw-data (feature-set world))))))

;; --------------------
;; + Helper functions +
;; --------------------
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
          do (setf (slot-value world 'feature-set)
                   (cons channel (slot-value world 'feature-set)))
          do (setf (gethash channel (slot-value world 'channel-type))
                   type)
          do (setf (gethash channel (slot-value world 'symbolic-attribute))
                   symbolic-attribute))))

(defmethod get-feature-set ((world world))
  (slot-value world 'feature-set))

(defmethod get-channel-type ((world world) channel)
  (gethash channel (slot-value world 'channel-type)))

(defmethod get-channels-with-symbolic-attribute ((world world) symbolic-attribute)
  (loop for channel being the hash-keys of (slot-value world 'symbolic-attribute)
        when (equal (gethash channel (slot-value world 'symbolic-attribute))
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
