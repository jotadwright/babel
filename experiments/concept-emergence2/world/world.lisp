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
  (;; only init-ed slot
   ;; list of available channels
   (feature-set :type list :initform nil :accessor feature-set)
   (channel-type :type hash-table :initform (make-hash-table :test #'equal) :accessor channel-type)
   (symbolic-attribute :type hash-table :initform (make-hash-table :test #'equal) :accessor symbolic-attribute)
   ;; list of scene filepaths
   (scene-fpaths :type list :initform nil :accessor scene-fpaths)
   ;; the loaded current scene
   (current-scene :type (or null dataset-scene) :initform nil :accessor current-scene)
   ;; scene ids (if deterministic)
   (scene-ids :type list :initform nil :accessor scene-ids)
   ;; current scene index (if deterministic)
   (current-scene-idx :type int :initform 0 :accessor current-scene-idx))
  (:documentation "Loads the scenes of a dataset"))

(defmethod initialize-instance :around ((world world) &key
                                        dataset-name
                                        dataset-split
                                        feature-set
                                        scene-sampling
                                        (data-fname nil))
  "Initializes the world by loading the dataset."
  ;; initialize the instance (so that experiment slot is set)
  (call-next-method)

  ;; load the scenes
  (let ((fpath (merge-pathnames (make-pathname :directory `(:relative "concept-emergence2" ,dataset-name "scenes" ,dataset-split))
                                cl-user:*babel-corpora*)))
    (unless (probe-file fpath)
      (error "Could not find a 'scenes' subdirectory in ~a~%" fpath))
    ;; set the scenes (sorted by name)
    (setf (slot-value world 'scene-fpaths)
          (sort (directory (make-pathname :directory (pathname-directory fpath)
                                          :name :wild :type "json"))
                #'string< :key #'namestring)))

  ;; if the scene sampling is deterministic, retrieve and set the scene ids
  ;; deterministic = a file specifies the scene ids to load in order (located at ~/Corpora/concept-emergence2/<dataset>/scenes/)
  (when (eql scene-sampling :deterministic)
    (let* ((fpath (merge-pathnames (make-pathname :directory `(:relative "concept-emergence2" ,dataset-name)
                                                  :name data-fname
                                                  :type "txt")
                                   cl-user:*babel-corpora*))
           (raw (uiop:read-file-lines fpath))
           (scene-ids (map 'list #'parse-integer raw)))
      (setf (slot-value world 'scene-ids) scene-ids)))

  ;; set the available channels
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

(defmethod copy-object ((world world)) world)
