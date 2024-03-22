(in-package :cle)

(defclass dataset-world (entity)
  ((dataset  :type string   :initarg :dataset   :accessor dataset)
   (dataset-split    :type string   :initarg :dataset-split :accessor dataset-split)
   (available-channels :type list :initarg :available-channels :accessor available-channels)
   (scenes   :type list     :initarg :scenes    :accessor scenes)
   (current-scene :type (or null dataset-scene) :initform nil :accessor current-scene))
  (:documentation "Loads the scenes of a dataset"))

(defmethod initialize-instance :around ((world dataset-world)
                                        &key
                                        (dataset nil)
                                        (dataset-split nil))
  (let ((datapath (merge-pathnames (make-pathname :directory `(:relative "concept-emergence2" ,dataset))
                                   cl-user:*babel-corpora*)))
    ;; check for *clevr-data-path*
    (unless (probe-file datapath)
      (error "Could not find the 'data-path' directory in ~%~a" datapath))
    ;; initialize the instance
    (call-next-method)
    ;; load the scenes
    (let ((scenes-path (merge-pathnames (make-pathname :directory `(:relative "scenes" ,dataset-split))
                                        datapath)))
      (unless (probe-file scenes-path)
        (error "Could not find a 'scenes' subdirectory in ~a~%" scenes-path))
      (setf (slot-value world 'dataset) dataset)
      (setf (slot-value world 'dataset-split) dataset-split)
      (setf (slot-value world 'scenes)
            ;; sort all files by name
            (sort (directory (make-pathname :directory (pathname-directory scenes-path)
                                            :name :wild :type "json"))
                  #'string< :key #'namestring)))
    
    ;; set the current scene
    (unless (scenes world)
      (warn "No scenes were loaded."))))

(defmethod copy-object ((world dataset-world)) world)