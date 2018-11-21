(in-package :physical-robot-world)

(export '(entities
          physical-robot-world-model source-path create-world-model
          physical-robot-scene
          physical-robot-world
          physical-robot-interpret-pointing))

;; ############################################################################
;; physical-robot-world-model
;; ############################################################################

(defclass physical-robot-world-model (robot-world-model)
  ((source-path :type pathname :initarg :source-path :reader source-path
                :documentation "The directory for that world model")
   (entities :type list :initarg :entities :accessor entities :initarg nil))
  (:documentation "The world model of one robot for one scene"))

(defmethod s-expr->object ((type (eql 'physical-robot-world-model))
                           (s-expr list) &key
                           (id (make-id 'physical-robot-world-model))
                           (source-path (make-pathname))
                           (robot "unknown"))
  (declare (ignorable type))
  (make-instance
    'physical-robot-world-model
    :id id :robot robot :source-path source-path
    :entities (loop for expression in s-expr
                    collect (s-expr->object 'physical-robot-world-object expression))))

(defmethod object->s-expr ((wm physical-robot-world-model))
  (loop for entity in (entities wm)
        collect (object->s-expr entity)))

(defmethod load-object ((type (eql 'physical-robot-world-model))
                        (directory pathname)
                        &key robot)
  (assert (stringp robot)) ;; only string should be passed as robot here
  (let* ((file-path (merge-pathnames (make-pathname :name robot :type "lisp")
                                     directory))
         (scene-name (car (last (pathname-directory directory))))
         (s-expression (with-open-file (input-stream file-path :direction :input)
                                       (read input-stream nil nil)))
         (id (symb scene-name robot)))
    (s-expr->object 'physical-robot-world-model
                    s-expression 
                    :id id :source-path file-path :robot robot)))

(defmethod print-object ((wm physical-robot-world-model) stream)
  (if *print-pretty*
      (pprint-logical-block (stream nil)
                            (format stream "<physical-robot-world-model:~
                                    ~:_ robot: ~s,~:_ source-path: ~a,~:_ entities:~:_ ~:w"
                                    (robot wm) (source-path wm) (entities wm))
                            (format stream ">"))
      (format stream "<physical-robot-world-model>")))

(defmethod copy-object ((wm physical-robot-world-model))
  wm)

;; ############################################################################
;; physical-robot-interpret-pointing
;; ############################################################################

(defmethod physical-robot-interpret-pointing ((x number) (y number)
                                              (world-model physical-robot-world-model)
                                              &key (no-robots-and-landmarks t))
  (let* ( ;; x and y of rother robot in own coordinate system 
         (x-r (find-fvalue (second (entities world-model)) 'x))
         (y-r (find-fvalue (second (entities world-model)) 'y))
         ;; orientation of other robot in own coordinate system
         (rot-r (* (- (find-fvalue (second (entities world-model)) 'orientation) 0.5)
                   2 pi)) ;; orientation of other robot
         ;; the vector from the other robot to the object rotated by the orientation
         ;; the other robot (now in own coordinate sytem)
         (x-rotated (- (* x (cos rot-r)) (* y (sin rot-r))))
         (y-rotated (+ (* x (sin rot-r)) (* y (cos rot-r))))
         ;; the rotated vector translated by the vector to the the other robot
         (x2 (+ x-rotated x-r))
         (y2 (+ y-rotated y-r))
         ;; the distances to all objects in the own context
         (object-distances 
           (loop for object in (entities world-model)
                 for fvalue = (find-fvalue object 'width)
                 for radius = (if fvalue (/ fvalue 2) 0)
                 unless (or
                          (region-p object)
                          (and no-robots-and-landmarks (or (robot-p object) (box-p object))))
                 collect (cons (id object) 
                               (- (sqrt (+ (expt (- (find-fvalue object 'x) x2) 2)
                                           (expt (- (find-fvalue object 'y) y2) 2)))
                                  radius))))
         (smallest-distance (loop for object-distance in object-distances
                                  minimize (cdr object-distance)))
         (closest-object (car (find smallest-distance object-distances :key #'cdr))))
    (values closest-object x2 y2 smallest-distance)))

;; ############################################################################
;; physical-robot-scene
;; ############################################################################

(defclass physical-robot-scene (robot-scene)
  () (:documentation "The world models of both robots for one scene"))

(defmethod load-object ((type (eql 'physical-robot-scene))
                        (directory pathname) 
                        &key
                        (check-scene-methods '(number-of-objects pointing all-robots))
                        (warn t))
  "makes a scene and returns it"
  (let* ((scene-name (car (last (pathname-directory directory))))
         (scene (let ((wm-a (load-object 'physical-robot-world-model directory :robot "a"))
                      (wm-b (load-object 'physical-robot-world-model directory :robot "b")))
                  (make-instance 'physical-robot-scene :name scene-name
                                 :source-path directory
                                 :wm-a wm-a :wm-b wm-b))))
    (when (loop for check-scene-method in check-scene-methods
                always (check-scene scene check-scene-method :warn warn))
      scene)))

(defmethod print-object ((scene physical-robot-scene) stream)
  (if *print-pretty*
      (pprint-logical-block (stream nil)
                            (format stream "<physical-robot-scene:~:_ name: ~s,~:_ wm-a: ~a,~:_ wm-b: ~a>"
                                    (name scene) (wm-a scene) (wm-b scene)))
      (call-next-method)))

;; ############################################################################
;; check-scene defmethods
;; ############################################################################

(export '(pointing number-of-objects all-robots))

(defun check-pointing (wm-1 wm-2 scene-name robot &key (warn nil))
  (loop with ok = t
        for object in (entities wm-1)
        for object-x = (when (find-fvalue object 'x)
                         (find-fvalue object 'x))
        for object-y = (when (find-fvalue object 'y)
                         (find-fvalue object 'y))
        for id = (id object)
        for id-2 = (when (and object-x object-y)
                     (physical-robot-interpret-pointing 
                       (find-fvalue object 'x) (find-fvalue object 'y)
                       wm-2 :no-robots-and-landmarks nil))
        for object-2 = (when id-2 (find id-2 (entities wm-2) :key #'id))
        unless (and object-2
                    (eq (physical-robot-interpret-pointing 
                          (find-fvalue object-2 'x) (find-fvalue object-2 'y)
                          wm-1 :no-robots-and-landmarks nil)
                        id))
        do
        (progn
          (setf ok nil)
          (when warn
            (format t
                    "~%Pointing test failed for object ~a in ~a robot ~a"
                    id scene-name robot)))
        and collect id into ids
        finally (return (values ok ids))))

(defmethod check-scene ((scene physical-robot-scene) (mode (eql 'pointing))
                        &key (warn t))
  (and (check-pointing (wm-a scene) (wm-b scene) (name scene) 
                       "a" :warn warn)
       (check-pointing (wm-b scene) (wm-a scene) (name scene) 
                       "b" :warn warn)))

(defmethod check-scene ((scene physical-robot-scene) (mode (eql 'number-of-objects))
                        &key (warn t))
  (let ((equal-number-of-objects (= (length (entities (wm-a scene)))
                                    (length (entities (wm-b scene))))))
    (when (and (not equal-number-of-objects) warn)
      (format t "~%Different number of objects in ~a for robot a and b." (name scene)))
    equal-number-of-objects))

(defmethod check-scene ((scene physical-robot-scene) (mode (eql 'all-robots))
                        &key (warn t))
  (let ((all-robots (and (wm-a scene)
                         (wm-b scene))))
    (when (and (not all-robots) warn)
      (format t "~%World models for robot missing in scene ~a." (name scene)))
    all-robots))

;;; create some check for loading scenes where pointing-fails and/or different-nr-of-objects
(defmethod check-scene ((scene physical-robot-scene) (mode (eql 'different-nr-of-objects))
                        &key (warn t))
  (declare (ignore warn))
  (not (check-scene scene 'number-of-objects)))

(defmethod check-scene ((scene physical-robot-scene) (mode (eql 'pointing-fails))
                        &key (warn t))
  (declare (ignore warn))
  (not (check-scene scene 'pointing :warn t)))

;; ############################################################################
;; physical-robot-world
;; ############################################################################

(defclass physical-robot-world (robot-world) ()
  (:documentation "An interface to static robot worlds, i.e. non movement data"))

(defmethod copy-object ((world physical-robot-world)) world)

(defmethod initialize-instance :around ((world physical-robot-world)
                                        &key (data-sets (error "Please provide :data-sets"))
                                        (check-scene-methods '(number-of-objects
                                                               pointing
                                                               all-robots))
                                        (exclude-scenes nil)
                                        (warn t))
  (unless (probe-file *robotdata-path*)
    (error "Could not find the 'robotdata' repository in ~%~a" *robotdata-path*))
  (unless (and (listp data-sets) 
               (> (length data-sets) 0)
               (loop for data-set in data-sets always (stringp data-set)))
    (error ":data-sets should be a list of strings"))  
  (call-next-method)
  (setf (slot-value world 'scenes)
        (loop with directories 
              = (loop for data-set in data-sets
                      for set-directory = (merge-pathnames 
                                            (make-pathname :directory (list :relative data-set))
                                            *robotdata-path*)
                      unless (probe-file set-directory)
                      do (error "~s is not a subdirectory of ~%~a" data-set *robotdata-path*)
                      append (loop for directory 
                                   in (directory 
                                        (make-pathname :directory (pathname-directory set-directory)
                                                       :name :wild)
                                        #+(or mcl openmcl) :directories #+(or mcl openmcl) t)
                                   when (and (directory-pathname-p directory)
                                             (probe-file 
                                               (make-pathname :directory (pathname-directory directory)
                                                              :name "a" :type "lisp")))
                                   collect directory))
              for directory in directories
              for scene-name = (car (last (pathname-directory directory)))
              for scene = (unless (find scene-name exclude-scenes :test #'equal)
                            (load-object 
                              'physical-robot-scene
                              directory
                              :check-scene-methods check-scene-methods
                              :warn warn))
              when scene
              collect scene))
  (if (scenes world)
      (setf (current-scene world) (first (scenes world)))
      (warn "There were no scenes loaded.")))

(defmethod print-object ((world physical-robot-world) stream)
  (if *print-pretty*
      (pprint-logical-block (stream nil)
                            (format stream "<physical-robot-world:~:_ scenes: ~a,~:_ current-scene: ~a,~:_ "
                                    (mapcar #'name (scenes world))
                                    (when (current-scene world)
                                      (name (current-scene world))))
                            (call-next-method)
                            (format stream ">"))
      (call-next-method)))

