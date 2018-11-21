
(in-package :physical-robot-world)

(export '(dynamic-physical-robot-world-model 
	  dynamic-physical-robot-scene
	  dynamic-physical-robot-world
          world-models))

;; ############################################################################
;; dynamic-physical-robot-world-model
;; ############################################################################
 
(defclass dynamic-physical-robot-world-model (robot-world-model)
  ((world-models :type list :accessor world-models
                 :initarg :world-models
                 :documentation "A list of physical-robot-world-models over time"))
  (:documentation "The world models of one robot of one scene over time"))

(defmethod s-expr->object ((type (eql 'dynamic-physical-robot-world-model))
                           (s-expr list) &key
                           (id (make-id 'dynamic-physical-robot-world-model))
                           (source-path (make-pathname))
                           (robot "unknown"))
  (make-instance
     'dynamic-physical-robot-world-model
     :id id :robot robot :source-path source-path
     :world-models (loop for expr in s-expr
                         for timestamp = (first expr)
                         for wm-sexpr = (second expr)
                         for wm = (s-expr->object
                                   'physical-robot-world-model
                                   wm-sexpr
                                   :id (symb id '- timestamp)
                                   :robot robot
                                   :source-path source-path)
                         do (setf (timestamp wm) timestamp)
                         collect wm)))

(defmethod load-object ((type (eql 'dynamic-physical-robot-world-model))
                    (directory pathname)
                    &key robot)
  (let* ((file-path (merge-pathnames (make-pathname :name robot :type "lisp")
                                     directory))
	 (scene-name (car (last (pathname-directory directory))))
	 (s-expression (with-open-file (input-stream file-path :direction :input)
			 (read input-stream nil nil)))
         (id (symb scene-name robot)))
    (s-expr->object 'dynamic-physical-robot-world-model
                    s-expression
                    :id id :source-path file-path :robot robot)))
	 
(defmethod print-object ((wm dynamic-physical-robot-world-model) stream)
  (if *print-pretty*
      (pprint-logical-block (stream nil)
	(format stream "<dynamic-physical-robot-world-model:~
                         ~:_ robot: ~s,~:_ source-path: ~a,~:_ "
		(robot wm) (source-path wm))
	(call-next-method)
	(format stream ">"))
      (format stream "<dynamic-physical-robot-world-model>")))

;; ############################################################################
;; dynamic-physical-robot-scene
;; ############################################################################

(defclass dynamic-physical-robot-scene (robot-scene)
  () (:documentation "The world models of both robots for one dynamic scene"))

(defmethod load-object ((type (eql 'dynamic-physical-robot-scene))
                        (directory pathname) &key)
  "makes a scene and returns it"
  (let ((scene-name (car (last (pathname-directory directory))))
        (wm-a (load-object 'dynamic-physical-robot-world-model directory
                           :robot "a"))
        (wm-b (load-object 'dynamic-physical-robot-world-model directory
                           :robot "b")))
    (if (and wm-a wm-b)
      (make-instance 'dynamic-physical-robot-scene
                     :name scene-name
                     :source-path directory
                     :wm-a wm-a :wm-b wm-b)
      nil)))

(defmethod print-object ((scene dynamic-physical-robot-scene) stream)
  (if *print-pretty*
      (pprint-logical-block (stream nil)
	(format stream "<dynamic-physical-robot-scene:~:_ name: ~s,~:_
                        ~:_ wm-a: ~a,~:_ wm-b: ~a>"
		(name scene) (wm-a scene) (wm-b scene)))
      (call-next-method)))

;; ############################################################################
;; dynamic-physical-robot-world
;; ############################################################################

(defclass dynamic-physical-robot-world (robot-world)
  () (:documentation "An interface to the robotdata repository"))

;; Physical-robot-worlds are not copied.
(defmethod copy-object ((world dynamic-physical-robot-world)) world)

(defmethod initialize-instance :around ((world dynamic-physical-robot-world)
					&key (data-sets (error "Please provide :data-sets"))
					(exclude-scenes nil))
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
                           (load-object 'dynamic-physical-robot-scene directory))
	     when scene
	     collect scene))    
  (if (scenes world)
    (setf (current-scene world) (first (scenes world)))	 
    (warn "There were no scenes loaded.")))

(defmethod print-object ((world dynamic-physical-robot-world) stream)
  (if *print-pretty*
      (pprint-logical-block (stream nil)
	(format stream "<dynamic-physical-robot-world:~:_ scenes: ~a,~:_ current-scene: ~a,~:_ "
		(mapcar #'name (scenes world))
		(when (current-scene world)
		  (name (current-scene world))))
	(call-next-method)
	(format stream ">"))
      (call-next-method)))
