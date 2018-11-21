(in-package :physical-robot-world)

(export '(*robotdata-path*))

(defparameter *robotdata-path* 
  (babel-pathname :directory (list :up "robotdata"))
  "The path of the scenes repository")

;; ############################################################################
;; robot-world-model
;; ############################################################################

(export '(robot-world-model timestamp source-path robot))

(defclass robot-world-model ()
  ((id :type symbol :initarg :id :accessor id)
   (timestamp :type integer :initarg :timestamp :accessor timestamp)
   (source-path :type pathname :initarg :source-path :reader source-path
                :documentation "The directory for this robot-world-model")
   (robot :type string :initarg :robot :reader robot
	  :documentation "the robot associated to this perception"))
  (:documentation "base class for a world model"))

;; ############################################################################
;; robot-scene
;; ############################################################################
(export '(robot-scene name wm-a wm-b))

(defclass robot-scene ()
  ((name :type string :initarg :name :reader name
         :documentation "The name of the scene")
   (wm-a :type robot-world-model :initarg :wm-a :reader wm-a
	 :documentation "The world model of robot a")
   (wm-b :type robot-world-model :initarg :wm-b :reader wm-b
	 :documentation "The world model of robot b")
   (source-path :type pathname :initarg :source-path :reader source-path))
  (:documentation "base class for a scene"))

(defgeneric check-scene (scene mode &key &allow-other-keys)
  (:documentation "Implement checks to see if the given scene is valid"))

;; ############################################################################
;; robot-world
;; ############################################################################

(export '(scenes current-scene))

(defclass robot-world ()
  ((data-sets :type list :initarg :data-sets :reader data-sets
              :documentation "A list of data-sets (subfolders of robotdata)")
   (scenes :type list :initform nil :accessor scenes
           :documentation "A list of scenes (robot-scene instances)")
   (current-scene :type (or null robot-scene) :initform nil :accessor current-scene
                  :documentation "The current scene that both agents are looking at"))
  (:documentation "An interface to the robotdata repository"))

;; ############################################################################
;; get-world-model
;; ############################################################################

(export '(a b get-world-model))

(defgeneric get-world-model (world scene-name robot)
  (:documentation "Returns a named scene for debugging purposes"))

(defmethod get-world-model ((world robot-world) (scene-name string) (robot symbol))
  (get-world-model (find scene-name (scenes world) :key #'name :test #'equal)
                   scene-name robot))

(defmethod get-world-model ((scene robot-scene) (scene-name t) (robot (eql 'a)))
  (wm-a scene))
  
(defmethod get-world-model ((scene robot-scene) (scene-name t) (robot (eql 'b)))
  (wm-b scene))

(defmethod get-world-model ((scene (eql nil)) (scene-name t) (robot t))
  (error "there is no scene ~a" scene-name))

(defmethod get-world-model ((scene t) (scene-name t) (robot t))
  (error "there is no robot ~a" robot))

;; ############################################################################
;; next-scene
;; ############################################################################

(export '(next-scene))

(defgeneric next-scene (world)
  (:documentation "Randomly chooses the next scene and returns it"))

(defmethod next-scene ((world robot-world))
  (setf (current-scene world)
        (random-elt (scenes world))))

;; ############################################################################
;; robot-interpret-pointing
;; ############################################################################

(export '(physical-robot-interpret-pointing))

(defgeneric physical-robot-interpret-pointing (x y world-model &key &allow-other-keys)
  (:documentation "Interprets pointing in the a robot's own
                   world model. X and y are the coordinates of
                   the object pointed to in the coordinate system
                   of the pointer. Returns the id of the closest
                   object to the position an the coordinates."))

;; ############################################################################
;; load-object
;; ############################################################################

(defgeneric load-object (type pathname &key &allow-other-keys)
  (:documentation "Loads world models, scenes and the like from files in directory"))

(defgeneric s-expr->object (type s-expr &key &allow-other-keys)
  (:documentation "Reads an object from an s-expression"))

(defgeneric object->s-expr (thing)
  (:documentation "Turns an object into a readable s-expression"))