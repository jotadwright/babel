(in-package :gcng)

;; ----------------
;; + Embody Agent +
;; ----------------

(defun embody (agent robot)
  "Connect a robot to an agent and (if needed)
   open a new connection. If the agent is the
   speaker, it will say the current interaction
   number."
  (setf (robot agent) robot)
  (unless (robot-connected-p robot)
    (make-new-connection robot :test-connection
                         (not (get-configuration agent :silent))))
  (unless (get-configuration agent :silent)
    (when (speakerp agent)
      (speak (robot agent)
             (format nil "Interaction ~a starts"
                     (interaction-number interaction))))))

;; -----------------
;; + Observe World +
;; -----------------

(define-event observe-scene-finished (context sensory-object-set)
  (pathspec t) (agent grounded-color-naming-game-agent))

(defun detect-context-of-required-size (agent size)
  "Detect a context of the required size. Ask for help if this fails"
  (loop with nr-of-objects-detected = 0
        with detected-scene = nil
        with img-path = nil
        while (/= nr-of-objects-detected size)
        do (multiple-value-bind (data pathspec) (observe-world (robot agent) :open nil)
             (let ((scene (process-scene data)))
               (setf nr-of-objects-detected (length (entities scene)))
               (setf detected-scene scene)
               (setf img-path pathspec)
               (unless (= nr-of-objects-detected size)
                 (unless (get-configuration agent :silent)
                   (speak (robot agent) (format nil "I could not detect ~a objects. Please help me" size)))
                 (detect-head-touch (robot agent) :middle))))
        finally
        (return (values detected-scene img-path))))
  
(defun generate-and-detect-context (agent size)
  "Detect a context of the required size. Generate a new one if this fails"
  (loop with nr-of-objects-detected = 0
        with detected-scene = nil
        with img-path = nil
        while (/= nr-of-objects-detected size)
        do (progn (generate-new-scene size)
             (multiple-value-bind (data pathspec) (observe-world (robot agent) :open nil)
               (let ((scene (process-scene data)))
                 (setf nr-of-objects-detected (length (entities scene)))
                 (setf detected-scene scene)
                 (setf img-path pathspec))))
        finally
        (return (values detected-scene img-path))))

(defmethod robot-observe-world ((agent grounded-color-naming-game-agent) context-size)
  "Observe the world and store this in the 'context' slot"
  (multiple-value-bind (context pathspec)
      (if (speakerp agent)
        (generate-and-detect-context agent context-size)
        (detect-context-of-required-size agent context-size))
    (setf (context agent) context)
    (set-data (ontology agent) 'context context)
    (unless (get-configuration agent :silent)
      (speak (robot agent)
             (format nil "I detected ~a objects"
                     (length (entities (context agent))))))
    (notify observe-scene-finished (context agent) pathspec agent))
  (context agent))

(defmethod agent-observe-world ((agent grounded-color-naming-game-agent))
  (let* ((interaction (current-interaction (experiment agent)))
         (context-size (get-data interaction 'context-size))
         (perceptual-deviation (get-configuration (experiment interaction) :perceptual-deviation)))
    (if (speakerp agent)
      (robot-observe-world agent context-size)
      (if perceptual-deviation
        (robot-observe-world agent context-size)
        (let ((speaker-context (context (speaker interaction))))
          (setf (context agent) speaker-context)
          (set-data (ontology agent) 'context speaker-context))))))
  

;; ----------------
;; + Choose Topic +
;; ----------------

(define-event choose-topic-finished (topic sensory-object))

(defmethod choose-topic ((agent grounded-color-naming-game-agent)
                         (context sensory-object-set))
  "Choose a random topic from the context"
  (setf (topic agent) (random-elt (entities context)))
  (notify choose-topic-finished (topic agent))
  (unless (get-configuration agent :silent)
    (speak (robot agent) "I chose the topic"))
  (topic agent))

;; ---------------------
;; + Determine Success +
;; ---------------------

(defun sort-on-x-axis (objects)
  (sort objects #'< :key #'x-pos))

(defmethod determine-success ((speaker grounded-color-naming-game-agent)
                              (hearer grounded-color-naming-game-agent))
  "Determine the success of the interaction"
  (let (success)
    (when (and (topic speaker)
               (hypothesized-topic hearer)
               (observed-object speaker))
      (setf success
            (eql (id (topic speaker))
                 (id (observed-object speaker)))))
    (setf (communicated-successfully speaker) success
          (communicated-successfully hearer) success)
    success))
      

;; ---------------------
;; + Point and Observe +
;; ---------------------

(defmethod point-and-observe ((pointer grounded-color-naming-game-agent)
                              object-from-pointer-perspective
                              (observer grounded-color-naming-game-agent))
  (let* ((sorted-pointer-context (sort-on-x-axis (context pointer)))
         (sorted-observer-context (sort-on-x-axis (context observer)))
         (pointer-object-position (position object-from-pointer-perspective sorted-pointer-context :key #'x-pos :test #'=)))
  (if (get-configuration pointer :perceptual-deviation)
    (let ((observed-object (nth pointer-object-position sorted-observer-context)))
      (cond ((= pointer-object-position (/ (length (context pointer)) 2))
             (point (robot pointer) :both))
            ((< pointer-object-position (/ (length (context pointer)) 2))
             (point (robot pointer) :left))
            ((> pointer-object-position (/ (length (context pointer)) 2))
             (point (robot pointer) :right)))
      (setf (observed-object observer) observed-object))
    (progn
      (cond ((= pointer-object-position (/ (length (context pointer)) 2))
             (point (robot pointer) :both))
            ((< pointer-object-position (/ (length (context pointer)) 2))
             (point (robot pointer) :left))
            ((> pointer-object-position (/ (length (context pointer)) 2))
             (point (robot pointer) :right)))
      (setf (observed-object observer) object-from-pointer-perspective)))))