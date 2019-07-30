;;;; ./processes.lisp

(in-package :roos)

;; -------------------------
;; + Dummy initial process +
;; -------------------------

(defmethod run-process (process
                        (process-label (eql 'initial-process))
                        task
                        agent)
  "Dummy process for restart situations. Use this to make the
   robot say whether it is speaker or hearer."
  (declare (ignorable process task agent process-label))
  (speak agent (format nil "I am the ~a"
                       (discourse-role agent)))
  (make-process-result 1 nil :process process))

;; -----------------
;; + Observe Scene +
;; -----------------

(defclass detection-problem (problem)
  ((required-context-size :accessor required-context-size :initarg :required-context-size))
  (:documentation "Problem when the wrong number of objects are detected"))

(defclass detection-diagnostic (diagnostic)
  ((trigger :initform 'detection))
  (:documentation "Diagnostic for the scene detection"))

(defclass new-scene-repair (repair)
  ((trigger :initform 'detection))
  (:documentation "Repair by generating a new scene"))

(defmethod diagnose ((diagnostic detection-diagnostic)
                     process-result
                     &key trigger)
  "Diagnose if the right amount of objects is detected"
  (declare (ignorable trigger))
  (let ((context-size (find-data process-result 'required-size))
        (object-set (find-data process-result 'observed-scene)))
    (unless (= context-size (length (entities object-set)))
      (make-instance 'detection-problem :required-context-size context-size))))

(defmethod repair ((repair new-scene-repair)
                   (problem detection-problem)
                   (object process-result)
                   &key trigger)
  "Repair by generating a new scene"
  (declare (ignorable trigger))
  (generate-unique-scene (required-context-size problem))
  (restart-object object nil)
  (make-instance 'fix))

(define-event observe-scene-finished (img-path string) (scene object-set))

(defmethod run-process (process
                        (process-label (eql 'observe-scene))
                        task
                        agent)
  "Process to observe the scene using the robot's vision.
   The diagnostic will check if the right amount of objects is detected.
   If not, the repair will generate a new scene and
   restart this process."
  (let ((context-size (get-configuration agent :context-size))
        scene
        process-result)
    (multiple-value-bind (data img-path) (observe-scene agent :open nil)
      (setf scene (alist->object-set data))
      (setf process-result (make-process-result 1 (list (cons 'observed-scene scene)
                                                        (cons 'required-size context-size))
                                                :process process))
      (unless (notify-learning process-result :trigger 'detection)
        (speak agent (format nil "I detected ~a objects" context-size))
        (when img-path
          (notify observe-scene-finished img-path scene))
        process-result))))

;; -----------------
;; + Sensory Scale +
;; -----------------

(define-event sensory-scale-finished (scaled-scene object-set))

(defmethod run-process (process
                        (process-label (eql 'sensory-scale))
                        task
                        agent)
  "Sensory scale the scene using the configuration values
   :features and :feature-bounds."
  (let* ((prev-process-input (input process))
         (observed-scene (find-data prev-process-input 'observed-scene))
         (scene (copy-object observed-scene))
         (features (get-configuration agent :features))
         (feature-bounds (get-configuration agent :feature-bounds)))
    (loop for object in (entities scene)
          do (loop for feature in features
                   for min-bound = (rest (assoc 'min (rest (assoc feature feature-bounds))))
                   for max-bound = (rest (assoc 'max (rest (assoc feature feature-bounds))))
                   do (scale-object-feature object feature min-bound max-bound)))
    (notify sensory-scale-finished scene)
    (make-process-result 1 (list (cons 'scene scene))
                         :process process)))

;; -----------------
;; + Choose Topics +
;; -----------------

(define-event choose-topics-finished (topic-ids list))

(defmethod run-process (process
                        (process-label (eql 'choose-topics))
                        task
                        agent)
  "Choose one or multiple topic from the scene."
  (let* ((prev-process-input (input process))
         (scene (find-data prev-process-input 'scene))
         (all-ids (mapcar #'id (entities scene)))
         (nr-of-topics (get-configuration agent :nr-of-topics))
         (topic-ids (random-elts all-ids nr-of-topics)))
    (if (= nr-of-topics 1)
      (speak agent "I chose the topic.")
      (speak agent (format nil "I chose ~a topics" nr-of-topics)))
    (notify choose-topics-finished topic-ids)
    (make-process-result 1 (list (cons 'topic-ids topic-ids))
                         :process process)))

;; --------------------------
;; + Compute All Saliencies +
;; --------------------------

(defmethod run-process (process
                        (process-label (eql 'compute-all-saliencies))
                        task
                        agent)
  "Compute the channel that is most discriminating for the topic,
   given that any object can be the topic."
  (let* ((prev-process-input (input process))
         (scene (find-data prev-process-input 'scene))
         (features (get-configuration agent :features))
         (game-stage (get-configuration agent :game-stage))
         saliencies-per-object)
    (loop for topic in (entities scene)
          for list-of-objects = (remove (id topic) (entities scene) :key #'id)
          for topic-saliencies = nil
          do (loop for feature in features
                   for topic-value = (get-object-feature topic feature)
                   for min-saliency = (loop for object in list-of-objects
                                            for object-value = (get-object-feature object feature)
                                            for saliency = (if (listp object-value)
                                                             (average (mapcar (lambda (x y)
                                                                                (abs (- x y)))
                                                                              topic-value
                                                                              object-value))
                                                             (abs (- topic-value object-value)))
                                            minimizing saliency)
                   do (push (cons feature min-saliency) topic-saliencies))
          do (push (cons (id topic) topic-saliencies) saliencies-per-object))
    (make-process-result 1 (list (cons 'all-saliencies saliencies-per-object))
                         :process process)))

;; -------------------
;; + Context Scaling +
;; -------------------

(defmethod run-process (process
                        (process-label (eql 'context-scale))
                        task
                        agent)
  "Context scale the scene."
  (let* ((prev-process-input (input process))
         (scene (copy-object (find-data prev-process-input 'scene)))
         (features (get-configuration agent :features)))
    (loop for feature in features
          do (multiple-value-bind (min-val max-val)
                 (loop with all-values = nil
                       for object in (entities scene)
                       for feature-val = (get-object-feature object feature)
                       unless (listp feature-val)
                       do (push feature-val all-values)
                       end
                       finally
                       do (return (if all-values
                                    (values (apply #'min all-values)
                                            (apply #'max all-values))
                                    (values nil nil))))
               (when (and min-val max-val)
                 (loop for object in (entities scene)
                       do (scale-object-feature object feature min-val max-val)))))
    (make-process-result 1 (list (cons 'scene scene))
                         :process process)))

;; ----------------
;; + Speech Input +
;; ----------------

(defun prompt ()
  ;; !!! This will only work in LispWorks
  (capi:prompt-for-string "Please enter an utterance:"))

(defun prompt-and-split ()
  ;; !!! This will only work in LispWorks
  (let ((input (capi:prompt-for-string "Please enter an utterance:")))
    (split input #\space)))

(define-event speech-input-finished (utterance list))

(defmethod run-process (process
                        (process-label (eql 'speech-input))
                        task
                        agent)
  "Get an utterance from the human using speech processing"
  (let* ((english (get-configuration agent :english-vocabulary))
         (input-form (get-configuration agent :input-form))
         (words (mapcar (lambda (cxn) (attr-val cxn :form)) (constructions (grammar agent))))
         (vocab (remove-duplicates (append english words) :test #'string=))
         (nr-of-topics (get-configuration agent :nr-of-topics))
         (game-stage (get-configuration agent :game-stage))
         utterance)
    (if (= nr-of-topics 1)
      (speak agent "Choose a word for the topic")
      (speak agent "Choose words for the topics"))
  (case input-form
    (:speech (case game-stage
               (:lexical (when (head-touch-middle agent)
                           (loop with this-utterance = ""
                                 while (= (length this-utterance) 0)
                                 do (setf this-utterance (first (recognise-words agent vocab)))
                                 when (= (length this-utterance) 0)
                                 do (speak agent "I did not understand. Could you repeat please?")
                                 finally
                                 do (push this-utterance utterance))))
               (:grammatical (loop
                              if (head-yes-no agent)
                              do (loop with this-utterance = ""
                                       while (= (length this-utterance) 0)
                                       do (setf this-utterance (first (recognise-words agent vocab)))
                                       when (= (length this-utterance) 0)
                                       do (speak agent "I did not understand. Could you repeat please?")
                                       finally
                                       do (push this-utterance utterance))
                              else
                              do (if (or (null utterance)
                                         (< (length utterance) nr-of-topics))
                                   (speak agent "I did not receive enough words")
                                   (progn
                                     (setf utterance (reverse utterance))
                                     (return)))
                              end))))
    (:text (loop while (< (length utterance) nr-of-topics)
                 do (setf utterance (if (eql game-stage :lexical)
                                      (list (prompt))
                                      (prompt-and-split)))
                 when (< (length utterance) nr-of-topics)
                 do (capi:popup-confirmer nil "Please enter at least one word per topic object"))))
  (notify speech-input-finished utterance)
  (make-process-result 1 (list (cons 'utterance utterance))
                       :process process)))

;; ----------------
;; + Visual Input +
;; ----------------

(defmethod run-process (process
                        (process-label (eql 'visual-input))
                        task
                        agent)
  "Ask the human to show the topic (a single object).
   Use the vision system to get this object.
   We call this the 'observed-topic'.
   This process is used by both speaker and hearer."
  (let* ((prev-process-input (input process))
         (utterance (find-data prev-process-input 'utterance))
         (nr-of-topics (get-configuration agent :nr-of-topics))
         (set-len 0)
         set)
  (speak agent (format nil "Please show me ~a ~a you would call" nr-of-topics (if (> nr-of-topics 1) "objects" "object")))
  (speak agent (format nil "~a" utterance) :speed 75)
  (loop while (not (= set-len nr-of-topics))
        when (head-touch-middle agent)
        do (progn
             (multiple-value-bind (data img) (observe-scene agent :open nil)
               (declare (ignorable img))
               (setf set (alist->object-set data))
               (setf set-len (length (entities set))))
             (when (not (= set-len nr-of-topics))
               (speak agent (format nil "Sorry, I detected ~a objects." set-len))
               (when (< set-len nr-of-topics) (put-back-scene)))))
  (make-process-result 1 (list (cons 'observed-topics (entities set)))
                       :process process)))

;; ----------------------
;; + Match Visual Input +
;; ----------------------

(defmethod run-process (process
                        (process-label (eql 'match-visual-input))
                        task
                        agent)
  "Match the 'observed-topics' with the objects in the original scene.
   Do this by comparing the (x,y) coordinates of the 'observed-topics'
   and the 'observed-scene' (= result of observe-scene process).
   This process is used by both speaker and hearer."
  (let* ((prev-process-input (input process))
         (observed-topics (find-data prev-process-input 'observed-topics))
         (observed-scene (find-data prev-process-input 'observed-scene))
         guessed-topics)
    (loop for topic in observed-topics
          do (loop with closest-object = nil
                   with closest-distance = nil
                   for object in (entities observed-scene)
                   for distance = (euclidean (list (get-object-feature object :xpos)
                                                   (get-object-feature object :ypos))
                                             (list (get-object-feature topic :xpos)
                                                   (get-object-feature topic :ypos)))
                   when (or (null closest-distance)
                            (< distance closest-distance))
                   do (setf closest-object object
                            closest-distance distance)
                   finally
                   do (push closest-object guessed-topics)))
    (make-process-result 1 (list (cons 'observed-topic-ids (mapcar #'id guessed-topics)))
                         :process process)))

;; ---------------------
;; + Determine success +
;; ---------------------

(defmethod run-process (process
                        (process-label (eql 'determine-success))
                        task
                        agent)
  "Determine if the interaction was successful."
  (let* ((prev-process-input (input process))
         (topic-ids (find-data prev-process-input 'topic-ids))
         (observed-topic-ids (find-data prev-process-input 'observed-topic-ids))
         success)
    (put-back-scene)
    (setf success (permutation-of? topic-ids observed-topic-ids))
    (if (speaker? agent)
      (if success
        (speak agent "You are correct!")
        (speak agent "You are wrong!"))
      (if success
        (speak agent "I am correct!")
        (speak agent "I am learning!")))
    (make-process-result 1 (list (cons 'communicated-successfully success))
                         :process process)))