;;;; ./processes.lisp

(in-package :demo-wtnschp)

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
  (case (get-configuration agent :input-lang)
    (:en (speak agent (format nil "I am the ~a"
                              (discourse-role agent))))
    (:nl (speak agent (format nil "Ik ben de ~a"
                              (case (discourse-role agent)
                                (speaker "spreker")
                                (hearer "luisteraar")))
                :speed 75)))
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

(defclass new-picture-repair (repair)
  ((trigger :initform 'detection))
  (:documentation "Repair by generating a new scene"))

(defmethod diagnose ((diagnostic detection-diagnostic)
                     process-result
                     &key trigger)
  "Diagnose if the right amount of objects is detected"
  (declare (ignorable trigger))
  (let ((context-size (find-data process-result 'required-size))
        (object-set (find-data process-result 'scene)))
    (unless (= context-size (length (entities object-set)))
      (make-instance 'detection-problem :required-context-size context-size))))

(defmethod repair ((repair new-picture-repair)
                   (problem detection-problem)
                   (object process-result)
                   &key trigger)
  "Repair by prompting the user of a problem and asking for a new picture"
  (declare (ignorable trigger))
  (let ((agent (owner (task (process object)))))
    (case (get-configuration agent :input-lang)
      (:en (speak agent (format nil "I could not detect ~a objects"
                                (required-context-size problem))))
      (:nl (speak agent (format nil "Oeps! Ik kan geen ~a monsters vinden"
                                (required-context-size problem)))))
    (head-touch-middle agent)
    (restart-object object nil)
    (make-instance 'fix)))

(define-event observe-scene-finished (img-path string) (scene object-set))

(defmethod run-process (process
                        (process-label (eql 'observe-scene))
                        task
                        agent)
  "Process to observe the scene using the robot's vision.
   The diagnostic will check if the right amount of objects is detected.
   If not, the repair will generate a new scene and restart this process."
  (let ((context-size (get-configuration agent :context-size))
        scene process-result)
    (multiple-value-bind (data img-path) (observe-scene agent :open nil)
      (setf scene (json->object-set data))
      (setf process-result (make-process-result 1 (list (cons 'scene scene)
                                                        (cons 'required-size context-size))
                                                :process process))
      (unless (notify-learning process-result :trigger 'detection)
        (case (get-configuration agent :input-lang)
          (:en (speak agent (format nil "I detected ~a objects" context-size)))
          (:nl (speak agent (format nil "Ik zie ~a monsters" context-size) :speed 75)))
        (when img-path
          (notify observe-scene-finished img-path scene))
        process-result))))

;; ----------------
;; + Choose Topic +
;; ----------------

(define-event choose-topic-finished (topic-id symbol))

(defmethod run-process (process
                        (process-label (eql 'choose-topic))
                        task
                        agent)
  "Choose one topic from the scene."
  (let* ((prev-process-input (input process))
         (scene (find-data prev-process-input 'scene))
         (all-ids (mapcar #'id (entities scene)))
         (topic-id (random-elt all-ids)))
    ;(case (get-configuration agent :input-lang)
    ;  (:en (speak agent "I chose the topic."))
    ;  (:nl (speak agent "Ik heb een onderwerp gekozen" :speed 75)))
    (notify choose-topic-finished topic-id)
    (make-process-result 1 (list (cons 'topic-id topic-id))
                         :process process)))

;; ---------------------
;; + Conceptualisation +
;; ---------------------

(defun categorise (object categories)
  (multiple-value-bind (category distance)
      (the-smallest (lambda (cat)
                      (distance object cat))
                    categories)
    (cons category (abs distance))))

(defun speaker-discriminating-p (topic-cat others-cat)
  (let ((topic-category (first topic-cat))
        (topic-distance (rest topic-cat))
        (success t))
    (loop for (cat . dist) in others-cat
          when (and (eql cat topic-category)
                    (< dist topic-distance))
          do (progn (setf success nil) (return)))
    success))

(defun hearer-discriminating-p (topic-cat others-cat)
  (let ((topic-category (first topic-cat))
        (topic-distance (rest topic-cat))
        (success t))
    (loop for (cat . dist) in others-cat
          when (eql cat topic-category)
          do (progn (setf success nil) (return)))
    success))

(defclass conceptualisation-problem (problem)
  ()
  (:documentation "Problem created when conceptualisation fails,
                   i.e. when no meaning is found for the topic"))

(defclass conceptualisation-diagnostic (diagnostic)
  ((trigger :initform 'conceptualisation))
  (:documentation "Diagnostic to check the result of the conceptualisation process"))

(defclass new-category-repair (repair)
  ((trigger :initform 'conceptualisation))
  (:documentation "Repair created when the concepualisation problem
                   occurred. The repair consist of creating a new
                   category."))

(defmethod diagnose ((diagnostic conceptualisation-diagnostic)
                     process-result
                     &key trigger)
  "Diagnose if categorisation succeeded and is discriminating"
  (declare (ignorable trigger))
  (let ((topic-cat (find-data process-result 'topic-cat))
        (others-cat (find-data process-result 'others-cat))
        (problem (make-instance 'conceptualisation-problem)))
    (when topic-cat
      (when (speaker-discriminating-p topic-cat others-cat)
        (setf problem nil)))
    problem))

(define-event new-category-repair-triggered (new-category category))

(defmethod repair ((repair new-category-repair)
                   (problem conceptualisation-problem)
                   (object process-result)
                   &key trigger)
  "Repair by creating a new category
   and adding it to the agent's ontology"
  (declare (ignorable trigger))
  (let* ((agent (owner (task (process object))))
         (topic-id (if (speaker? agent)
                     (find-data object 'topic-id)
                     (find-data object 'observed-topic-id)))
         (scene (find-data object 'scene))
         (topic (find-entity-by-id scene topic-id))
         (new-category (make-color-category (rgbcolor topic))))
    (push-data (ontology agent) 'color-categories new-category)
    (notify new-category-repair-triggered new-category)
    (restart-object object nil)
    (make-instance 'fix)))

(define-event conceptualise-finished (topic-cat cons))

(defmethod run-process (process
                        (process-label (eql 'conceptualise))
                        task
                        agent)
  "Conceptualise the topic by searching for the color category that is
   closest to it.
   The diagnostic will check if conceptualisation succeeded.
   If not, the repair will create a new category
   and add it to the agent's ontology.
   This process is used by both speaker and hearer. The hearer will
   use this during adoption."
  (let* ((prev-process-input (input process))
         (topic-id (if (speaker? agent)
                     (find-data prev-process-input 'topic-id)
                     (find-data prev-process-input 'observed-topic-id)))
         (scene (find-data prev-process-input 'scene))
         (topic (find-entity-by-id scene topic-id))
         (others (remove topic (entities scene)))
         (color-categories (find-data (ontology agent) 'color-categories))
         topic-cat
         others-cat
         process-result)
    (when color-categories
      (setf topic-cat (categorise topic color-categories))
      (setf others-cat (loop for obj in others
                             collect (categorise obj color-categories))))
    (setf process-result (make-process-result 1 (list (cons 'topic-cat topic-cat)
                                                      (cons 'others-cat others-cat))
                                              :process process))
    (unless (notify-learning process-result :trigger 'conceptualisation)
      (notify conceptualise-finished topic-cat)
      process-result)))

;; --------------
;; + Production +
;; --------------

(defclass production-problem (problem)
  ()
  (:documentation "Problem created when production fails,
                   i.e. when no word is found for the meaning"))

(defclass production-diagnostic (diagnostic)
  ((trigger :initform 'production))
  (:documentation "Diagnostic to check the result of production process"))

(defclass new-word-repair (repair)
  ((trigger :initform 'production))
  (:documentation "Repair created when production-problem occurred.
                   The repair consists of inventing a new word"))

(defmethod diagnose ((diagnostic production-diagnostic)
                     process-result
                     &key trigger)
  "Diagnose if the agent found a form for the given meaning"
  (declare (ignorable trigger))
  (let ((utterance (find-data process-result 'utterance))
        (problem (make-instance 'production-problem)))
    (when utterance
      (setf problem nil))
    problem))

(define-event new-word-repair-triggered (utterance string))

(defvar *used-dutch-nonsense-words* nil)

(defmethod repair ((repair new-word-repair)
                   (problem production-problem)
                   (object process-result)
                   &key trigger)
  "Repair by inventing a new form and adding it
   to the agent's lexicon"
  (declare (ignorable trigger))
  (let* ((agent (owner (task (process object))))
         (meaning (first (find-data (process object) 'topic-cat)))
         (form (case (get-configuration agent :input-lang)
                 (:en (make-new-word))
                 (:nl (loop with chosen-word = nil
                            while (null chosen-word)
                            for maybe-word = (random-elt (get-configuration agent :dutch-nonsense))
                            unless (member maybe-word *used-dutch-nonsense-words*)
                            do (progn (push maybe-word *used-dutch-nonsense-words*)
                                 (setf chosen-word maybe-word))
                            finally (return chosen-word))))))
    (add-lex-cxn agent form meaning)
    (notify new-word-repair-triggered form)
    (restart-object object nil)
    (make-instance 'fix)))

(define-event produce-finished (utterance string) (applied-cxn fcg-construction))

(defmethod run-process (process
                        (process-label (eql 'produce))
                        task
                        agent)
  "Produce a form for the category found for the topic.
   The diagnostic will check if the agent knows a word for this
   category. If not, the repair will invent a new one and add it
   to the agent's lexicon."
  (let* ((prev-process-input (input process))
         (meaning (first (find-data prev-process-input 'topic-cat)))
         (applied-cxn (find-cxn-by-meaning meaning agent :highest-score))
         (utterance (when applied-cxn (attr-val applied-cxn :form)))
         process-result)
  (setf process-result (make-process-result 1 (list (cons 'applied-cxn applied-cxn)
                                                    (cons 'utterance utterance))
                                            :process process))
  (unless (notify-learning process-result :trigger 'production)
    (case (get-configuration agent :input-lang)
      (:en (speak agent (format nil "I am thinking of an object and it has the color ~a" utterance)))
      (:nl (speak agent (format nil "Ik heb een monster in mijn gedachten en het heeft de kleur ~a" utterance) :speed 75)))
    (notify produce-finished utterance applied-cxn)
    process-result)))


;; -----------
;; + Parsing +
;; -----------

(define-event parse-succeeded (applied-cxn fcg-construction))
(define-event parse-failed)

(defmethod run-process (process
                        (process-label (eql 'parse))
                        task
                        agent)
  "Parse the utterance received through speech recognition."
  (let* ((prev-process-input (input process))
         (utterance (find-data prev-process-input 'utterance))
         (applied-cxn (find-cxn-by-form utterance agent :highest-score)))
    (if applied-cxn
      (notify parse-succeeded applied-cxn)
      (notify parse-failed))
    (unless applied-cxn
      (case (get-configuration agent :input-lang)
        (:en (speak agent (format nil "I do not know the word ~a" utterance)))
        (:nl (speak agent (format nil "Ik ken het woord ~a niet" utterance) :speed 75))))
    (make-process-result 1 (list (cons 'applied-cxn applied-cxn))
                         :process process)))


;; ------------------
;; + Interpretation +
;; ------------------

(defun interpret (objects category)
  (the-smallest (lambda (object)
                  (distance object category))
                objects))

(define-event interpret-finished (topic-id symbol))

(defun robot-point-to-topic (agent topic scene)
  (let* ((sorted-on-x-axis (sort (entities scene) #'< :key #'xpos))
         (topic-position (position topic sorted-on-x-axis)))
    (case topic-position
      (0 (point agent :left))
      (1 (point agent :both))
      (2 (point agent :right)))))

(defmethod run-process (process
                        (process-label (eql 'interpret))
                        task
                        agent)
  "Interpret the applied-lex in the context."
  (let* ((prev-process-input (input process))
         (applied-cxn (find-data prev-process-input 'applied-cxn))
         topic)
    (when applied-cxn
       (let ((category (find (attr-val applied-cxn :meaning)
                             (get-data (ontology agent) 'color-categories)
                             :key #'id))
             (scene (find-data prev-process-input 'scene)))
         (setf topic (interpret (entities scene) category))
         (case (get-configuration agent :input-lang)
           (:en (speak agent "I think I know! I will point to it"))
           (:nl (speak agent "Ik denk dat ik het weet! Ik zal er naar wijzen" :speed 75)))
         (robot-point-to-topic agent topic scene)
         (notify interpret-finished (id topic))))
    (make-process-result 1 (list (cons 'topic-id (when topic (id topic))))
                         :process process)))

;; ----------------
;; + Speech Input +
;; ----------------

(defun prompt ()
  #+LISPWORKS (capi:prompt-for-string "Please enter an utterance:")
  #+CCL (ccl:get-string-from-user "Please enter an utterance:"))

(defun prompt-correct-speech-input (utterance)
  #+LISPWORKS (multiple-value-bind (res success) (capi:prompt-for-confirmation (format nil "Did you say ~a?" utterance))
                (if success
                  (if res
                    utterance
                    (capi:prompt-for-string "Please enter an utterance:"))
                  (prompt-correct-speech-input utterance)))
  #+CCL (let ((response (ccl:get-string-from-user (format nil "Did you say ~a? (y/n)" utterance))))
          (cond
           ((or (string= response "y")
                (string= response "Y")
                (string= response "yes")
                (string= response "YES")
                (string= response "Yes")) utterance)
           ((or (string= response "n")
                (string= response "N")
                (string= response "no")
                (string= response "NO")
                (string= response "No")) (ccl:get-string-from-user "Please enter an utterance:"))
           (t
            (format t "Invalid answer. Please reply with 'y' or 'n'~%")
            (prompt-correct-speech-input utterance)))))

(define-event speech-input-finished (utterance string))

(defmethod run-process (process
                        (process-label (eql 'speech-input))
                        task
                        agent)
  "Get an utterance from the human using speech processing"
  (let* ((vocab (rest (assoc (get-configuration agent :input-lang)
                       (get-configuration agent :robot-vocabulary))))
         (input-form (get-configuration agent :input-form))
         (words (mapcar (lambda (cxn)
                          (attr-val cxn :form))
                        (constructions (grammar agent))))
         utterance)
    (setf vocab
          (remove-duplicates (append vocab words) :test #'string=))
    (case (get-configuration agent :input-lang)
      (:en (speak agent "Choose an object and say its color"))
      (:nl (speak agent "Kies een monster en benoem zijn kleur" :speed 75)))
    (case input-form
      (:speech (when (head-touch-middle agent)
                 (loop with this-utterance = ""
                       while (= (length this-utterance) 0)
                       do (setf this-utterance (first (recognise-words agent vocab)))
                       when (= (length this-utterance) 0)
                       do (case (get-configuration agent :input-lang)
                            (:en (speak agent "I did not understand. Could you repeat please?"))
                            (:nl (speak agent "Dat heb ik niet begrepen. Kan je dat herhalen alsjeblief?" :speed 75)))
                       finally
                       (setf utterance this-utterance))
                 (setf utterance (prompt-correct-speech-input utterance))))
      (:text (loop while (null utterance)
                   for input-utterance = (list (prompt))
                   if (not (= (length input-utterance) 1))
                   do (capi:popup-confirmer nil "Please enter one word")
                   else
                   do (setf utterance (first input-utterance)))))
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
         observedp observed-topic)
    ;; if hearer and interpretation succeeded
    (if (and (eql (discourse-role agent) 'hearer)
             (find-data prev-process-input 'topic-id))
      (case (get-configuration agent :input-lang)
        (:en (speak agent (format nil "Am I correct? Please show me the object you would call ~a" utterance)))
        (:nl (speak agent (format nil "Had ik het juist? Welk monster had jij in gedachten?" utterance) :speed 75)))
      ;; else; if speaker or unknown word
      (case (get-configuration agent :input-lang)
        (:en (speak agent (format nil "Please show me the object you would call ~a" utterance)))
        (:nl (speak agent (format nil "Toon mij het monster dat jij ~a zou noemen" utterance) :speed 75))))
    (loop while (null observedp)
          when (head-touch-middle agent)
          do (multiple-value-bind (data img) (observe-scene agent :open nil)
               (declare (ignorable img))
               (let ((object-set (json->object-set data)))
                 (when (= (length (entities object-set)) 1)
                   (setf observedp t)
                   (setf observed-topic (first (entities object-set))))
                 (unless (= (length (entities object-set)) 1)
                   (case (get-configuration agent :input-lang)
                     (:en (speak agent (format nil "Sorry, I detected ~a objects" (length (entities object-set)))))
                     (:nl (speak agent (format nil "Sorry, ik zie ~a monsters" (length (entities object-set))) :speed 75)))))))
    (make-process-result 1 (list (cons 'observed-topic observed-topic))
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
         (observed-topic (find-data prev-process-input 'observed-topic))
         (scene (find-data prev-process-input 'scene))
         guessed-topic)
    (setf guessed-topic
          (the-smallest (lambda (obj)
                          (euclidean (list (xpos obj) (ypos obj))
                                     (list (xpos observed-topic) (ypos observed-topic))))
                          (entities scene)))
    (make-process-result 1 (list (cons 'observed-topic-id (id guessed-topic)))
                         :process process)))

;; ---------------------
;; + Determine success +
;; ---------------------

(defvar *speaker-success-messages* '("Proficiat! Je hebt het juist"
                                     "Gefeliciteerd! Je hebt het juist"
                                     "Bravo! Goed gedaan"))
(defvar *speaker-failure-messages* '("Jammer! Je hebt het fout"
                                     "Helaas! Je hebt het fout"
                                     "Spijtig! Dat had ik niet in gedachten"))
(defvar *hearer-success-messages* '("Hoera! Ik heb het juist"
                                    "Joepie! Ik was juist"
                                    "Ja! Ik heb het juist"))
(defvar *hearer-failure-messages* '("Dankjewel! Ik heb weer iets bijgeleerd"
                                    "Ok, bedankt! Ik heb weer iets bijgeleerd"
                                    "Dankzij jou blijf ik maar leren"))

(defun robot-give-feedback (agent topic scene)
  (let* ((sorted-on-x-axis (sort (entities scene) #'< :key #'xpos))
         (topic-position (position topic sorted-on-x-axis :key #'id)))
    (case topic-position
      (0 (case (get-configuration agent :input-lang)
           (:en (speak agent "I meant the object on the left"))
           (:nl (speak agent "Ik bedoelde het linkse monster. Volgende keer beter"))))
      (1 (case (get-configuration agent :input-lang)
           (:en (speak agent "I meant the object in the middle"))
           (:nl (speak agent "Ik bedoelde het middelste monster. Volgende keer beter"))))
      (2 (case (get-configuration agent :input-lang)
           (:en (speak agent "I meant the object on the right"))
           (:nl (speak agent "Ik bedoelde het rechtse monster. Volgende keer beter")))))))

(defmethod run-process (process
                        (process-label (eql 'determine-success))
                        task
                        agent)
  "Determine if the interaction was successful."
  (let* ((prev-process-input (input process))
         (topic-id (find-data prev-process-input 'topic-id))
         (observed-topic-id (find-data prev-process-input 'observed-topic-id))
         (scene (find-data prev-process-input 'scene))
         success)
    (setf success (eql topic-id observed-topic-id))
    (if (speaker? agent)
      (if success
        (case (get-configuration agent :input-lang)
          (:en (speak agent "You are correct!"))
          (:nl (speak agent (random-elt *speaker-success-messages*) :speed 75)))
        (case (get-configuration agent :input-lang)
          (:en (progn (speak agent "You are wrong!")
                 (robot-give-feedback agent topic-id scene)))
          (:nl (progn
                 (speak agent (random-elt *speaker-failure-messages*) :speed 75)
                 (robot-give-feedback agent topic-id scene)))))
      (if success
        (case (get-configuration agent :input-lang)
          (:en (speak agent "I am correct!"))
          (:nl (speak agent (random-elt *hearer-success-messages*) :speed 75)))
        (case (get-configuration agent :input-lang)
          (:en (speak agent "I am learning!"))
          (:nl (speak agent (random-elt *hearer-failure-messages*) :speed 75)))))
    (make-process-result 1 (list (cons 'communicated-successfully success))
                         :process process)))

;; -------------------
;; + Hearer Learning +
;; -------------------

(define-event hearer-conceptualise-finished (topic-cat category))
(define-event hearer-create-finished (topic-cat category))

(defun hearer-conceptualise-or-create (agent process)
  (let* ((prev-process-input (input process))
         (observed-topic-id (find-data prev-process-input 'observed-topic-id))
         (scene (find-data prev-process-input 'scene))
         (observed-topic (find-entity-by-id scene observed-topic-id))
         (color-categories (find-data (ontology agent) 'color-categories))
         (scene-w/o-topic (remove observed-topic-id (entities scene) :key #'id))
         (try-new-category (make-color-category (rgbcolor observed-topic))))
    (if color-categories
      (let ((topic-cat/dist (categorise observed-topic color-categories))
            (others-cat/dist (mapcar (lambda (obj) (categorise obj color-categories)) scene-w/o-topic)))
        (if (hearer-discriminating-p topic-cat/dist others-cat/dist)
          (progn
            (notify hearer-conceptualise-finished (first topic-cat/dist))
            (first topic-cat/dist))
          (progn
            (notify hearer-create-finished try-new-category)
            (push-data (ontology agent) 'color-categories try-new-category)
            try-new-category)))
      (progn
        (notify hearer-create-finished try-new-category)
        (push-data (ontology agent) 'color-categories try-new-category)
        try-new-category))))

(define-event hearer-learning-finished (new-lex-cxn fcg-construction))

(defmethod run-process (process
                        (process-label (eql 'hearer-learning))
                        task
                        agent)
  "Adopt the unknown utterance. This process assumes the human
   has already shown the correct topic to the robot and the
   robot has processed this with its vision system."
  (let* ((prev-process-input (input process))
         (topic-id (find-data prev-process-input 'topic-id)))
    ;; only apply hearer-learning when parsing failed
    (if (not topic-id)
      (let ((observed-topic-cat (hearer-conceptualise-or-create agent process))
            (utterance (find-data prev-process-input 'utterance))
            new-lex-cxn)
        (setf new-lex-cxn (add-lex-cxn agent utterance observed-topic-cat :score 0.5))
        (notify hearer-learning-finished new-lex-cxn)
        (make-process-result 1 (list (cons 'applied-cxn new-lex-cxn))
                             :process process))
      (make-process-result 1 nil :process process))))

;; -----------------
;; + Consolidation +
;; -----------------

(defun check-bounds (prototype)
  (labels ((set-bound (value min-bound max-bound)
             (cond ((> value max-bound) max-bound)
                   ((< value min-bound) min-bound)
                   (t value))))
    (mapcar (lambda (p) (set-bound p 0 255)) prototype)))

(defun shift-category (applied-category target-object &key (alpha 0.05))
  (let* ((target-color (rgbcolor target-object))
         (new-prototype (mapcar (lambda (x y)
                                  (+ (* (- 1 alpha) x)
                                     (* alpha y)))
                                (prototype applied-category)
                                target-color)))
    (setf new-prototype (check-bounds new-prototype))
    (setf (prototype applied-category) new-prototype)))

(define-event alignment-started)
(define-event lexicon-alignment (rewarded list) (punished list))
(define-event category-alignment (shifted list))

(defun align-speaker (agent process success)
  (let* ((li-inc (get-configuration agent :li-inc))
         (li-dec (get-configuration agent :li-dec))
         (alpha (get-configuration agent :alpha))
         (i-number (interaction-number (current-interaction (experiment agent))))
         (scene (find-data (input process) 'scene))
         (topic-id (find-data (input process) 'topic-id))
         (topic-obj (find-entity-by-id scene topic-id))
         (applied-cxn (find-data (input process) 'applied-cxn))
         (applied-category (find (attr-val applied-cxn :meaning)
                                 (get-data (ontology agent) 'color-categories)
                                 :key #'id)))
    (unless (= (attr-val applied-cxn :added) i-number)
      ; don't do something when the speaker just invented
      (if success
        (let ((punished-cxns (dec-competitor-score applied-cxn agent :delta li-dec)))
          (notify alignment-started)
          (inc-score applied-cxn :delta li-inc)
          (shift-category applied-category topic-obj :alpha alpha)
          (notify lexicon-alignment (list applied-cxn) punished-cxns)
          (notify category-alignment (list applied-category)))
        (progn
          (notify alignment-started)
          (dec-score applied-cxn agent :delta li-dec)
          (notify lexicon-alignment nil (list applied-cxn)))))))

(define-event lexicon-added (cxn fcg-construction))
(define-event category-added (cat category))

(defun align-hearer (agent process success)
  (let* ((li-inc (get-configuration agent :li-inc))
         (li-dec (get-configuration agent :li-dec))
         (alpha (get-configuration agent :alpha))
         (i-number (interaction-number (current-interaction (experiment agent))))
         (scene (find-data (input process) 'scene))
         (topic-id (find-data (input process) 'observed-topic-id))
         (topic-obj (find-entity-by-id scene topic-id))
         (applied-cxn (find-data (input process) 'applied-cxn))
         (applied-category (find (attr-val applied-cxn :meaning)
                                 (get-data (ontology agent) 'color-categories)
                                 :key #'id)))
    (unless (= (attr-val applied-cxn :added) i-number)
      ; don't do something when the hearer just learned
      (if success
        (let ((punished-cxns (dec-competitor-score applied-cxn agent :delta li-dec)))
          (notify alignment-started)
          (inc-score applied-cxn :delta li-inc)
          (shift-category applied-category topic-obj :alpha alpha)
          (notify lexicon-alignment (list applied-cxn) punished-cxns)
          (notify category-alignment (list applied-category)))
        (progn
          (notify alignment-started)
          (dec-score applied-cxn agent :delta li-dec)
          ;; if the hearer did not have success; maybe it should have used a different
          ;; (existing) category for the utterance OR create a new category and link it
          ;; to the utterance
          (let* ((color-categories (find-data (ontology agent) 'color-categories))
                 (rest (remove topic-id (entities scene) :key #'id))
                 (topic-category (categorise topic-obj color-categories))
                 (rest-categories (mapcar (lambda (obj) (categorise obj color-categories)) rest))
                 (discriminating (hearer-discriminating-p topic-category rest-categories))
                 (utterance (find-data (input process) 'utterance)))
            (if discriminating
              (let ((cxn (find-cxn-by-form-and-meaning utterance (id (first topic-category)) agent :highest-score)))
                (if cxn
                  (progn
                    (inc-score cxn :delta li-inc)
                    (notify lexicon-alignment (list cxn) (list (applied-cxn agent))))
                  (let ((new-lex-cxn (add-lex-cxn agent utterance (first topic-category))))
                    (notify lexicon-alignment nil (list applied-cxn))
                    (notify lexicon-added new-lex-cxn)))) 
              (let* ((new-category (make-color-category (rgbcolor topic-obj)))
                     (new-lex-cxn (add-lex-cxn agent utterance new-category)))
                (push-data (ontology agent) 'color-categories new-category)
                (notify lexicon-alignment nil (list applied-cxn))
                (notify category-added new-category)
                (notify lexicon-added new-lex-cxn)))))))))

      
(defmethod run-process (process
                        (process-label (eql 'consolidate))
                        task
                        agent)
  "Do alignment"
  (let* ((prev-process-input (input process))
         (success (find-data prev-process-input 'communicated-successfully))
         (who-aligns (get-configuration agent :who-aligns)))
    (case who-aligns
      (:both (if (speaker? agent)
               (align-speaker agent process success)
               (align-hearer agent process success)))
      (:speaker (when (speaker? agent)
                  (align-speaker agent process success)))
      (:hearer (when (hearer? agent)
                 (align-hearer agent process success))))
    (make-process-result 1 nil :process process)))