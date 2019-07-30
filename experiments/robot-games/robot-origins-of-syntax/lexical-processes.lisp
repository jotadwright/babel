;;;; /lexical-processes.lisp

(in-package :roos)

;; ---------------------
;; + Conceptualisation +
;; ---------------------

(defun discriminating? (topic-cat others-cat)
  (let ((topic-category (first topic-cat))
        (topic-distance (rest topic-cat))
        (success t))
    (loop for (cat . dist) in others-cat
          when (and (eql cat topic-category)
                    (< dist topic-distance))
          do (progn (setf success nil) (return)))
    success))

(defun categorise (object categories)
  (multiple-value-bind (category distance)
      (the-smallest (lambda (cat)
                      (distance object cat))
                    categories)
    (cons category (abs distance))))

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
      (when (discriminating? topic-cat others-cat)
        (setf problem nil)))
    problem))

(define-event new-category-repair-triggered (new-category category))

(defmethod repair ((repair new-category-repair)
                   (problem conceptualisation-problem)
                   (object process-result)
                   &key trigger)
  "Repair by creating a new category on the most salient channel
   and adding it to the agent's ontology"
  (declare (ignorable trigger))
  (let* ((agent (owner (task (process object))))
         (topic-id (if (speaker? agent)
                     (first (find-data object 'topic-ids))
                     (first (find-data object 'observed-topic-ids))))
         (scene (find-data object 'scene))
         (topic (find-entity-by-id scene topic-id))
         (most-salient-channel (find-data object 'most-salient-channel))
         (new-category (case most-salient-channel
                         (:xpos (make-xpos-category (get-object-feature topic most-salient-channel)))
                         (:ypos (make-ypos-category (get-object-feature topic most-salient-channel)))
                         (:area (make-area-category (get-object-feature topic most-salient-channel)))
                         (:color (make-color-category (get-object-feature topic most-salient-channel))))))
    (push-data (ontology agent) most-salient-channel new-category)
    (notify new-category-repair-triggered new-category)
    (restart-object object nil)
    (make-instance 'fix)))

(define-event lex-conceptualise-finished (topic-cat cons) (most-salient-channel symbol))

(defmethod run-process (process
                        (process-label (eql 'lex-conceptualise))
                        task
                        agent)
  "Conceptualise the topic using the most salient channel.
   The diagnostic will check if conceptualisation succeeded and
   is discriminating. If not, the repair will create a new category
   on the most salient channel and add it to the agent's ontology.
   This process is used by both speaker and hearer. The hearer will
   use this during adoption."
  (let* ((prev-process-input (input process))
         (topic-id (if (speaker? agent)
                     (first (find-data prev-process-input 'topic-ids))
                     (first (find-data prev-process-input 'observed-topic-ids))))
         (scene (find-data prev-process-input 'scene))
         (topic (find-entity-by-id scene topic-id))
         (scene-w/o-topic (remove topic-id (entities scene) :key #'id))
         (all-saliencies (find-data prev-process-input 'all-saliencies))
         (saliencies-for-topic (rest (assoc topic-id all-saliencies)))
         (most-salient-channel (first (the-biggest #'cdr saliencies-for-topic)))
         (categories (find-data (ontology agent) most-salient-channel))
         topic-cat
         others-cat
         process-result)
    (when categories
      (setf topic-cat (categorise topic categories))
      (setf others-cat (mapcar (lambda (obj) (categorise obj categories)) scene-w/o-topic)))
    (setf process-result (make-process-result 1 (list (cons 'topic-cat topic-cat)
                                                      (cons 'others-cat others-cat)
                                                      (cons 'most-salient-channel most-salient-channel))
                                              :process process))
    (unless (notify-learning process-result :trigger 'conceptualisation)
      (notify lex-conceptualise-finished topic-cat most-salient-channel)
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

(defclass invention-repair (repair)
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

(define-event invention-repair-triggered (utterance string))

(defmethod repair ((repair invention-repair)
                   (problem production-problem)
                   (object process-result)
                   &key trigger)
  "Repair by inventing a new form and adding it
   to the agent's lexicon"
  (declare (ignorable trigger))
  (let* ((agent (owner (task (process object))))
         (meaning (first (find-data (process object) 'topic-cat)))
         (form (make-new-word)))
    (add-lex-cxn agent form meaning)
    (notify invention-repair-triggered form)
    (restart-object object nil)
    (make-instance 'fix)))

(define-event lex-produce-finished (utterance string) (applied-cxn fcg-construction))

(defmethod run-process (process
                        (process-label (eql 'lex-produce))
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
    (speak agent (format nil "I call the topic ~a" utterance))
    (notify lex-produce-finished utterance applied-cxn)
    process-result)))

;; -----------
;; + Parsing +
;; -----------

(define-event lex-parse-succeeded (applied-cxn fcg-construction))
(define-event lex-parse-failed)

(defmethod run-process (process
                        (process-label (eql 'lex-parse))
                        task
                        agent)
  "Parse the utterance received through speech recognition."
  (let* ((prev-process-input (input process))
         (utterance (first (find-data prev-process-input 'utterance)))
         (applied-cxn (find-cxn-by-form utterance agent :highest-score)))
    (if applied-cxn
      (notify lex-parse-succeeded applied-cxn)
      (notify lex-parse-failed))
    (unless applied-cxn
      (speak agent (format nil "I do not know the word ~a" utterance)))
    (make-process-result 1 (list (cons 'applied-cxn applied-cxn))
                         :process process)))

;; ------------------
;; + Interpretation +
;; ------------------

(defun interpret (objects category)
  (the-smallest (lambda (object)
                  (distance object category))
                objects))

(defun find-category (id agent)
  (let ((all-channels (get-configuration agent :features)))
    (loop for channel in all-channels
          do (loop for category in (find-data (ontology agent) channel)
                   when (and category (eql (id category) id))
                   do (return-from find-category category)))))

(define-event lex-interpret-finished (topic-id symbol))

(defmethod run-process (process
                        (process-label (eql 'lex-interpret))
                        task
                        agent)
  "Interpret the applied-lex in the context."
  (let* ((prev-process-input (input process))
         (applied-cxn (find-data prev-process-input 'applied-cxn))
         topic)
    (when applied-cxn
       (let ((category (find-category (attr-val applied-cxn :meaning) agent))
             (scene (find-data prev-process-input 'scene)))
         (setf topic (interpret (entities scene) category))
         (notify lex-interpret-finished (id topic))))
    (make-process-result 1 (list (cons 'topic-ids (when topic (list (id topic)))))
                         :process process)))

;; -------------------
;; + Hearer Learning +
;; -------------------

(define-event lex-hearer-conceptualise-finished (topic-cat category) (most-salient-channel symbol))
(define-event lex-hearer-create-finished (topic-cat category) (most-salient-channel symbol))

(defun hearer-conceptualise-or-create (agent process)
  (let* ((prev-process-input (input process))
         (observed-topic-id (first (find-data prev-process-input 'observed-topic-ids)))
         (scene (find-data prev-process-input 'scene))
         (observed-topic (find-entity-by-id scene observed-topic-id))
         (all-saliencies (find-data prev-process-input 'all-saliencies))
         (observed-topic-saliencies (rest (assoc observed-topic-id all-saliencies)))
         (most-salient-channel (first (the-biggest #'cdr observed-topic-saliencies)))
         (channel-categories (find-data (ontology agent) most-salient-channel))
         (scene-w/o-topic (remove observed-topic-id (entities scene) :key #'id))
         (try-new-category (case most-salient-channel
                                (:xpos (make-xpos-category (get-object-feature observed-topic most-salient-channel)))
                                (:ypos (make-ypos-category (get-object-feature observed-topic most-salient-channel)))
                                (:area (make-area-category (get-object-feature observed-topic most-salient-channel)))
                                (:color (make-color-category (get-object-feature observed-topic most-salient-channel))))))
    (if channel-categories
      (let ((topic-cat/dist (categorise observed-topic channel-categories))
            (others-cat/dist (mapcar (lambda (obj) (categorise obj channel-categories)) scene-w/o-topic)))
        (if (discriminating? topic-cat/dist others-cat/dist)
          (progn
            (notify lex-hearer-conceptualise-finished (first topic-cat/dist) most-salient-channel)
            (first topic-cat/dist))
          (progn
            (notify lex-hearer-create-finished try-new-category most-salient-channel)
            (push-data (ontology agent) most-salient-channel try-new-category)
            try-new-category)))
      (progn
        (notify lex-hearer-create-finished try-new-category most-salient-channel)
        (push-data (ontology agent) most-salient-channel try-new-category)
        try-new-category))))

(define-event lex-hearer-learning-finished (new-lex-cxn fcg-construction))

(defmethod run-process (process
                        (process-label (eql 'lex-hearer-learning))
                        task
                        agent)
  "Adopt the unknown utterance. This process assumes the human
   has already shown the correct topic to the robot and the
   robot has processed this with its vision system."
  (let* ((prev-process-input (input process))
         (topic-ids (find-data prev-process-input 'topic-ids)))
    ;; only apply hearer-learning when parsing failed
    (if (not topic-ids)
      (let ((observed-topic-cat (hearer-conceptualise-or-create agent process))
            (utterance (first (find-data prev-process-input 'utterance)))
            new-lex-cxn)
        (setf new-lex-cxn (add-lex-cxn agent utterance observed-topic-cat :score 0.5))
        (notify lex-hearer-learning-finished new-lex-cxn)
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
    (if (listp prototype)
      (mapcar (lambda (p) (set-bound p 0 1)) prototype)
      (set-bound prototype 0 1))))

(defun shift-category (applied-category target-object &key (alpha 0.05))
  (let* ((channel (case (type-of applied-category)
                   (xpos-category :xpos)
                   (ypos-category :ypos)
                   (color-category :color)
                   (area-category :area)))
        (target-feature (get-object-feature target-object channel))
        (new-prototype (if (listp target-feature)
                         (mapcar (lambda (x y)
                                   (+ (* (- 1 alpha) x)
                                      (* alpha y)))
                                 (prototype applied-category)
                                 target-feature)
                         (+ (* (- 1 alpha) (prototype applied-category))
                            (* alpha target-feature)))))
    (setf new-prototype (check-bounds new-prototype))
    (setf (prototype applied-category) new-prototype)))

(define-event alignment-started)
(define-event lexicon-alignment (rewarded list) (punished list))
(define-event category-alignment (shifted list))

(defun align-speaker (agent process success)
  (let* ((li-inc (get-configuration agent :li-inc))
         (li-dec (get-configuration agent :li-dec))
         (alpha (get-configuration agent :alignment-rate))
         (i-number (interaction-number (current-interaction (experiment agent))))
         (scene (find-data (input process) 'scene))
         (topic-id (first (find-data (input process) 'topic-ids)))
         (topic-obj (find-entity-by-id scene topic-id))
         (applied-cxn (find-data (input process) 'applied-cxn))
         (applied-category (find-category (attr-val applied-cxn :meaning) agent)))
    (if success
      (let ((punished-cxns (dec-competitor-score applied-cxn agent :delta li-dec)))
        (notify alignment-started)
        (inc-score applied-cxn :delta li-inc)
        (shift-category applied-category topic-obj :alpha alpha)
        (notify lexicon-alignment (list applied-cxn) punished-cxns)
        (notify category-alignment (list applied-category)))
      (unless (= (attr-val applied-cxn :added) i-number)
        (notify alignment-started)
        (dec-score applied-cxn agent :delta li-dec)
        (notify lexicon-alignment nil (list applied-cxn))))))

(define-event lexicon-added (cxn fcg-construction))
(define-event category-added (cat category))

(defun align-hearer (agent process success)
  (let* ((li-inc (get-configuration agent :li-inc))
         (li-dec (get-configuration agent :li-dec))
         (alpha (get-configuration agent :alignment-rate))
         (i-number (interaction-number (current-interaction (experiment agent))))
         (scene (find-data (input process) 'scene))
         (topic-id (first (find-data (input process) 'observed-topic-ids)))
         (topic-obj (find-entity-by-id scene topic-id))
         (applied-cxn (find-data (input process) 'applied-cxn))
         (applied-category (find-category (attr-val applied-cxn :meaning) agent)))
    (if success
      (let ((punished-cxns (dec-competitor-score applied-cxn agent :delta li-dec)))
        (notify alignment-started)
        (inc-score applied-cxn :delta li-inc)
        (shift-category applied-category topic-obj :alpha alpha)
        (notify lexicon-alignment (list applied-cxn) punished-cxns)
        (notify category-alignment (list applied-category)))
      (unless (= (attr-val applied-cxn :added) i-number)
        ; don't do something when the hearer just learned
        (notify alignment-started)
        (dec-score applied-cxn agent :delta li-dec)
        ;; if the hearer did not have success; maybe it should have used a different
        ;; (existing) category for the utterance OR create a new category and link it
        ;; to the utterance
        (let* ((all-saliencies (find-data (input process) 'all-saliencies))
               (topic-saliencies (rest (assoc topic-id all-saliencies)))
               (most-salient-channel (first (the-biggest #'rest topic-saliencies)))
               (channel-categories (find-data (ontology agent) most-salient-channel))
               (rest (remove topic-id (entities scene) :key #'id))
               (topic-category (categorise topic-obj channel-categories))
               (rest-categories (mapcar (lambda (obj) (categorise obj channel-categories)) rest))
               (discriminating (discriminating? topic-category rest-categories))
               (utterance (first (find-data (input process) 'utterance))))
            (if discriminating
              (let ((cxn (find-cxn-by-form-and-meaning utterance (first topic-category) agent :highest-score)))
                (if cxn
                  (progn
                    (inc-score cxn :delta li-inc)
                    (notify lexicon-alignment (list cxn) (list (applied-cxn agent))))
                  (let ((new-lex-cxn (add-lex-cxn agent utterance (first topic-category))))
                    (notify lexicon-alignment nil (list applied-cxn))
                    (notify lexicon-added new-lex-cxn)))) 
              (let* ((new-category (case most-salient-channel
                                     (:xpos (make-xpos-category (get-object-feature topic-obj most-salient-channel)))
                                     (:ypos (make-ypos-category (get-object-feature topic-obj most-salient-channel)))
                                     (:area (make-area-category (get-object-feature topic-obj most-salient-channel)))
                                     (:color (make-color-category (get-object-feature topic-obj most-salient-channel)))))
                     (new-lex-cxn (add-lex-cxn agent utterance new-category)))
                (push-data (ontology agent) most-salient-channel new-category)
                (notify lexicon-alignment nil (list applied-cxn))
                (notify category-added new-category)
                (notify lexicon-added new-lex-cxn))))))))
      
(defmethod run-process (process
                        (process-label (eql 'lex-consolidate))
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