(in-package :clevr-learning)

;; #########
;; + Agent +
;; #########
(defclass holophrase-agent (agent)
  ((grammar
    :documentation "The agent's grammar"
    :accessor grammar :initarg :grammar :initform nil :type fcg-construction-set)
   (ontology
    :documentation "The agent's ontology"
    :accessor ontology :initarg :ontology :initform nil :type blackboard)
   (primitives
    :documentation "The available primitive operations"
    :accessor primitives :initarg :primitives :initform nil :type list)
   (applicable-chunks
    :documentation "The chunks that can be used for production"
    :accessor applicable-chunks :initarg :applicable-chunks :initform nil :type list)
   (applied-cxn
    :documentation "The cxn used in the current interaction"
    :accessor applied-cxn :initarg :applied-cxn :initform nil :type (or null fcg-construction))
   (applied-chunk
    :documentation "The chunk used in the current interaction"
    :accessor applied-chunk :initarg :applied-chunk :initform nil :type (or null chunk))
   (found-answer
    :documentation "The answer found by executing the program"
    :accessor found-answer :initarg :found-answer :initform nil :type (or null category number))
   (question-object
    :documentation "The question objects used"
    :accessor question-object :initarg :question-object :initform nil :type (or null clevr-question))
   (trash
    :documentation "The failed programs, used in learning strategy :keep-trash"
    :accessor trash :initarg :trash :initform nil :type list)
   (samples
    :documentation "The seen samples, used in learning strategy :keep-samples"
    :accessor samples :initarg :samples :initform nil :type list))
  (:documentation "The VQA Agent"))


(defun learnerp (agent)
  (eql (id agent) 'learner))

(defun tutorp (agent)
  (eql (id agent) 'tutor))


;; #####################
;; + Conceptualisation +
;; #####################
(defun get-possible-primitives (answer)
  "get all the primitives that might return the given answer"
  (etypecase answer
    (number '(count!))
    (boolean-category '(exist equal? less-than greater-than equal-integer))
    (attribute '(query))))

(defun get-final-primitive (chunk)
  "Get the final primitive from the chunk"
  (first
   (find (car (target-var chunk))
         (irl-program chunk)
         :test #'member)))

(define-event conceptualisation-started (answer t) (possible-chunks list))
(define-event conceptualisation-finished (applicable-chunks list))

(defmethod conceptualise ((agent holophrase-agent))
  "The agent looks for all chunks that lead to the given answer
   in the current scene."
  (let* ((possible-final-primitives (get-possible-primitives (found-answer agent)))
         (all-chunks (find-data (ontology agent) 'programs))
         (consider-chunks (remove-if-not #'(lambda (chunk)
                                             (member (get-final-primitive chunk)
                                                     possible-final-primitives))
                                         all-chunks)))
    (notify conceptualisation-started (found-answer agent) consider-chunks)
    (when consider-chunks
      (setf (applicable-chunks agent)
            (loop for chunk in (shuffle consider-chunks)
                  for solution = (with-disabled-monitors
                                   (first (evaluate-irl-program (irl-program chunk) (ontology agent))))
                  for answer = (when solution
                                 (get-target-value (irl-program chunk) solution))
                  when (equal-entity answer (found-answer agent))
                  collect chunk))))
  (notify conceptualisation-finished (applicable-chunks agent))
  (applicable-chunks agent))

#|
(defmethod conceptualise ((agent holophrase-agent))
  ;; the learner gets an answer and needs to find a chunk in its
  ;; ontology that returns that given answer in the current scene
  ;;   --> how to speed this up? by looking at the type of the answer and the last primitive in the chunk
  ;;   --> take the one with the highest score? Yes
  ;;   --> what if none are found? Interaction ends, nothing happens.
  (let* ((possible-final-primitives (get-possible-primitives (found-answer agent)))
         (all-chunks (find-data (ontology agent) 'programs))
         (consider-chunks (remove-if-not #'(lambda (chunk)
                                             (member (get-final-primitive chunk)
                                                     possible-final-primitives))
                                         all-chunks)))
    (notify conceptualisation-started (found-answer agent) consider-chunks)
    (setf (applied-chunk agent)
          (loop for chunk in (shuffle consider-chunks) ;(sort consider-chunks #'> :key #'score)
                for solution = (first (with-disabled-monitors
                                        (evaluate-irl-program (irl-program chunk) (ontology agent))))
                for answer = (when solution (get-target-value (irl-program chunk) solution))
                when (equal-entity answer (found-answer agent))
                return chunk))
    (notify conceptualisation-finished (applied-chunk agent))
    (applied-chunk agent)))
|#

;; ##############
;; + Production +
;; ##############
(define-event production-finished (applied-cxn t))

(defmethod produce-question ((agent holophrase-agent))
  "For each of the applicable chunks, the learer gets
   all cxns that have this chunk as meaning. The learner produces
   the utterance with the highest score. If multiple cxns have the
   same highest score, take one at random."
  (when (and (applicable-chunks agent)
             (constructions (grammar agent)))
    (let ((consider-cxns
           (remove-duplicates
            (loop for chunk in (applicable-chunks agent)
                  for cxns = (find-all (id chunk) (constructions (grammar agent))
                                       :key #'(lambda (cxn) (attr-val cxn :meaning)))
                  append cxns))))
      (when consider-cxns
        (setf (applied-cxn agent)
              (the-biggest #'(lambda (cxn) (attr-val cxn :score))
                           consider-cxns))
        (setf (applied-chunk agent)
              (get-chunk agent (attr-val (applied-cxn agent) :meaning)))
        (setf (utterance agent)
              (attr-val (applied-cxn agent) :form)))))
  (notify production-finished (applied-cxn agent))
  (utterance agent))

#|
(defmethod produce-question ((agent holophrase-agent))
  ;; the learner has an applied-chunk and now looks for a cxn
  ;; that has that chunk as meaning --> using fcg:formulate
  (multiple-value-bind (utterance cipn)
      (formulate
       (fcg::instantiate-variables
        (irl-program
         (applied-chunk agent)))
       :cxn-inventory (grammar agent))
    (when (and utterance (eql (first (statuses cipn)) 'fcg::succeeded))
      (setf (applied-cxn agent)
            (get-original-cxn (first (applied-constructions cipn))))
      (setf (utterance agent) (list-of-strings->string utterance)))
    (notify production-finished (applied-cxn agent)))
  (utterance agent))
|#

;; ###########################
;; + Tutor validates success +
;; ###########################
(defmethod tutor-interprets ((agent holophrase-agent))
  "The tutor gets the utterance from the learner, parses it and
   executes the result in the scene. The answers of both tutor
   and learner are compared."
  (let* ((irl-program (comprehend (preprocess-sentence (utterance agent))
                                  :cxn-inventory (grammar agent)))
         (solutions (with-disabled-monitors
                     (evaluate-irl-program irl-program (ontology agent))))
         solution)
    (when (and solutions (= (length solutions) 1))
      (setf solution (first solutions))
      (setf (found-answer agent)
            (get-target-value irl-program solution)))
    (found-answer agent)))


#|
;;;; Speaker learning
;; This function is called when the speaker was able to conceptualise and produce, but
;; the hearer (tutor) found a different answer to the question given by the speaker.
;; --> the speaker should find another program that leads to the given answer in this scene
;; --> the speaker fetches the utterance linked to this program and proposes this to the tutor
;; --> the tutor validates
;; --> if correct, the speaker treats this as a success and rewards it
(defmethod speaker-learning ((speaker holophrase-agent) (hearer holophrase-agent))
  (let* ((possible-final-primitives (get-possible-primitives (found-answer speaker)))
         (all-chunks (find-data (ontology speaker) 'programs))
         (consider-chunks (remove-if-not #'(lambda (chunk)
                                             (member (get-final-primitive chunk)
                                                     possible-final-primitives))
                                         all-chunks)))
    (loop for chunk in (shuffle consider-chunks) ;(sort consider-chunks #'> :key #'score) 
          for solution = (first (with-disabled-monitors
                                  (evaluate-irl-program (irl-program chunk) (ontology speaker))))
          for answer = (when solution (get-target-value (irl-program chunk) solution))
          when (equal-entity answer (found-answer speaker))
          do (loop with consider-cxns = (remove (applied-cxn speaker)
                                                (find-all (id chunk) (constructions (grammar speaker))
                                                          :key #'(lambda (cxn) (attr-val cxn :meaning))))
                   for cxn in (sort consider-cxns #'>
                                    :key (lambda (cxn) (attr-val cxn :score)))
                   do (setf (utterance speaker) (attr-val cxn :form))
                   when (tutor-validates-success hearer speaker)
                   do (progn (setf (alternative-chunk speaker) chunk
                                   (alternative-cxn speaker) cxn)
                        (return-from speaker-learning t))))))
|#








;; ###########
;; + Parsing +
;; ###########
(define-event parsing-finished (applied-cxn t))

(defmethod parse-question ((agent holophrase-agent))
  "Comprehend the utterance."
  (multiple-value-bind (irl-program cipn)
      (comprehend (utterance agent) :cxn-inventory (grammar agent))
    (when (and irl-program (eql (first (statuses cipn)) 'fcg::succeeded))
      (setf (applied-cxn agent)
            (get-original-cxn (first (applied-constructions cipn))))))
  (notify parsing-finished (applied-cxn agent))
  (applied-cxn agent))


;; ##################
;; + Interpretation +
;; ##################
(define-event interpretation-finished (found-answer t))

(defmethod interpret ((agent holophrase-agent))
  "Interpret the meaning in the context"
  (let* ((chunk (get-chunk agent (attr-val (applied-cxn agent) :meaning)))
         (solutions (evaluate-irl-program (irl-program chunk) (ontology agent))))
    (when (length= solutions 1)
      (let* ((solution (first solutions))
             (answer (get-target-value (irl-program chunk) solution)))
        (setf (applied-chunk agent) chunk
              (found-answer agent) answer)))
    (notify interpretation-finished (found-answer agent))
    (found-answer agent)))

;; ############
;; + Adoption +
;; ############
(define-event adoption-started)
(define-event added-to-trash (irl-program list))
(define-event composition-solution-found (solution chunk-evaluation-result))

(defmethod add-trash ((agent holophrase-agent))
  ;; trash strategy: add program to trash, so the composer can take this into account
  (let ((cxn-w-utterance (find (utterance agent) (constructions (grammar agent))
                                :key #'(lambda (cxn) (attr-val cxn :form)) :test #'string=)))
    (when cxn-w-utterance
      (let ((found (assoc (utterance agent) (trash agent) :test #'string=))
            (irl-program (irl-program (get-chunk agent (attr-val cxn-w-utterance :meaning)))))
        (notify added-to-trash irl-program)
        (if found
          (push irl-program (cdr found))
          (push (cons (utterance agent) (list irl-program)) (trash agent)))))))

(defmethod adopt ((agent holophrase-agent) answer)
  "Compose a program that results in the given answer
   and store this in the grammar of the agent."
  (let ((learning-strategy (get-configuration agent :learning-strategy)))
    (notify adoption-started)
    ;; use the learning strategy
    (when (eql learning-strategy :keep-trash)
      (add-trash agent))
    ;; compose a new solution
    (let* ((cxn-w-utterance (find (utterance agent) (constructions (grammar agent))
                                  :key #'(lambda (cxn) (attr-val cxn :form)) :test #'string=))
           (solution (compose-new-program agent answer learning-strategy))
           (chunk (solution->chunk solution :initial-score (get-configuration agent :initial-chunk-score)))
           (equivalent-chunk (find-equivalent-chunk agent chunk))
           (interaction-nr (interaction-number (current-interaction (experiment agent)))))
      (notify composition-solution-found solution)
      ;; add the chunk (or re-use an existing one)
      (unless equivalent-chunk
        (add-chunk (ontology agent) chunk))
      ;; remove the previous cxn for this utterance
      (when (and cxn-w-utterance (not (eql learning-strategy :lateral-inhibition)))
        (remove-holophrase-cxn agent cxn-w-utterance))
      ;; create and add a new holophrase cxn
      (add-holophrase-cxn (grammar agent) (utterance agent)
                          (if equivalent-chunk equivalent-chunk chunk)
                          interaction-nr
                          :initial-score (get-configuration agent :initial-cxn-score)))))

;; #####################
;; + Determine success +
;; #####################
(define-event success-determined (success t) (learner-role symbol))

(defmethod determine-success ((tutor holophrase-agent) (learner holophrase-agent))
  "Determine success of the current interaction"
  (when (applied-cxn learner)
    (let ((success (equal-entity (found-answer tutor)
                                 (found-answer learner))))
      (notify success-determined success (discourse-role learner))
      success)))
