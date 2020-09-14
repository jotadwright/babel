(in-package :clevr-learning)

;; #########
;; + Agent +
;; #########

(defclass holophrase-agent (agent)
  ((clevr-question :documentation "The clevr-question object"
                   :accessor clevr-question :initarg :clevr-question
                   :initform nil :type (or null clevr-question))
   (role :documentation "The role of the agent"
         :accessor role :initarg :role
         :initform 'agent :type symbol)
   (grammar :documentation "The agent's grammar"
            :accessor grammar :initarg :grammar
            :initform nil :type fcg-construction-set)
   (ontology :documentation "The agent's ontology"
             :accessor ontology :initarg :ontology
             :initform nil :type blackboard)
   (primitives :documentation "The available primitive operations"
               :accessor primitives :initarg :primitives
               :initform nil :type primitive-inventory)
   (ground-truth-answer :documentation "The answer to the question"
                        :accessor ground-truth-answer
                        :initarg :ground-truth-answer
                        :initform nil
                        :type (or null category number))
   (computed-answer :documentation "The answer found by executing the program"
                    :accessor computed-answer :initarg :computed-answer
                    :initform nil :type (or null category number)))
  (:documentation "The VQA Agent"))

(defclass holophrase-tutor (holophrase-agent) ()
  (:documentation "The tutor agent"))

(defclass holophrase-learner (holophrase-agent)
  ((applicable-chunks :documentation "The chunks that can be used for production"
                      :accessor applicable-chunks
                      :initarg :applicable-chunks
                      :initform nil :type list)
   (applied-cxn :documentation "The cxn used in the current interaction"
                :accessor applied-cxn :initarg :applied-cxn
                :initform nil :type (or null fcg-construction))
   (applied-chunk :documentation "The chunk used in the current interaction"
                  :accessor applied-chunk :initarg :applied-chunk
                  :initform nil :type (or null chunk))
   (trash :documentation "The failed programs, used in learning strategy :keep-trash"
          :accessor trash :initarg :trash
          :initform nil :type list)
   (samples :documentation "The seen samples, used in learning strategy :keep-samples"
            :accessor samples :initarg :samples
            :initform nil :type list)
   (composer :documentation "The learner's chunk composer"
             :accessor composer :initarg :composer
             :initform nil :type (or null chunk-composer)))
  (:documentation "The learner agent"))
   

(defun learnerp (agent)
  (eql (type-of agent) 'holophrase-learner))

(defun tutorp (agent)
  (eql (type-of agent) 'holophrase-tutor))


;; #####################
;; + Conceptualisation +
;; #####################

(defun get-possible-primitives (answer)
  "get all the primitives that might return the given answer"
  (mapcar #'(lambda (s) (intern s :clevr-primitives))
          (mapcar #'mkstr
                  (etypecase answer
                    (number '(count!))
                    (boolean-category '(exist equal? less-than greater-than equal-integer))
                    (attribute '(query))))))

(defun get-final-primitive (chunk)
  "Get the final primitive from the chunk"
  (first
   (find (get-target-var (irl-program chunk))
         (irl-program chunk)
         :test #'member)))

(define-event conceptualisation-started (answer t) (possible-chunks list))
(define-event conceptualisation-finished (applicable-chunks list))

(defmethod conceptualise ((agent holophrase-learner))
  "The agent looks for all chunks that lead to the given answer
   in the current scene. These are stored in 'applicable-chunks'."
  (let* ((possible-final-primitives
          (get-possible-primitives
           (ground-truth-answer agent)))
         (all-chunks
          (find-data (ontology agent) 'programs))
         (consider-chunks
          (remove-if-not #'(lambda (chunk)
                             (member (get-final-primitive chunk)
                                     possible-final-primitives))
                         all-chunks)))
    (notify conceptualisation-started
            (ground-truth-answer agent)
            consider-chunks)
    (when consider-chunks
      (setf (applicable-chunks agent)
            (loop for chunk in (shuffle consider-chunks)
                  for solution
                  = (with-disabled-monitors
                      (first
                       (evaluate-irl-program
                        (irl-program chunk)
                        (ontology agent)
                        :primitive-inventory
                        (primitives agent))))
                  for answer
                  = (when solution
                      (get-target-value (irl-program chunk)
                                        solution))
                  when (equal-entity answer (ground-truth-answer agent))
                  collect chunk))))
  (notify conceptualisation-finished
          (applicable-chunks agent))
  (applicable-chunks agent))

;; ##############
;; + Production +
;; ##############

(define-event production-finished (applied-cxn t))

(defmethod produce-question ((agent holophrase-learner))
  "For each of the applicable chunks, the learer gets
   all cxns that have this chunk as meaning. The learner
   chooses the cxn-chunk combination that has the highest
   score. If multiple cxn-chunk combos have the same score,
   a random one is chosen."
  (when (and (applicable-chunks agent)
             (constructions (grammar agent)))
    (let* ((candidates
            (loop for chunk in (applicable-chunks agent)
                  for cxns = (find-all (id chunk) (constructions (grammar agent))
                                       :key #'(lambda (cxn) (attr-val cxn :meaning)))
                  append (mapcar #'(lambda (cxn)
                                     (list cxn chunk
                                           (* (attr-val cxn :score)
                                              (score chunk))))
                                 cxns)))
           (best
            (the-biggest #'(lambda (tuple) (third tuple)) candidates)))
      (when best
        (setf (applied-cxn agent) (first best)
              (applied-chunk agent) (second best)
              (utterance agent)
              (attr-val (first best) :form)))))
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

(defmethod tutor-interprets ((agent holophrase-tutor))
  "The tutor gets the utterance from the learner, parses it and
   executes the result in the scene. The answers of both tutor
   and learner are compared."
  (let* ((irl-program
          (comprehend (utterance agent)
                      :cxn-inventory (grammar agent)))
         (solutions
          (with-disabled-monitors
            (evaluate-irl-program irl-program (ontology agent)
                                  :primitive-inventory (primitives agent))))
         solution)
    (when (and solutions (= (length solutions) 1))
      (setf solution (first solutions))
      (setf (computed-answer agent)
            (get-target-value irl-program solution)))
    (computed-answer agent)))

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

(defmethod parse-question ((agent holophrase-learner))
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

(defmethod interpret ((agent holophrase-learner))
  "Interpret the meaning in the context"
  (let* ((chunk (get-chunk agent (attr-val (applied-cxn agent) :meaning)))
         (solutions (evaluate-irl-program (irl-program chunk) (ontology agent)
                                          :primitive-inventory (primitives agent))))
    (when (length= solutions 1)
      (let* ((solution (first solutions))
             (answer (get-target-value (irl-program chunk) solution)))
        (setf (applied-chunk agent) chunk
              (computed-answer agent) answer)))
    (notify interpretation-finished (computed-answer agent))
    (computed-answer agent)))

;; ############
;; + Adoption +
;; ############

(define-event adoption-started)
(define-event added-to-trash (irl-program list))
(define-event composition-solution-found (solution chunk-evaluation-result))

(defmethod add-trash ((agent holophrase-agent))
  ;; trash strategy: add program to trash, so the composer can take this into account
  (let ((cxn-w-utterance
         (find (utterance agent) (constructions (grammar agent))
               :key #'(lambda (cxn) (attr-val cxn :form))
               :test #'string=)))
    (when cxn-w-utterance
      (let ((found (assoc (utterance agent) (trash agent) :test #'string=))
            (irl-program (irl-program (get-chunk agent (attr-val cxn-w-utterance :meaning)))))
        (notify added-to-trash irl-program)
        (if found
          (push irl-program (cdr found))
          (push (cons (utterance agent) (list irl-program)) (trash agent)))))))

(defmethod adopt ((agent holophrase-agent) ground-truth-answer)
  "Compose a program that results in the given answer
   and store this in the grammar of the agent."
  (let ((learning-strategy (get-configuration agent :learning-strategy)))
    (notify adoption-started)
    ;; use the learning strategy
    (when (eql learning-strategy :keep-trash)
      (add-trash agent))
    ;; compose a new solution
    (let* ((cxn-w-utterance
            (find (utterance agent) (constructions (grammar agent))
                  :key #'(lambda (cxn) (attr-val cxn :form))
                  :test #'string=))
           (solution
            (compose-new-program agent ground-truth-answer
                                 learning-strategy)))
      (if (not (eql solution 'timeout-passed))
        (let* ((chunk
                (solution->chunk agent solution
                                 :initial-score (get-configuration agent :initial-chunk-score)))
               (interaction-nr
                (interaction-number
                 (current-interaction
                  (experiment agent)))))
          (notify composition-solution-found solution)
          ;; add the chunk
          (add-chunk (ontology agent) chunk)
          ;; remove the previous cxn for this utterance
          ;; if it existed
          (when (and cxn-w-utterance (not (eql learning-strategy :lateral-inhibition)))
            (remove-holophrase-cxn agent cxn-w-utterance))
          ;; create and add a new holophrase cxn
          (add-holophrase-cxn (grammar agent) (utterance agent)
                              chunk interaction-nr
                              :initial-score (get-configuration agent :initial-cxn-score))
          ;; remove unreachable chunks
          (remove-unreachable-chunks agent)
          ;; add a larger chunk to the composer-chunks
          (add-composer-chunk (ontology agent)
                              (chunk (irl::node solution))))
        (set-data agent :timeout t)))))

;; #####################
;; + Determine success +
;; #####################
(define-event success-determined (success t)
  (speaker holophrase-agent) (hearer holophrase-agent))

(defmethod determine-success ((speaker holophrase-agent)
                              (hearer holophrase-agent))
  (let ((success (equal-entity (ground-truth-answer speaker)
                               (computed-answer hearer))))
    (notify success-determined success speaker hearer)
    success))
