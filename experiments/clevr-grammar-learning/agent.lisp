(in-package :clevr-grammar-learning)

;; -----------------
;; + Agent Classes +
;; -----------------

(defclass clevr-learning-agent (agent object-w-tasks)
  ((meaning :initarg :meaning :initform nil
          :accessor meaning :type (or null entity number)
          :documentation "The meaning representation for the current utterance")
   (role :initarg :role :initform 'no-role :type symbol :accessor role
         :documentation "The role of the agent (tutor or learner)")
   (grammar :initarg :grammar :accessor grammar :initform nil
            :type (or null fcg-construction-set)
            :documentation "The agent's grammar"))
  (:documentation "Base class for both agents"))

(defclass clevr-learning-tutor (clevr-learning-agent)
  ((question-success-table
    :initarg :success-table :initform nil :type list
    :accessor question-success-table
    :documentation "Agent keeps track of which questions
                               were seen, and how often it was
                               successful or not")
   (current-question-index :initform -1 :accessor current-question-index
                           :type number :documentation "Index of current question"))
   (:documentation "The tutor agent"))

(defclass clevr-learning-learner (clevr-learning-agent)
  ((task-result :initarg :task-result
                :accessor task-result
                :initform nil
                :documentation "Pointer to the result of the task in this interaction"))
  (:documentation "The learner agent"))

(defun default-clevr-grammar ()
  (let ((clevr-grammar (copy-object *CLEVR*)))
    (set-configurations clevr-grammar
                        '((:cxn-supplier-mode . :ordered-by-label-hashed)
                          (:priority-mode . :nr-of-applied-cxns)
                          (:parse-order hashed nom cxn)
                          (:production-order hashed-lex nom cxn hashed-morph)
                          (:max-nr-of-nodes . 10000))
                        :replace t)
    (set-configurations (processing-cxn-inventory clevr-grammar)
                        '((:cxn-supplier-mode . :ordered-by-label-hashed)
                          (:priority-mode . :nr-of-applied-cxns)
                          (:parse-order hashed nom cxn)
                          (:production-order hashed-lex nom cxn hashed-morph)
                          (:max-nr-of-nodes . 10000))
                        :replace t)
    clevr-grammar))

(defun make-clevr-learning-tutor (experiment)
  (make-instance 'clevr-learning-tutor
                 :role 'tutor :experiment experiment
                 :grammar (default-clevr-grammar)
                 :success-table (loop for i below (length (question-data experiment))
                                      collect (cons i nil))))

(defun make-clevr-learning-learner (experiment)
  (let ((learner
         (make-instance 'clevr-learning-learner
                        :role 'learner :experiment experiment
                        :grammar (empty-cxn-set (get-configuration experiment :hide-type-hierarchy)
                                                (get-configuration experiment :learner-cxn-supplier)))))
    learner))

(defmethod clear-question-success-table ((agent clevr-learning-tutor))
  (setf (question-success-table agent)
        (loop for i below (length (question-data (experiment agent)))
              collect (cons i nil))))

