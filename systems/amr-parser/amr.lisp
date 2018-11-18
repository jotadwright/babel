(in-package :amr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Contents                                              ;;
;;                                                       ;;
;; 1. Class definitions for representing AMR objects     ;;
;; 2. From AMR PENMAN notation to AMR objects            ;;
;; 3. From AMR objects to predicate networks             ;;
;; 4. From predicate networks to AMR objects             ;;
;; 5. From AMR objects to AMR PENMAN notation            ;;
;; 6. Higher-Level functions                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. Class definitions for representing AMR  objects ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass amr-object ()
  ((instance
    :type symbol :initarg :instance :initform nil :accessor amr-instance
    :documentation "The variable that represents an instance of an AMR concept.")
   (concept
    :type symbol :initarg :concept :initform nil :accessor amr-concept
    :documentation "An AMR concept.")
   (arguments
    :type list :initarg :arguments :initform nil :accessor amr-arguments
    :documentation "The arguments of the AMR object."))
   (:documentation "Class for representing Abstract Meaning Representations (AMR)."))

(defclass amr-argument ()
  ((type
    :type symbol :initarg :type :initform nil :accessor amr-type
    :documentation "The type of the argument (e.g. :ARG0).")
   (value
    :type (or symbol amr-object) :initarg :value :initform nil :accessor amr-value
    :documentation "The value of the argument: either a symbol or an AMR object."))
   (:documentation "Class for representing an argument of an AMR object"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. From AMR formatted meanings to AMR objects   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun penman->object (amr-formatted-meaning)
  "Takes an Penman formatted meaning and returns an AMR object."
  (make-instance 'amr-object
                 :instance (instance amr-formatted-meaning)
                 :concept (concept amr-formatted-meaning)
                 :arguments (loop for argument in (arguments amr-formatted-meaning)
                                  for argument-type = (first argument)
                                  for argument-value = (second argument)
                                  collect (if (listp argument-value)
                                            (make-instance 'amr-argument
                                                           :type argument-type
                                                           :value (penman->object (second argument)))
                                            (make-instance 'amr-argument
                                                           :type argument-type
                                                           :value argument-value)))))

(defun instance (amr-formatted-meaning)
  "Returns the variable that represents an instance of an AMR concept."
  (first amr-formatted-meaning))

(defun concept (amr-formatted-meaning)
  "Returns the AMR concept."
  (third amr-formatted-meaning))

(defun arguments (amr-formatted-meaning)
  "Returns the arguments of the AMR in a ((arg-type-1 value-1) (arg-type-2 value-2)) format."
  (let ((argument-value-pairs nil)
        (current-a-v-pair nil))
    (dolist (element (cdddr amr-formatted-meaning))
      (if current-a-v-pair
        (progn
          (push `(,current-a-v-pair ,element) argument-value-pairs)
          (setf current-a-v-pair nil))
        (setf current-a-v-pair element)))
    (reverse argument-value-pairs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. From AMR objects to predicate networks       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod object->predicates ((amr-object amr-object))
  "Return the predicate represention for an AMR object."
  (multiple-value-bind (instances relations) (object->instances-relations amr-object)
    (append instances relations)))

(defmethod object->instances-relations ((amr-object amr-object))
  "Return the predicate represention for an AMR object."  
  (let* (;; For the current object
         (concept (amr-concept amr-object))
         (instance (amr-instance amr-object))
         (instance-predicate `((,concept ,instance)))
         (relation-predicates (loop for arg in (amr-arguments amr-object)
                                   for arg-type = (amr-type arg)
                                   for amr-value = (amr-value arg)
                                   for arg-value-instance = (if (equal (type-of amr-value) 'amr-object)
                                                              (amr-instance amr-value)
                                                              amr-value)
                                   collect `(,arg-type ,instance ,arg-value-instance)))
         ;; For the arguments
         (other-instances (loop for arg in (amr-arguments amr-object)
                                for arg-value = (amr-value arg)
                                when (equal (type-of arg-value) 'amr-object)
                                append (object->instances-relations arg-value)))
         (other-relation-predicates (loop for arg in (amr-arguments amr-object)
                                          for arg-value = (amr-value arg)
                                          when (equal (type-of arg-value) 'amr-object)
                                          append (second (multiple-value-list (object->instances-relations arg-value))))))
    (values (append instance-predicate other-instances) (append relation-predicates other-relation-predicates))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4. From predicate networks to AMR objects       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun predicates->object (predicate-network &optional top-level-instance introduced-instances)
  "Transforms an AMR meaning in predicate notation to an AMR object."
  (let* ((instance-predicates (instances predicate-network))
         (top-level-instance (or top-level-instance (top-level-instance predicate-network)))
         (introduced-instances (or introduced-instances (list top-level-instance)))
         (top-level-relations (top-level-relations predicate-network top-level-instance))
         (top-level-arguments (loop
                               for amr-arguments = nil then amr-arguments
                               for relation in top-level-relations
                               collect (make-instance 'amr-argument
                                                      :type (first relation)
                                                      :value (let ((instance (find (third relation)
                                                                                   (set-difference instance-predicates
                                                                                                   introduced-instances)
                                                                                   :key #'second)))
                                                               (if instance
                                                                 (progn
                                                                   (push instance introduced-instances)
                                                                   (predicates->object
                                                                    predicate-network instance introduced-instances))
                                                                    (third relation)))))))
    (make-instance 'amr-object
                   :concept (first top-level-instance)
                   :instance (second top-level-instance)
                   :arguments top-level-arguments)))

;; (predicates->object '((WANT-01 W) (BOY B) (GO-01 G) (:ARG0 W B) (:ARG1 W G) (:ARG0 G B)))

(defun top-level-instance (predicate-network)
  "Returns the top-level instance of a predicate network."
  (let* ((instance-predicates (instances predicate-network))
         (relation-predicates (relations predicate-network)))
    (loop for instance in instance-predicates
          unless (find (second instance) relation-predicates :key #'third)
          return instance)))

;; (top-level-instance '((WANT-01 W) (BOY B) (GO-01 G) (:ARG0 W B) (:ARG1 W G) (:ARG0 G B)))

(defun top-level-relations (predicate-network instance)
  "Returns the top-level instance of a predicate network."
  (let* ((relation-predicates (relations predicate-network)))
    (loop for relation in relation-predicates
          when (eql (second instance) (second relation))
          collect relation)))

;; (top-level-relations '((WANT-01 W) (BOY B) (GO-01 G) (:ARG0 W B) (:ARG1 W G) (:ARG0 G B)) '(WANT-01 W))

(defun instances (predicate-network)
  "Returns the instance predicates of a predicate network."
  (remove-if #'(lambda (predicate) (keywordp (first predicate))) predicate-network))

;; (instances '((WANT-01 W) (BOY B) (GO-01 G) (:ARG0 W B) (:ARG1 W G) (:ARG0 G B)))

(defun relations (predicate-network)
  "Returns the relation predicates of a predicate network."
  (remove-if-not #'(lambda (predicate) (keywordp (first predicate))) predicate-network))

;; (relations '((WANT-01 W) (BOY B) (GO-01 G) (:ARG0 W B) (:ARG1 W G) (:ARG0 G B)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5. From AMR objects to AMR formatted meanings  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod object->penman ((amr-object amr-object))
  "Returns a Penman formatted meaning for an AMR object."
  (let ((instance (amr-instance amr-object))
        (concept (amr-concept amr-object))
        (arguments (loop for arg in (amr-arguments amr-object)
                         append `(,(amr-type arg) ,(object->penman (amr-value arg))))))
    `(,instance / ,concept ,@arguments)))

(defmethod object->penman ((amr-object t))
  amr-object)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6. Higher-level functions                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun penman->predicates (amr-formatted-meaning)
  "Transforms a Penman formatted meaning into a predicate network."
  (object->predicates (penman->object amr-formatted-meaning)))

(defun predicates->penman (predicate-network)
  "Transforms a predicate network into a Penman formatted meaning."
  (object->penman (predicates->object predicate-network)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testing                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| Testing:

(defparameter *penman-in* '(w / want-01
                              :arg0 (b / boy)
                              :arg1 (g / go-01
                                       :arg0 b)))

(setf *penman-in* '(s / say-01
                      :ARG0 (g / organization
                               :name (n / name
                                        :op1 "UN"))
                      :ARG1 (f / flee-01
                               :ARG0 (p / person
                                        :quant (a / about
                                                  :op1 14000))
                               :ARG1 (h / home
                                        :poss p)
                               :time (w / weekend)
                               :time (a2 / after
                                         :op1 (w2 / warn-01
                                                  :ARG1 (t / tsunami)
                                                  :location (l / local))))
                      :medium (s2 / site
                                  :poss g
                                  :mod (w3 / web))))

(progn
  (setf *object-in* (penman->object *penman-in*))
  (setf *predicates* (object->predicates *object-in*))
  (setf *object-out* (predicates->object *predicates*))
  (setf *penman-out* (object->penman *object-out*))
  (equal *penman-in* *penman-out*)

  (setf *predicates* (penman->predicates *penman-in*))
  (setf *penman-out* (predicates->penman *predicates*))
  (equal *penman-in* *penman-out*))

|#