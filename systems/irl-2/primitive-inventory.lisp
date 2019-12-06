
(in-package :irl-2)

;; ############################################################################
;; primitive inventory definition:
;; ----------------------------------------------------------------------------

(defclass primitive-inventory ()
  ((name :type symbol :initform (gensym "PRIMITIVES")
         :initarg :name :accessor name
         :documentation "The name of the construction inventory. Typically,
                         this is the name that you use in the
                         def-irl-primitives macro")
   (configuration :type configuration :initform (make-config)
                  :initarg :configuration :accessor configuration
                  :documentation "Determines the behavior of the
                  primitive inventory in processing")
   (primitives :type list :initform nil
               :initarg :primitives
               :documentation "The list of primitives")
   (ontology :type blackboard :initform (make-blackboard)
             :initarg :ontology :accessor ontology
             :documentation "The ontology used during processing"))
  (:documentation "The primitive inventory organises the primitives
       and determines the behaviour during processing"))


(defmethod initialize-instance :after ((primitive-inventory primitive-inventory)
                                       &key &allow-other-keys)
  "Initializing the primitive inventory. Here, we set the configurations
   to default values"
  (set-configuration primitive-inventory :check-node-fn :no-duplicate-solutions)
  (set-configuration primitive-inventory :check-irl-program-fn :check-irl-program)
  (set-configuration primitive-inventory :choose-next-primitive-fn :random)
  (set-configuration primitive-inventory :evaluate-bind-statements-fn :evaluate-bind-statements))


(defgeneric primitives (primitive-inventory &key &allow-other-keys)
  (:documentation "Return the list of primitives stored in the primitive-inventory"))

(defgeneric (setf primitives) (primitive-list primitive-inventory &key &allow-other-keys)
  (:documentation "Sets the primitives of a primitive inventory"))

(defgeneric size (primitive-inventory &key &allow-other-keys)
  (:documentation "Return the size of the primitive inventory"))

(defgeneric add-primitive (primitive primitive-inventory &key &allow-other-keys)
  (:documentation "Adds a primitive to a primitive inventory"))

(defgeneric delete-primitive (primitive primitive-inventory &key test key)
  (:documentation "Deletes a primitive from the primitive inventory.
       Returns the deleted primitive or nil when it could not be found."))

(defgeneric find-primitive (primitive primitive-inventory &key test key)
  (:documentation "Finds a primitive in the primitive inventory"))

(defmethod copy-object-content ((source primitive-inventory)
                                (target primitive-inventory))
  t)

(defmethod set-configuration ((primitive-inventory primitive-inventory)
                              key value &key (replace t))
  (set-configuration (configuration primitive-inventory)
                     key value :replace replace))

(defmethod get-configuration ((primitive-inventory primitive-inventory) key)
  (get-configuration (configuration primitive-inventory) key))

(defmethod print-object ((primitive-inventory primitive-inventory) stream)
  (format stream "<~(~a~): ~a primitives>" 
          (class-name (class-of primitive-inventory))
          (size primitive-inventory)))

(defun eval-when-bound (sexp)
  "evaluates sexp, and if it is an unbound atom, doesn't evaluate it"
  (if (and (atom sexp) (not (boundp sexp)))
    sexp
    (eval sexp)))

(defun find-key-arg (arguments key)
  "find key in arguments and return its value"
  (loop for (arg . remaining-args) on arguments
        when (eq arg key)
        return (first remaining-args)))

(defun remove-key-args (arguments)
  (loop for (arg . remaining-args) on arguments by #'cddr
        unless (keywordp arg)
        return (cons arg remaining-args)))

(defun check-def-irl-primitives-keys (keys-and-defs)
  (let ((accepted-keys '(:irl-configurations :visualization-configurations
                         :primitive-inventory :hashed :primitive-inventory-type
                         :ontology)))
    (dolist (x keys-and-defs)
      (when (keywordp x)
        (unless (member x accepted-keys)
          (error "Unknown keyword ~a. Accepted keywords are: ~a" x accepted-keys))))))

;;;; TO DO:
;; add visualization-configurations?
;; add different primitive inventory types?
;; add hashed primitive inventories?

(defmacro def-irl-primitives (name &body keys-and-defs)
  "Create an IRL primitive inventory, setting all configurations"
  (check-def-irl-primitives-keys keys-and-defs)
  (let* ((name (eval-when-bound name))
         (creator-fn-name (internal-symb 'make- name '-primitives))
         (primitive-inventory (or (find-key-arg keys-and-defs :primitive-inventory)
                                  '*irl-primitives*))
         (ontology (find-key-arg keys-and-defs :ontology)))
    `(progn
       (with-disabled-monitor-notifications
         (defun ,creator-fn-name ()
           (setf ,primitive-inventory
                 (make-instance 'primitive-inventory
                                :name ',name
                                :ontology ,(or ontology (make-blackboard))))
           ,@(loop for configuration in (find-key-arg keys-and-defs :irl-configurations)
                   collect `(set-configuration ,primitive-inventory
                                               ,(first configuration)
                                               ',(cdr configuration)))
           (let ((*irl-primitives* ,primitive-inventory))
             ,@(remove-key-args keys-and-defs)
             *irl-primitives*))
         (,creator-fn-name))
       ,primitive-inventory)))
           
                                
  