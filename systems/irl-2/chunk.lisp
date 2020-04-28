(in-package :irl-2)

;; ############################################################################
;; chunk
;; ----------------------------------------------------------------------------

(export '(create-chunks-from-primitives
          create-chunk-from-primitive
          create-chunk-from-irl-program))

(defclass chunk ()
  ((id :initarg :id :initform (make-id 'chunk) :accessor id :type symbol)
   (irl-program 
    :initarg :irl-program :initform nil :accessor irl-program :type list
    :documentation "the s-expression representation of the irl program")
   (target-var 
    :initarg :target-var :accessor target-var :type list
    :documentation "A (?variable . type) cons for the target for the chunk"
    :initform nil)
   (open-vars
    :initarg :open-vars :accessor open-vars :type list :initform nil
    :documentation "A list of (?variable . type) conses for the open variables")
   (score :initarg :score :accessor score :type single-float :initform 1.0
          :documentation "A score for the chunk"))
  (:documentation "A chunk is an irl program with an explicit target
                   and open variables"))

(defmethod initialize-instance :around ((chunk chunk) &rest initargs &key id irl-program)
  "when :id was not passed, make a new one based on the irl program of the chunk"
  (apply #'call-next-method chunk :id (or id (irl-program->id irl-program))
         :irl-program irl-program initargs))

(defun create-chunks-from-primitives (things &key (score 0.5) (primitive-inventory *irl-primitives*))
  "Creates chunks from things. This can be a list of primitives or a list
   of primitive ids (i.e. symbols)"
  (assert (listp things))
  (loop for thing in things
        collect (create-chunk-from-primitive thing :score score :primitive-inventory primitive-inventory)))

(defgeneric create-chunk-from-primitive (thing &key score
                                               primitive-inventory
                                               chunk-id target-var-is-open-var)
  (:documentation "Create a chunk from thing. This can be a symbol denoting
   the primitive's name or the primitive object itself. This is done by
   making the variable of the first slot the target variable and all other
   variables open variables"))

(defmethod create-chunk-from-primitive ((primitive-id symbol)
                                        &key (score 0.5)
                                        (primitive-inventory *irl-primitives*)
                                        chunk-id target-var-is-open-var)
  (let* ((primitive (find-primitive primitive-id primitive-inventory))
         (vars (loop for slot-spec in (slot-specs primitive)
                     collect (cons (make-var (slot-spec-name slot-spec))
                                   (slot-spec-type slot-spec))))
         (irl-program (list (cons primitive-id (mapcar #'car vars)))))
    (make-instance 'chunk :id (or chunk-id primitive-id)
                   :irl-program irl-program :score score
                   :target-var (car vars)
                   :open-vars (if target-var-is-open-var vars (cdr vars)))))

(defmethod create-chunk-from-primitive ((primitive primitive)
                                        &key (score 0.5)
                                        (primitive-inventory *irl-primitives*)
                                        chunk-id target-var-is-open-var)
  (declare (ignorable primitive-inventory))
  (let* ((vars (loop for slot-spec in (slot-specs primitive)
                     collect (cons (make-var (slot-spec-name slot-spec))
                                   (slot-spec-type slot-spec))))
         (irl-program (list (cons (id primitive) (mapcar #'car vars)))))
    (make-instance 'chunk :id (or chunk-id (id primitive))
                   :irl-program irl-program :score score
                   :target-var (car vars)
                   :open-vars (if target-var-is-open-var vars (cdr vars)))))

(defun create-chunk-from-irl-program (irl-program
                                      &key (id (irl-program->id irl-program))
                                      (score 0.5) target-var
                                      (primitive-inventory *irl-primitives*))
  (let* ((found-target-var (get-target-var irl-program))
         (target-var (if target-var
                       (if (eql target-var found-target-var)
                         found-target-var
                         (error "specified target-var not available"))
                       (if found-target-var
                         found-target-var
                         (error "zero or more than one possible target var found in irl-program"))))
         (target-var-type (get-type-of-var target-var irl-program
                                           :primitive-inventory primitive-inventory))
         (open-v (get-open-vars irl-program))
         (open-vars
          (sort open-v #'<
                :key #'(lambda (x)
                         (position x (find x irl-program :test #'member)))))
         (open-var-types (loop for open-var in open-vars
                               collect (get-type-of-var open-var irl-program
                                                        :primitive-inventory primitive-inventory))))
    (make-instance 'chunk :id id :irl-program irl-program
                   :target-var (cons target-var target-var-type)
                   :open-vars (loop for open-var in open-vars
                                    for open-var-type in open-var-types
                                    collect (cons open-var open-var-type))
                   :score score)))

(defun irl-program->id (irl-program)
  "creates a symbol representing the primitives used"
  (let* ((bind-statements (all-bind-statements irl-program))
         (bind-id-parts
          (mapcar #'fourth bind-statements))
         (predicates (set-difference irl-program bind-statements))
         (primitives-id-parts
          (loop with primitive-ids = (mapcar #'car predicates)
                with unique-primitive-ids =
                (stable-sort
                 (copy-list
                  (remove-duplicates primitive-ids)) #'string<)
                for id in unique-primitive-ids
                for id-count = (count id primitive-ids)
                if (> id-count 1)
                collect (mkstr id-count "x-" id)
                else
                collect id)))
    (symb
     (string-append
      (format nil "~{~a~^--~}" bind-id-parts)
      (if (and (length> bind-statements 1)
               (length> predicates 1))
        "/" "")
      (format nil "~{~a~^--~}" primitives-id-parts)))))

